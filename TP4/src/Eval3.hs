module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
  StateErrorTrace { runStateErrorTrace :: (Env, Trace) -> Either Error ((Pair a Env), Trace) }

instance Monad StateErrorTrace where
  return x = StateErrorTrace (\(s, t) -> Right ((x :!: s), t))
  m >>= f  = StateErrorTrace (\(s, t) -> case runStateErrorTrace m (s, t) of 
                                Right ((v :!: s'), t') -> runStateErrorTrace (f v) (s', t')
                                Left e                 -> Left e)

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return  
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  trace t = StateErrorTrace (\(s, t') -> Right (() :!: s, t' ++ t))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\_ -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v  = StateErrorTrace (\(s, t) -> if M.member v s then Right (((M.!) s v) :!: s, t) else Left UndefVar)
  update v i = StateErrorTrace (\(s, t) -> Right (() :!: update' v i s, t ++ (v ++ "=" ++ show i ++ ";"))) where update' = M.insert


-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval p = case runStateErrorTrace (stepCommStar p) (initEnv, "") of
           Right ((_:!: env), t) -> Right (env, t)
           Left e                -> Left e

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm     Skip                = return Skip
stepComm    (Let v e)            = do { x <- evalExp e; update v x; return Skip }
stepComm    (Seq Skip c2)        = return c2
stepComm    (Seq c1 c2)          = do { c1' <- stepComm c1; return (Seq c1' c2) }
stepComm    (IfThenElse e c1 c2) = do { x <- evalExp e; if x then return c1 else return c2 }
stepComm c1@(While e c2)         = do { x <- evalExp e; if x then return (Seq c2 c1) else return Skip }

-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
-- Int
evalExp (Const i)     = return i
evalExp (Var v)       = lookfor v
evalExp (UMinus e1)   = do { x <- evalExp e1; return (-x) }
evalExp (Plus e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x + y) }
evalExp (Minus e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x - y) }
evalExp (Times e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x * y) }
evalExp (Div e1 e2)   = do { x <- evalExp e1; y <- evalExp e2; if y == 0 then return (x `div` y) else throw DivByZero }

-- Bool
evalExp (BTrue)     = return True
evalExp (BFalse)    = return False
evalExp (Lt e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x < y) }
evalExp (Gt e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x > y) }
evalExp (And e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x && y) }
evalExp (Or e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x || y) }
evalExp (Not e1)    = do { x <- evalExp e1; return (not x) }
evalExp (Eq e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x == y) }
evalExp (NEq e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x /= y) }
-- evalExp (NEq e1 e2)  = do { x <- evalExp (Not (Eq (e1 e2)); return x }

-- Otras
evalExp (EAssgn v e) = do { x <- evalExp e; update v x; return x } 
evalExp (ESeq e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return y }
