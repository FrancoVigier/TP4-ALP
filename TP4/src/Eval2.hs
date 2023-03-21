module Eval2
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

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of 
                                Right (v :!: s') -> runStateError (f v) s'
                                Left e           -> Left e)

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v  = StateError (\s -> if M.member v s then Right (((M.!) s v) :!: s) else Left UndefVar)
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
           Right (x :!: env) -> Right env
           Left e            -> Left e

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm     Skip                = return Skip
stepComm    (Let v e)            = do { x <- evalExp e; update v x; return Skip }
stepComm    (Seq Skip c2)        = return c2
stepComm    (Seq c1 c2)          = do { c1' <- stepComm c1; return (Seq c1' c2) }
stepComm    (IfThenElse e c1 c2) = do { x <- evalExp e; if x then return c1 else return c2 }
stepComm c1@(While e c2)         = do { x <- evalExp e; if x then return (Seq c2 c1) else return Skip }

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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

