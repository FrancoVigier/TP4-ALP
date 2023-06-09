module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm     Skip                = return Skip
stepComm    (Let v e)            = do { x <- evalExp e; update v x; return Skip }
stepComm    (Seq Skip c2)        = return c2
stepComm    (Seq c1 c2)          = do { c1' <- stepComm c1; return (Seq c1' c2) }
stepComm    (IfThenElse e c1 c2) = do { x <- evalExp e; if x then return c1 else return c2 }
stepComm c1@(While e c2)         = do { x <- evalExp e; if x then return (Seq c2 c1) else return Skip }

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Int
evalExp (Const i)     = return i
evalExp (Var v)       = lookfor v
evalExp (UMinus e1)   = do { x <- evalExp e1; return (-x) }
evalExp (Plus e1 e2)  = do { x <- evalExp e1; y <- evalExp e2; return (x + y) }
evalExp (Minus e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x - y) }
evalExp (Times e1 e2) = do { x <- evalExp e1; y <- evalExp e2; return (x * y) }
evalExp (Div e1 e2)   = do { x <- evalExp e1; y <- evalExp e2; return (x `div` y) }

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



