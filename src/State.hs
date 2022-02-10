module State where

import AST
import Control.Monad                       (liftM, ap)
import Data.Graph.Inductive.PatriciaTree   (Gr)

type Env = [(Name, Gr String ())]

initState :: Env
initState = []

data Error = UndefinedGraph Name
           | UncolourableGraph Name

instance Show Error where
  show (UndefinedGraph n)    = "Graph " ++ show n ++ " was not defined."
  show (UncolourableGraph n) = "Graph " ++ show n ++ " is not colourable."

newtype GraphStateT m a = GraphStateT { runGraphStateT :: Env -> m (Either Error (a,Env)) } 

-- given a string n and an environment e, returns True if n is a graph in e
isVarDef :: Name -> Env -> Bool
isVarDef _ [] = False
isVarDef n ((i,g):e) | i == n = True
                     | otherwise = isVarDef n e


instance (Monad m) => Monad (GraphStateT m) where
    return x              = GraphStateT $ \s -> return $ Right (x,s)
    (GraphStateT f) >>= g = GraphStateT $ \s -> do 
        r1 <- f s 
        case r1 of 
            Left error -> return $ Left error
            Right (a,s') -> runGraphStateT (g a) s'

instance (Monad m) => Functor (GraphStateT m) where
    fmap = liftM

instance (Monad m) => Applicative (GraphStateT m) where
    pure   = return
    (<*>)  = ap

class (Monad m) => MonadError m where 
    throwUndefinedG :: Name -> m a
    throwUncolourableG :: Name -> m a

instance (Monad m) => MonadError (GraphStateT m) where
    throwUndefinedG n    = GraphStateT $ \s -> return $ Left (UndefinedGraph n)
    throwUncolourableG n = GraphStateT $ \s -> return $ Left (UncolourableGraph n)

class (Monad m) => MonadState m where
    getEnv :: m Env 
    lookfor :: Name -> m (Gr String ())
    save :: Name -> Gr String () -> m ()

instance (Monad m) => MonadState (GraphStateT m) where
    getEnv = GraphStateT $ \s -> return $ Right (s,s)
    lookfor n = do e <- getEnv
                   if isVarDef n e then GraphStateT $ \s -> return $ Right (lookfor' n e, s)
                                   else throwUndefinedG n 
                        where lookfor' n ((i,g):e) | i == n    = g
                                                   | otherwise = lookfor' n e
    save n g = GraphStateT $ \s -> return $ Right ((), save' n g s)
                    where save' n g [] = [(n, g)]
                          save' n g ((u, _):ss) | n == u = (n, g):ss
                          save' n g ((u, j):ss) | n /= u = (u, j):(save' n g ss)
    
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a 

instance MonadTrans GraphStateT where
    lift o = GraphStateT $ \s -> o >>= (\r -> return $ Right (r,s)) 