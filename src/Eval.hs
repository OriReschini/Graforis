module Eval (eval) where

import AST
import State
import Draw                                (draw)
import Data.Graph.Inductive.Graph          (mkGraph, empty, noNodes, labNodes, labEdges, LNode, LEdge)
import Data.Graph.Inductive.PatriciaTree   (Gr)


eval :: Comm -> IO (Either Error ((), Env))
eval p = runGraphStateT (evalComm p) initState

--evalComm :: (MonadState m, MonadError m, MonadTrans t) => Comm -> t m ()
evalComm (DefVar n g) = (evalGraph g 1) >>= (\g' -> save n g')
evalComm (Draw g) = (evalGraph g 1) >>= (\g' -> draw "prueba" g')
--evalComm (Apply n g ori des) =
evalComm (Seq c1 c2) = (evalComm c1) >>= (\x -> evalComm c2)


evalGraph :: (MonadState m, MonadError m) => Graph -> Int -> m (Gr String ())
evalGraph Empty _           = return empty -- :: Gr String ()
evalGraph (Vertex n) i      = return (mkGraph [(i, n)] [])
evalGraph (Overlay g1 g2) i = do 
    g1' <- evalGraph g1 i 
    g2' <- evalGraph g2 (i + (noNodes g1')) 
    let (nodes,edges) = uniqueNodes (labNodes g1'++labNodes g2') (labEdges g1'++labEdges g2')
    return (mkGraph nodes edges)
    --return (uniqueNodes (labNodes g1'++labNodes g2') (labEdges g1'++labEdges g2'))
evalGraph (Connect g1 g2) i = do 
    g1' <- evalGraph g1 i 
    g2' <- evalGraph g2 (i + (noNodes g1'))
    let newEdges = connect (labNodes g1') (labNodes g2') []
        (nodes,edges) = (uniqueNodes (labNodes g1'++labNodes g2') (newEdges++labEdges g1'++labEdges g2') )
    return (mkGraph nodes edges)
evalGraph (Var n) i         = lookfor n 
   
replaceE :: [LEdge ()] -> Int -> Int -> [LEdge ()]
replaceE [] _ _ = []
replaceE ((ori, des, ()):es) old new 
          | ori == old && des == old = ( (new, new, ()) : (replaceE es old new) )
          | ori == old = ( (new, des, ()) : (replaceE es old new) )
          | des == old = ( (ori, new, ()) : (replaceE es old new) )
          | otherwise = ( (ori, des, ()) : (replaceE es old new) )

replaceN :: [LNode String] -> Int -> Int -> [LNode String]
replaceN [] _ _ = []
replaceN ((i, name):ns) old new 
          | i == old = ( (new, name) : (replaceN ns old new) )
          | otherwise = ( (i, name) : (replaceN ns old new) )

-- idxName name list returns i, where (i,name) in list
-- it will always be called after knowing that the name is present in the list (check unique)
idxName :: String -> [LNode String] -> Int
idxName s ((i,name):ns) | s == name = i
                        | otherwise = idxName s ns

has :: [LNode String] -> String -> Bool
has [] _ = False
has ((i,name):ns) s | s == name = True
                    | otherwise = has ns s 

-- given a list of nodes and edges, returns a tuple with a list of nodes and edges
-- where nodes with the same name, have the same value (int)
-- originalmente quería que devuela un Gr String (), pero en el caso base:
-- uniqueNodes [] edges si hacía mkGraph [] edges, me devolvía grafo vacío
uniqueNodes :: [LNode String] -> [LEdge ()] -> ([LNode String], [LEdge ()])
uniqueNodes [] edges = ([], edges)
uniqueNodes ((i,name):ns) edges 
              | has ns name = let idx = idxName name ns
                                  newNodes = replaceN ns idx i 
                                  newEdges = replaceE edges idx i
                                in (newNodes, newEdges)
              | otherwise = let (nodes, edges') = uniqueNodes ns edges 
                              in ( ((i,name):nodes), edges')

-- connect l1 l2 returns the list of edges (n1, n2), where n1 in l1, n2 in l2
connect :: [LNode String] -> [LNode String] -> [LEdge ()] -> [LEdge ()]
connect [] _ edges = edges
connect _ [] edges = edges
connect ((i1,n1):ns1) ((i2,n2):ns2) edges = 
  let edges1 = connect [(i1,n1)] ns2 edges
      edges2 = connect ns1 ((i2,n2):ns2) edges1
      e = (i1,i2,()) :: LEdge ()
    in (e : ( edges1 ++ edges2 )) 

