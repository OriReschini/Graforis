module Eval (eval) where

import AST
import State
import Draw                                (draw)
import Colour                              (colour)
import Data.Graph.Inductive.Graph          (mkGraph, empty, noNodes, labNodes, labEdges, LNode, LEdge)
import Data.Graph.Inductive.PatriciaTree   (Gr)


eval :: Comm -> IO (Either Error ((), Env))
eval p = runGraphStateT (evalComm p) initState

evalComm :: (MonadState (t IO), MonadError (t IO), MonadTrans t) => Comm -> t IO ()
evalComm (DefVar n g) = (evalGraph g 1) >>= (\g' -> save n g')
evalComm (Draw g) = (evalGraph g 1) >>= (\g' -> draw g' (nameFile g))
evalComm (Colour g) = (evalGraph g 1) >>= (\g' -> colour g' (nameFile g))
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
        (nodes,edges) = uniqueNodes (labNodes g1'++labNodes g2') (newEdges++labEdges g1'++labEdges g2') 
    return (mkGraph nodes edges)
evalGraph (Var n) i         = do 
    g <- lookfor n 
    let nodes = adjustNodes (labNodes g) i 
        edges = adjustEdges (labEdges g) i 
    return (mkGraph nodes edges)
   
-- replaceE es i k replaces in the list of edges es all appearences of i with k and makes sure that
-- all edges' first node is smaller than the second node
replaceE :: [LEdge ()] -> Int -> Int -> [LEdge ()]
replaceE [] _ _ = []
replaceE ((ori, des, ()):es) old new 
          | ori == old && des == old = ( (new, new, ()) : (replaceE es old new) )
          -- origin has to be smaller than destination
          | ori == old = if new < des then ( (new, des, ()) : (replaceE es old new) ) else ( (des, new, ()) : (replaceE es old new) )
          | des == old = if ori < new then ( (ori, new, ()) : (replaceE es old new) ) else ( (new, ori, ()) : (replaceE es old new) )
          | otherwise = ( (ori, des, ()) : (replaceE es old new) )

-- replaceN ns i k replaces in the list of nodes ns all nodes with int i with k
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

-- has ns n returns true if n is in ns. If not, it returns false.
has :: [LNode String] -> String -> Bool
has [] _ = False
has ((i,name):ns) s | s == name = True
                    | otherwise = has ns s 

-- uniqueNodes ns es returns a tuple (n, e) where all nodes with the same name have the same value (int).
uniqueNodes :: [LNode String] -> [LEdge ()] -> ([LNode String], [LEdge ()])
uniqueNodes [] edges = ([], edges)
uniqueNodes ((i,name):ns) edges 
              | has ns name = let idx = idxName name ns
                                  newNodes = replaceN ns idx i 
                                  newEdges = replaceE edges idx i
                                  (nodes',edges') = uniqueNodes newNodes newEdges
                                in ( (i,name):nodes', edges' )
              | otherwise = let (nodes, edges') = uniqueNodes ns edges 
                              in ( ((i,name):nodes), edges' )

-- connect l1 l2 e returns a list of edges (n1, n2), where n1 is in l1 and n2 is in l2
-- edges acumulate in e
connect :: [LNode String] -> [LNode String] -> [LEdge ()] -> [LEdge ()]
connect [] _ edges = edges
connect _ [] edges = edges
connect ((i1,n1):ns1) l2@((i2,n2):ns2) edges = 
  let edges1 = connect [(i1,n1)] ns2 edges 
      edges2 = connect ns1 l2 edges1
      e = (i1,i2,()) :: LEdge ()
    in if i1 > i2 then ( (i2,i1,()) : edges2 ) else ( (i1,i2,()) : edges2 )

-- adjustNodes l i returns a list of nodes where i was added to each node
adjustNodes :: [LNode String] -> Int -> [LNode String]
adjustNodes [] _ = []
adjustNodes ( (idx,name):ns ) i = ( (idx+i,name) : adjustNodes ns i )

-- adjustEdges l i returns a list of edges (e1, e2) where (e1-i, e2-i) belongs to l
-- basically this function adds i to every node in every edge
adjustEdges :: [LEdge ()] -> Int -> [LEdge ()]
adjustEdges [] _ = []
adjustEdges ( (src,dst,()):es ) i = ( (src+i,dst+i,()) : adjustEdges es i )

-- nameFile g returns a string that will be the file's name that will be created
-- if g is not a variable, the file will be called "output"
nameFile :: Graph -> String
nameFile (Var n) = tail n -- i use tail n because n starts with a '#'
nameFile g = "output"