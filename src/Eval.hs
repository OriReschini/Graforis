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
   
replaceE :: [LEdge ()] -> Int -> Int -> [LEdge ()]
replaceE [] _ _ = []
replaceE ((ori, des, ()):es) old new 
          | ori == old && des == old = ( (new, new, ()) : (replaceE es old new) )
          -- origin has to be smaller than destination
          | ori == old = if new < des then ( (new, des, ()) : (replaceE es old new) ) else ( (des, new, ()) : (replaceE es old new) )
          | des == old = if ori < new then ( (ori, new, ()) : (replaceE es old new) ) else ( (new, ori, ()) : (replaceE es old new) )
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

-- has ns n devuelve true si n está en ns. En caso contrario, devuelve false
has :: [LNode String] -> String -> Bool
has [] _ = False
has ((i,name):ns) s | s == name = True
                    | otherwise = has ns s 

-- uniqueNodes ns es devuelve una tupla con una lista de nodos y una lista de aristas
-- en las que los nodos con el mismo nombre tienen el mismo valor (int)
-- originalmente quería que devuela un Gr String (), pero en el caso base:
-- uniqueNodes [] edges si hacía mkGraph [] edges, me devolvía grafo vacío
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

-- connect l1 l2 e devuelve la lista de aristas (n1, n2), donde n1 en l1, n2 en l2
-- en e se van acumulando las aristas
connect :: [LNode String] -> [LNode String] -> [LEdge ()] -> [LEdge ()]
connect [] _ edges = edges
connect _ [] edges = edges
connect ((i1,n1):ns1) l2@((i2,n2):ns2) edges = 
  let edges1 = connect [(i1,n1)] ns2 edges 
      edges2 = connect ns1 l2 edges1
      e = (i1,i2,()) :: LEdge ()
    in if i1 > i2 then ( (i2,i1,()) : edges2 ) else ( (i1,i2,()) : edges2 )

-- adjustNodes l i devuelve una lista de nodos en la que a cada nodo se me sumó i
adjustNodes :: [LNode String] -> Int -> [LNode String]
adjustNodes [] _ = []
adjustNodes ( (idx,name):ns ) i = ( (idx+i,name) : adjustNodes ns i )

-- adjustEdges l i devuelve una lista de aristas en la que al origen y destino de cada arista se le sumó i
adjustEdges :: [LEdge ()] -> Int -> [LEdge ()]
adjustEdges [] _ = []
adjustEdges ( (src,dst,()):es ) i = ( (src+i,dst+i,()) : adjustEdges es i )

-- nameFile g devuelve un string que será el nombre del archivo que se creará
-- si g no es una variable, el archivo se llamará "output"
nameFile :: Graph -> String
nameFile (Var n) = tail n -- devuelvo tail n porque n empieza con '#'
nameFile g = "output"