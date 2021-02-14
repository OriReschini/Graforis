module Colour (colour) where

import State
import AST
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz                     (graphToDot, GraphvizParams(..), NodeCluster(..), GraphID(..), Number(Int))
import Data.GraphViz.Attributes.Complete (Attribute(Label, Color), toColorList, Color(RGB), ColorList) 
import Data.GraphViz.Attributes          (toLabel, Labellable)
import Data.GraphViz.Printing            (renderDot, toDot)
import Data.Text.Lazy                    (unpack)
import Data.Graph.Inductive.Graph        (mkGraph, LNode, LEdge, labNodes, labEdges, neighbors, nodes, Node)
import Data.List                         (sortOn, sort)
import Draw
import Lib

params :: GraphvizParams n (String,Attribute) el () (String,Attribute)
params = Params { isDirected       = False
                       , globalAttributes = []
                       , clusterBy        = N
                       , isDotCluster     = const True
                       , clusterID        = const (Num $ Int 0)
                       , fmtCluster       = const []
                       , fmtNode          = nodes
                       , fmtEdge          = const []
                       }
                    where nodes (num, (label, colour)) = [toLabel label, colour]

--colorsParams = params { fmtNode = \(a, (label, colour)) -> color a label colour}
--colorsParams = params { fmtNode = \(a, label) -> color a label (Color $ toColorList [RGB 40 255 40]) }

color a label colour | a == 2 = [toLabel label, colour]
                     | otherwise = [toLabel label, colour]
--labelledNodesParams = myDefaultParams { fmtNode= \(_,label)-> [Label (StrLabel (pack label)), Color $ toColorList [RGB 40 255 40] ] }  

-- hasLoop l returns true if there is a loop in the list l of edges. If not, it returns false
hasLoop :: [LEdge ()] -> Bool
hasLoop [] = False
hasLoop ( (ori, des, ()):es ) 
    | ori == des = True
    | otherwise  = hasLoop es 

-- given a graph, sortGraphNodes returns a list ordered by the second component in descending order with tuples
-- where first component is Int representing the node and second component is a list of nodes(their ints) that 
-- are connected to the first component
--sortGraphNodes :: [LNode String] -> [LEdge ()] -> [(Int, [Int])] -> [(Int, [Int])]
sortGraphNodes :: Gr String () -> [(Node, [Node])]
-- negate (length neighbours) is used so that it is sorted in a descending order
sortGraphNodes g = sortOn (\(node, neighbours) -> negate(length neighbours)) nodesWithNeighbours
                    where nodesWithNeighbours = map (\i -> (i, neighbors g i)) (nodes g)

availableColours :: [ColorList]
-- orden colores: rojo, verde, azul, celeste, fucsia, naranja, violeta, amarillo, azul no tan oscuro, verde oscuro, azul oscuro, marron
availableColours = [toColorList [RGB 255 0 0], toColorList [RGB 0 255 0], toColorList [RGB 0 0 255], toColorList [RGB 0 255 255],
                    toColorList [RGB 255 0 255], toColorList [RGB 255 128 0], toColorList [RGB 127 0 255], toColorList [RGB 255 255 0], 
                    toColorList [RGB 0 128 255], toColorList [RGB 0 102 0], toColorList [RGB 0 0 102], toColorList [RGB 102 0 0]]

-- retrurns a list of tuples (n, i) where n is a node and i is the int that represents the colour assigned
realAssignment :: [(Node, [Node])] -> [(Node, ColorList)] -> Int -> [(Node, ColorList)]
realAssignment [] res _ = res
realAssignment sortedNodes res k = realAssignment [n | n <- sortedNodes, (fst n, availableColours !! k) `notElem` newRes] newRes (k+1)
                                    where colourNodesWith _ [] res = res
                                          -- if node has a neighbour with colour k
                                          colourNodesWith k ((node, neigh) : ns) res = 
                                              if any (\n -> (n, availableColours !! k) `elem` res) neigh
                                                then colourNodesWith k ns res
                                                else colourNodesWith k ns ((node, availableColours !! k) : res)
                                          newRes = colourNodesWith k sortedNodes res

-- assignColours g returns a coloured graph
assignColours :: Gr String () -> Gr (String, Attribute) ()
--assignColours g = mkGraph newNodes edges
-- para definir newnodes llamo a realassignment y ahi tengo que ver como genero los colores y despues asigno
    
--    assignColoursAux sortedNodes [] 1
                    --where sortedNodes = sortGraphNodes g 
                          --assignColoursAux [] res _ = res
                          --assignColoursAux sn res k = let newRes = colourNodesWith k sn res
                                                        --in assignColoursAux [n | n <- sn, (fst n, k) `notElem` newRes] newRes (k+1)
                          --colourNodesWith _ [] res = res
                          -- if node has a neighbour with colour k
                          --colourNodesWith k ((node, neigh) : ns) res = if any (\n -> (n, k) `elem` res) neigh
                                                                        --then colourNodesWith k ns res
                                                                        --else colourNodesWith k ns ((node, k) : res)
--assignColours g = mkGraph newNodes edges
--                    where edges = labEdges g
--                          newNodes = map (\(a,lab)->(a,(lab,Color $ (availableColours !! 5))) ) (labNodes g)
assignColours g = mkGraph newNodes (labEdges g)
                    where sortedNodes = sortGraphNodes g  -- sortedNodes :: [(Node, [Node])] 
                          nodesWithColours = sort $ realAssignment sortedNodes [] 0 -- nodesWithColours :: [(Node, ColorList)]
                          nodesWithLabels = sort $ labNodes g
                          newNodes = zipWith (\(n1, lab) (n2, colour) -> (n1, (lab, Color $ colour)) ) nodesWithLabels nodesWithColours
-- tengo nodesWithColours y (labNodes g), tengo que combinar para conseguir las labels de (labNodes g)
--                        newNodes = map ()


drawWithCol :: MonadTrans t => Gr String () -> Name -> t IO ()
drawWithCol g name = do
    let graphColoured = assignColours g 
    let dot = unpack $ renderDot $ toDot $ graphToDot params graphColoured
    lift (createFile dot name)

-- VER CASO DE SI ES GRAFO VACÍO (0 NODOS, 0 ARISTAS)

-- dado un grafo y un string que representa el nombre
colour :: (MonadTrans t, MonadError (t IO)) => Gr String () -> Name -> t IO ()
colour g name = --drawWithCol g name
    do 
        let edges = labEdges g 
        if hasLoop edges then throwUncolourableG name else drawWithCol g name
    
    --do
    --let dot = unpack $ renderDot $ toDot $ graphToDot colorsParams g
    --lift (createFile dot name)
    --lift (removeFile (name++".dot"))