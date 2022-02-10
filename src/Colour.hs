module Colour (colour) where

import State
import AST
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz                     (graphToDot, GraphvizParams(..), NodeCluster(..), GraphID(..), Number(Int))
import Data.GraphViz.Attributes.Complete (Attribute(Label, Color), toColorList, Color(RGB), ColorList) 
import Data.GraphViz.Attributes          (toLabel, Labellable)
import Data.GraphViz.Printing            (renderDot, toDot)
import Data.Text.Lazy                    (unpack)
import Data.Graph.Inductive.Graph        (mkGraph, LEdge, labNodes, labEdges, neighbors, nodes, Node)
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

-- hasLoop l returns true if there is a loop in the list l of edges. If not, it returns false
hasLoop :: [LEdge ()] -> Bool
hasLoop [] = False
hasLoop ( (ori, des, ()):es ) 
    | ori == des = True
    | otherwise  = hasLoop es 

-- given a graph, sortGraphNodes returns a list of tuples where the first component of each element is a Node (represented by an Int)
-- and the second component is a list of Nodes that are connected to the Node in the first component. Also, the list of tuples is 
-- ordered by the list's length in the second component in descending order.
sortGraphNodes :: Gr String () -> [(Node, [Node])]
-- negate (length neighbours) is used so that it is sorted in a descending order
sortGraphNodes g = sortOn (\(node, neighbours) -> negate(length neighbours)) nodesWithNeighbours
                    where nodesWithNeighbours = map (\i -> (i, neighbors g i)) (nodes g)

availableColours :: [ColorList]
-- colours: red, green, blue, light blue, fuchsia, orange, purple, yellow, light pink, dark green, dark blue, brown
availableColours = [toColorList [RGB 255 0 0], toColorList [RGB 0 255 0], toColorList [RGB 0 0 255], toColorList [RGB 0 255 255],
                    toColorList [RGB 255 0 255], toColorList [RGB 255 128 0], toColorList [RGB 127 0 255], toColorList [RGB 255 255 0], 
                    toColorList [RGB 255 190 190], toColorList [RGB 0 102 0], toColorList [RGB 0 0 102], toColorList [RGB 102 0 0]]

-- returns a list of tuples (n, colour) where n is a node and colour represents the colour assigned
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
assignColours g = mkGraph newNodes (labEdges g)
                    where sortedNodes = sortGraphNodes g  -- sortedNodes :: [(Node, [Node])] 
                          nodesWithColours = sort $ realAssignment sortedNodes [] 0 -- nodesWithColours :: [(Node, ColorList)]
                          nodesWithLabels = sort $ labNodes g
                          newNodes = zipWith (\(n1, lab) (n2, colour) -> (n1, (lab, Color $ colour)) ) nodesWithLabels nodesWithColours

-- drawWithCol g n colours g, creates a file called n and draws the graph in that file
drawWithCol :: MonadTrans t => Gr String () -> Name -> t IO ()
drawWithCol g name = do
    let graphColoured = assignColours g 
    let dot = unpack $ renderDot $ toDot $ graphToDot params graphColoured
    lift (createFile dot name)

-- given a graph g and a name n, this function colours g and creates a file called n with the coloured graph 
-- if the graph is not colourable, it returns an error saying that it's not possible to colour g 
colour :: (MonadTrans t, MonadError (t IO)) => Gr String () -> Name -> t IO ()
colour g name = 
    do 
        let edges = labEdges g 
        if hasLoop edges then throwUncolourableG name else drawWithCol g name
    