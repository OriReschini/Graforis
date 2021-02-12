module Draw (draw) where

import State
import AST 
import Lib
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz                     (graphToDot, GraphvizParams(..), NodeCluster(..), GraphID(..), Number(Int))
import Data.GraphViz.Attributes.Complete (Attribute(Label, Color), toColorList, Color(RGB)) 
import Data.GraphViz.Attributes          (toLabel, Labellable)
import Data.GraphViz.Printing            (renderDot, toDot)
import Data.Text.Lazy                    (unpack)

myDefaultParams :: (Labellable nl) => GraphvizParams n nl el () nl
myDefaultParams = Params { isDirected       = False
                       , globalAttributes = []
                       , clusterBy        = N
                       , isDotCluster     = const True
                       , clusterID        = const (Num $ Int 0)
                       , fmtCluster       = const []
                       , fmtNode          = nodes
                       , fmtEdge          = const []
                       }
                    where nodes (num,label) = [toLabel label]

--colorsParams = myDefaultParams { fmtNode = \(a, label) -> color a label}

--color a label | a == 2 = [toLabel label, Color $ (toColorList [RGB 200 2 2])]
--              | otherwise = [toLabel label, Color $ (toColorList [RGB 1 220 21])]
--labelledNodesParams = myDefaultParams { fmtNode= \(_,label)-> [Label (StrLabel (pack label)), Color $ toColorList [RGB 40 255 40] ] }  


draw :: MonadTrans t => Gr String () -> Name -> t IO ()
draw g name = do
    let dot = unpack $ renderDot $ toDot $ graphToDot myDefaultParams g
    lift (createFile dot name)

