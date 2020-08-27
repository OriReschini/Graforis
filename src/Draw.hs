module Draw(draw) where

import State
import AST 
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz                     (graphToDot, GraphvizParams(..), isDirected, globalAttributes, clusterBy,
                                         isDotCluster, NodeCluster(..), clusterID, GraphID(..), Number(Int), fmtCluster, fmtNode, fmtEdge)
import Data.GraphViz.Attributes.Complete (Label(StrLabel), Attribute(Label)) 
import Data.GraphViz.Printing            (renderDot, toDot, dot)
import Data.Text.Lazy                    (unpack, pack)
import System.Process                    (callCommand, system)
import System.Directory                  (removeFile)

myDefaultParams :: GraphvizParams n nl el () nl
myDefaultParams = Params { isDirected       = False
                       , globalAttributes = []
                       , clusterBy        = N
                       , isDotCluster     = const True
                       , clusterID        = const (Num $ Int 0)
                       , fmtCluster       = const []
                       , fmtNode          = const []
                       , fmtEdge          = const []
                       }

labelledNodesParams = myDefaultParams { fmtNode= \(_,label)-> [Label (StrLabel (pack label))] }  

createFile :: String -> Name -> IO ()
createFile dot name = do
    writeFile (name++".dot") dot
    callCommand ("dot -Tpng -o"++name++".png "++name++".dot")


draw :: MonadTrans t => Name -> Gr String () -> t IO ()
draw name g = do
    let dot = unpack $ renderDot $ toDot $ graphToDot labelledNodesParams g
    lift (createFile dot name)
    --lift (removeFile (name++".dot"))

