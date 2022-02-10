module Lib (createFile) where

import AST
import System.Process           (callCommand)
import System.Directory         (removeFile)        

-- createFile d n draws the graph that d represents (d is a graph in Dot format converted to string)
-- in a file named n.png
createFile :: String -> Name -> IO ()
createFile dot name = do
    writeFile (name++".dot") dot
    callCommand ("dot -Tpng -o"++name++".png "++name++".dot")
    removeFile (name++".dot")