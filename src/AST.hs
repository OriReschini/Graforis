module AST where

type Name = String

data Graph = Empty
           | Vertex Name
           | Overlay Graph Graph
           | Connect Graph Graph
           | Var Name

instance Show Graph where
  show Empty = "empty"
  show (Vertex n) = n
  show (Overlay g1 g2) = "( " ++ show g1 ++ " + " ++ show g2 ++ " )"
  show (Connect g1 g2) = "( " ++ show g1 ++ " * " ++ show g2 ++ " )"
  show (Var n) = n

data Comm = DefVar Name Graph
          | Draw Graph
          | Apply Name Graph Name Name
          | Seq Comm Comm

instance Show Comm where
  show (DefVar n g)        = n ++ " := " ++ show g
  show (Draw g)            = "draw " ++ show g
  show (Apply p g ori des) = "apply " ++ show p ++ " to " ++ show g
                           ++ " from vertex " ++ ori ++ " to vertex " ++ des
  show (Seq c1 c2)         = show c1 ++ "\n" ++ show c2

data Position = Position Int Int -- Position line column

instance Show Position where
  show (Position line column) = "close to line " ++ show line ++ " column " ++ show column
