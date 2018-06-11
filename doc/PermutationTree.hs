-- WARNING: THIS CODE IS FLAWED

import Data.Set (Set, fromList, toList, delete)
import Control.Monad.State

data Hint = G | B | W
  deriving (Eq, Ord, Show)

newtype Piece = Piece (Int,Char)
  deriving (Eq, Ord)

instance Show Piece where
  show (Piece (i,c)) = show i ++ "," ++ [c]

main :: IO ()
main = do
  writeFile "/tmp/tree1.dot" $ tree "c1" "xyz" (1,1)
  writeFile "/tmp/tree2.dot" $ tree "c2" "xyy" (2,1)

tree :: String -> String -> (Int,Int) -> String
tree name pieces' (good,bad) =
  let
    pieces'' = map Piece $ zip [0..] pieces'
    hints''  = take good (repeat G)
               ++ take bad (repeat B)
               ++ take (length pieces' - good - bad) (repeat W)
  in
    tree' name pieces'' hints''

tree' :: String -> [Piece] -> [Hint] -> String
tree' name pieces' hints' =
  let
    result = hints 0 (fromList pieces') (fromList hints')
  in
    graph name $ node 0 "root" ++ evalState result 0

hints :: Int -> Set Piece -> Set Hint -> State Int String
hints pid pieces' hints' = concat <$> mapM f (toList hints')
  where
    f h = do
      id <- newId
      let label = show h
          node' = node id label
          edge' = edge pid id
      sub <- pieces id pieces' (delete h hints')
      return $ node' ++ edge' ++ sub

pieces :: Int -> Set Piece -> Set Hint -> State Int String
pieces pid pieces' hints' = concat <$> mapM f (toList pieces')
  where
    f p = do
      id <- newId
      let label = show p
          node' = node id label
          edge' = edge pid id
      sub <- hints id (delete p pieces') hints'
      return $ node' ++ edge' ++ sub

newId :: State Int Int
newId = modify (+1) >> get

graph :: String -> String -> String
graph name content = "digraph " ++ name ++ " {\n" ++ content ++ "}\n"
  
node :: Int -> String -> String
node id label = 
  "  " ++ nodeId id ++ " [label=\"" ++ label ++ "\"];\n"

edge :: Int -> Int -> String
edge id1 id2 =
  "  " ++ nodeId id1 ++ " -> " ++ nodeId id2 ++ ";\n"

nodeId :: Int -> String
nodeId x = map g (show x)
  where
    g c = toEnum $ fromEnum c - fromEnum '0' + fromEnum 'a'
  
