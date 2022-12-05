module Main (main) where

import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.Maybe ( fromJust, isJust )

type Stack = [Char]
data Move = Move { from :: Int , to :: Int, times :: Int } deriving (Show)

main :: IO ()
main = do
    cont <- readFile "input"
    let (stacks, moves) =  parse cont
    print $ length stacks
    putStr "Part 1: "
    putStrLn $ part1 (stacks, moves)
    putStr "Part 2: "
    putStrLn $ part2 (stacks, moves)

part1 :: ([Stack], [Move]) -> String
part1 (stacks, moves) =
    map (\(x:_) -> x) finalStacks
    where finalStacks = foldl (\acc x -> applyMove x acc) stacks moves

applyMove :: Move -> [Stack] -> [Stack]
applyMove (Move _ _ 0) stacks         = stacks
applyMove (Move from to times) stacks =
    applyMove (Move from to $ times-1) afterPush
    where (afterPop, popped) = popFromIdx from stacks
          afterPush = pushToIdx to popped afterPop

part2 :: ([Stack], [Move]) -> String
part2 (stacks, moves) =
    "[None]"

popFromIdx :: Int -> [Stack] -> ([Stack], Char)
popFromIdx idx xs =
    (bef ++ rest : aft, x)
    where (bef, (x:rest):aft) = splitAt idx xs

pushToIdx :: Int -> Char -> [Stack] -> [Stack]
pushToIdx idx x xs =
  bef ++ (x:rest) : aft
  where (bef, rest:aft) = splitAt idx xs

parse :: String -> ([Stack], [Move])
parse cont = (stacks, moves)
    where packed = T.pack cont
          [stacksStr, movesStr] = T.splitOn (T.pack "\n\n") packed
          stacks = parseStacks stacksStr
          moves = parseMoves $ T.strip movesStr

parseMoves :: T.Text -> [Move]
parseMoves packed =
    map parseMove $ T.splitOn (T.pack "\n") packed

parseMove :: T.Text -> Move
parseMove text =
    Move (from-1) (to-1) times
    where meaningfulPart = T.dropWhile (not . isDigit) text
          timesStr:rest:_ = T.splitOn (T.pack "from") meaningfulPart
          [fromStr, toStr] = T.splitOn (T.pack "to") rest
          [from, to, times] = map ((read :: String -> Int) . T.unpack) [fromStr, toStr, timesStr]

parseStacks :: T.Text -> [Stack]
parseStacks text =
    internalParseStacks lines startStacks
    where lines = reverse . map (`T.append` T.pack " ") . init . T.splitOn (T.pack "\n") $ text
          slots = (length . T.unpack . head $ lines) `div` 4
          startStacks = map (const []) [0..slots]

internalParseStacks :: [T.Text] -> [Stack] -> [Stack]
internalParseStacks [] stacks = stacks 
internalParseStacks lines stacks =
    internalParseStacks rest newStacks
    where line:rest = lines
          boxes = parseBoxes $ T.chunksOf 4 line
          newStacks = [ if isJust box then fromJust box : stack else stack
                      | (stack, box) <- zip stacks boxes]

parseBoxes :: [T.Text] -> [Maybe Char]
parseBoxes = map parseBox
    where getBoxContent str = str `T.index` 1
          parseBox str = case getBoxContent str of
                            ' ' -> Nothing
                            x -> Just x
