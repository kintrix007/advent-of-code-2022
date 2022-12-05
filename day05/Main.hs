module Main (main) where

import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.Maybe ( fromJust, isJust )

data Move = Move { from :: Int , to :: Int, times :: Int } deriving (Show)

main :: IO ()
main = do
    cont <- readFile "input"
    let (stacks, moves) =  parse cont
    putStr "Part 1: "
    putStrLn $ part1 (stacks, moves)
    putStr "Part 2: "
    putStrLn $ part2 (stacks, moves)

part1 :: ([[Char]], [Move]) -> String
part1 (stacks, moves) =
    "[None]"

part2 :: ([[Char]], [Move]) -> String
part2 (stacks, moves) =
  "[None]"

parse :: String -> ([[Char]], [Move])
parse cont =
    (parseStacks stacksStr, parseMoves movesStr)
    where packed = T.strip . T.pack $ cont
          [stacksStr, movesStr] = T.splitOn (T.pack "\n\n") packed

parseMoves :: T.Text -> [Move]
parseMoves packed =
    map parseMove $ T.splitOn (T.pack "\n") packed

parseMove :: T.Text -> Move
parseMove text =
    Move from to times
    where meaningfulPart = T.dropWhile (not . isDigit) text
          timesStr:rest:_ = T.splitOn (T.pack "from") meaningfulPart
          [fromStr, toStr] = T.splitOn (T.pack "to") rest
          [from, to, times] = map ((read :: String -> Int) . T.unpack) [fromStr, toStr, timesStr]

parseStacks :: T.Text -> [[Char]]
parseStacks text =
    internalParseStacks (tail lines) startStacks
    where lines = reverse $ T.splitOn (T.pack "\n") text
          slots = ((length . T.unpack . head $ lines) + 1) `div` 4
          startStacks = map (const []) [1..slots]

internalParseStacks :: [T.Text] -> [[Char]] -> [[Char]]
internalParseStacks [] stacks = stacks 
internalParseStacks lines stacks =
    internalParseStacks rest newStacks
    where line:rest = lines
          chunks = T.chunksOf 4 line
          parsed = parseBoxes chunks
          boxes = parseBoxes chunks
          newStacks = [ if isJust box then fromJust box : stack else stack
                      | (stack, box) <- zip stacks boxes]

parseBoxes :: [T.Text] -> [Maybe Char]
parseBoxes = map parseBox
    where getBoxContent str = str `T.index` 1
          parseBox str = case getBoxContent str of
                            ' ' -> Nothing
                            x -> Just x
