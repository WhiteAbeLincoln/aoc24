{-# LANGUAGE GHC2024 #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Either ()
import Data.List (find, intercalate)
import Data.Set (Set, empty, insert, member)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hReady, stdin)

data Pos = Pos {x :: Int, y :: Int}
    deriving (Eq, Ord, Read, Show)

data Orientation = N | E | S | W
    deriving (Eq, Ord, Read, Show)

data Guard = Guard {pos :: Pos, orientation :: Orientation}
    deriving (Eq, Ord, Read, Show)

data Grid = Grid
    { guard :: Guard
    , obstacles :: Set Pos
    , visited :: Set Pos
    , bounds :: (Int, Int)
    }
    deriving (Eq, Ord)

instance Show Grid where
    show g =
        gridStr
      where
        (width, height) = bounds g
        pointStr p
            | p == pos (guard g) = printGuard (guard g)
            | member p (obstacles g) = "#"
            | member p (visited g) = "X"
            | otherwise = "."
        rowStr y = intercalate "" [pointStr (Pos{x, y}) | x <- [0 .. (width - 1)]]
        gridStr = unlines [rowStr y | y <- [0 .. (height - 1)]]

emptyGridWithBounds :: Int -> Int -> Grid
emptyGridWithBounds width height =
    Grid
        { guard = Guard{pos = Pos{x = 0, y = 0}, orientation = N}
        , obstacles = empty
        , visited = empty
        , bounds = (width, height)
        }

tryReadGrid :: String -> Either (Pos, String) Grid
tryReadGrid input = foldM readGridChar startingGrid rowsWithPos
  where
    rows = filter (/= "") $ lines input
    height = length rows
    width = foldr min (maxBound :: Int) [length r | r <- rows]
    rowsWithPos = do
        (r, y) <- zip rows ([0 ..] :: [Int])
        (c, x) <- zip (take width r) ([0 ..] :: [Int])
        return (c, Pos{x, y})

    startingGrid = emptyGridWithBounds width height

readGridChar :: Grid -> (Char, Pos) -> Either (Pos, String) Grid
readGridChar g (c, p) = case c of
    '^' -> return $ g{guard = Guard{pos = p, orientation = N}, visited = insert p (visited g)}
    '>' -> return $ g{guard = Guard{pos = p, orientation = E}, visited = insert p (visited g)}
    'V' -> return $ g{guard = Guard{pos = p, orientation = S}, visited = insert p (visited g)}
    '<' -> return $ g{guard = Guard{pos = p, orientation = W}, visited = insert p (visited g)}
    '#' -> return $ g{obstacles = insert p (obstacles g)}
    'X' -> return $ g{visited = insert p (visited g)}
    '.' -> return $ g
    _ -> Left (p, "invalid character " ++ [c])

printGuard :: Guard -> String
printGuard Guard{orientation = N} = "^"
printGuard Guard{orientation = E} = ">"
printGuard Guard{orientation = S} = "V"
printGuard Guard{orientation = W} = "<"

rotateRight :: Orientation -> Orientation
rotateRight = \case
    N -> E
    E -> S
    S -> W
    W -> N

-- we orient our grid with 0,0 in the top left
nextPos :: Guard -> Pos
nextPos Guard{pos = Pos{x, y}, orientation = N} = Pos{x, y = y - 1}
nextPos Guard{pos = Pos{x, y}, orientation = E} = Pos{x = x + 1, y}
nextPos Guard{pos = Pos{x, y}, orientation = S} = Pos{x, y = y + 1}
nextPos Guard{pos = Pos{x, y}, orientation = W} = Pos{x = x - 1, y}

moveGuard :: Guard -> Set Pos -> Guard
moveGuard g obstacles =
    let p = nextPos g
     in if member p obstacles
            -- same position, rotate 90 degrees to the guard's right
            then Guard{pos = pos g, orientation = rotateRight (orientation g)}
            -- otherwise move forward one position
            else Guard{pos = p, orientation = orientation g}

outOfBounds :: Pos -> (Int, Int) -> Bool
outOfBounds Pos{x, y} (width, height) = x < 0 || x >= width || y < 0 || y >= height

moveOne :: (Monad m) => StateT Grid m Bool
moveOne = do
    grid <- get
    let g = guard grid
    let ng = moveGuard g $ obstacles grid
    let stop = outOfBounds (pos ng) $ bounds grid
    put $
        ( if stop
            then grid
            else grid{guard = ng, visited = insert (pos ng) $ visited grid}
        )
    return stop

moveAll :: (Monad m) => m Bool -> (Grid -> m a) -> StateT Grid m a
moveAll getContinue doWrite = do
    start <- lift $ getContinue
    if start then executeContinue else executeStop
  where
    executeContinue = do
        stop <- moveOne
        g <- get
        a <- lift $ doWrite g
        case stop of
            False -> moveAll getContinue doWrite
            True -> return a
    executeStop = do
        g <- get
        lift $ doWrite g

-- https://stackoverflow.com/a/38553473
getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

getMoveKey :: IO Bool
getMoveKey = do
    key <- getKey
    return $ not $ elem key ["\ESC", "q"]

moveAllInteractive :: StateT Grid IO ()
moveAllInteractive = moveAll getMoveKey (putStrLn . show)

moveAllImmediate :: (Monad m) => StateT Grid m ()
moveAllImmediate = moveAll (return True) (\_ -> return ())

getVisitedCount :: (Monad m) => StateT Grid m Int
getVisitedCount = do
    g <- get
    return $ length $ visited g

getInputStr :: Maybe String -> Bool -> IO String
getInputStr = \case
    Nothing -> getInputStdin
    Just "-" -> getInputStdin
    Just f -> \_ -> readFile f
  where
    getInputStdin False = getContents
    getInputStdin True = do
        l <- lines <$> getContents
        let leading = takeWhile (not . null) l
        return $ unlines leading

main :: IO ()
main = do
    args <- getArgs
    let file = findInputFile args
    let interactive = (elem "--interactive" args) || (elem "-i" args)
    input <- getInputStr file interactive
    let grid = tryReadGrid input
    let moveFn = if interactive then moveAllInteractive else moveAllImmediate
    case grid of
        Left (pos, err) -> do
            putStrLn $ "error at " ++ (show pos) ++ ": " ++ err
            exitFailure
        Right g -> do
            putStrLn $ show g
            count <- evalStateT (moveFn >> getVisitedCount) g
            putStrLn $ "visited count: " ++ (show count)
            exitSuccess
  where
    findInputFile args =
        let
            pair = find (\(f, _) -> f == "-f" || f == "--file") pairs
            pairs = zip args (drop 1 args)
         in
            fmap snd pair
