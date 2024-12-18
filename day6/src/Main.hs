{-# LANGUAGE GHC2024 #-}

module Main where

import Control.Monad (foldM)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Either ()
import Data.List (find, intercalate)
import Data.Set (Set, empty, insert, member)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hReady, hSetBuffering, hSetEcho, stdin)

data Pos = Pos {x :: Int, y :: Int}
    deriving (Eq, Ord, Read, Show)

data Orientation = N | E | S | W
    deriving (Eq, Ord, Read, Show)

data Guard = Guard {pos :: Pos, orientation :: Orientation}
    deriving (Eq, Ord, Read, Show)

data StopCondition = Loop | OutOfBounds | Quit | Continue
    deriving (Eq, Ord, Read, Show)

data Grid = Grid
    { guard :: Guard
    , obstacles :: Set Pos
    , visitedCells :: Set Pos
    , visitedObstacles :: Set (Pos, Orientation)
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
            | member p (visitedCells g) = "X"
            | otherwise = "."
        rowStr y = intercalate "" [pointStr (Pos{x, y}) | x <- [0 .. (width - 1)]]
        gridStr = unlines [rowStr y | y <- [0 .. (height - 1)]]

emptyGridWithBounds :: Int -> Int -> Grid
emptyGridWithBounds width height =
    Grid
        { guard = Guard{pos = Pos{x = 0, y = 0}, orientation = N}
        , obstacles = empty
        , visitedCells = empty
        , visitedObstacles = empty
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
    '^' -> return $ g{guard = Guard{pos = p, orientation = N}, visitedCells = insert p (visitedCells g)}
    '>' -> return $ g{guard = Guard{pos = p, orientation = E}, visitedCells = insert p (visitedCells g)}
    'V' -> return $ g{guard = Guard{pos = p, orientation = S}, visitedCells = insert p (visitedCells g)}
    '<' -> return $ g{guard = Guard{pos = p, orientation = W}, visitedCells = insert p (visitedCells g)}
    '#' -> return $ g{obstacles = insert p (obstacles g)}
    'X' -> return $ g{visitedCells = insert p (visitedCells g)}
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

getNextGrid :: Grid -> Either StopCondition Grid
getNextGrid grid
    -- a loop occurs when we hit an obstacle again with the same orientation as previously seen
    | member (p, dir) visitedObs = Left Loop
    -- stop when out of the grid bounds
    | outOfBounds p (bounds grid) = Left OutOfBounds
    -- same position, rotate 90 degrees to the guard's right
    | member p obs = Right grid{guard = g{orientation = rotateRight dir}, visitedObstacles = insert (p, dir) visitedObs}
    -- otherwise move forward one position
    | otherwise = Right grid{guard = g{pos = p}, visitedCells = insert p (visitedCells grid)}
  where
    g = guard grid
    dir = orientation g
    obs = obstacles grid
    visitedObs = visitedObstacles grid
    p = nextPos g

    outOfBounds :: Pos -> (Int, Int) -> Bool
    outOfBounds Pos{x, y} (width, height) = x < 0 || x >= width || y < 0 || y >= height

moveOne :: (Monad m) => StateT Grid m StopCondition
moveOne = do
    grid <- get
    let nextGrid = getNextGrid grid
    put $ either (const grid) id nextGrid
    return $ either id (const Continue) nextGrid

moveAll :: (Monad m) => m Bool -> (Grid -> m a) -> StateT Grid m (StopCondition, a)
moveAll getContinue doWrite = do
    start <- lift $ getContinue
    if start then executeContinue else executeStop
  where
    executeContinue = do
        stop <- moveOne
        g <- get
        a <- lift $ doWrite g
        case stop of
            Continue -> moveAll getContinue doWrite
            cond -> return (cond, a)
    executeStop = do
        g <- get
        a <- lift $ doWrite g
        return (Quit, a)

-- https://stackoverflow.com/a/38553473
getKey :: IO String
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
        char <- getChar
        more <- hReady stdin
        (if more then getKey' else return) (char : chars)

strip :: [Char] -> [Char]
strip = lstrip . rstrip
lstrip :: [Char] -> [Char]
lstrip = dropWhile (`elem` " \t\r\n")
rstrip :: [Char] -> [Char]
rstrip = reverse . lstrip . reverse

getMoveKey :: IO Bool
getMoveKey = do
    key <- getKey
    return $ not $ elem (strip key) ["\ESC", "q"]

moveAllInteractive :: StateT Grid IO (StopCondition, ())
moveAllInteractive = moveAll getMoveKey (putStrLn . show)

moveAllImmediate :: StateT Grid IO (StopCondition, ())
moveAllImmediate = moveAll (return True) (\_ -> return ())

getVisitedCount :: (Monad m) => StateT Grid m Int
getVisitedCount = do
    g <- get
    return $ length $ visitedCells g

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

    -- don't echo keystrokes and don't wait for newlines when reading keys
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    let moveFn = if interactive then moveAllInteractive else moveAllImmediate
    case grid of
        Left (pos, err) -> do
            putStrLn $ "error at " ++ (show pos) ++ ": " ++ err
            exitFailure
        Right g -> do
            putStrLn $ show g
            (stopReason, count) <-
                evalStateT
                    ( do
                        (reason, _) <- moveFn
                        count <- getVisitedCount
                        return (reason, count)
                    )
                    g
            putStrLn $ "stop reason: " ++ (show stopReason)
            putStrLn $ "visited count: " ++ (show count)
            exitSuccess
  where
    findInputFile args =
        let
            pair = find (\(f, _) -> f == "-f" || f == "--file") pairs
            pairs = zip args (drop 1 args)
         in
            fmap snd pair
