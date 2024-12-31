{-# LANGUAGE GHC2024 #-}

module Main where
import System.Environment (getArgs)
import Data.List (find)

data Op = Add | Mul
  deriving (Eq, Ord, Read, Show)

data OpTree = Cons Op OpTree Int | First Int
  deriving (Eq, Ord, Read, Show)

empty :: Num a => Op -> a
empty Add = 0
empty Mul = 1

parseEqn :: String -> (Int, [Int])
parseEqn text =
    let
        result = takeWhile (/= ':') text
        inputs = words $ drop (length result + 1) text
     in
        (read result, read <$> inputs)

pairs :: [b] -> [(b, b)]
pairs lst = zip lst (drop 1 lst)

applyOps :: OpTree -> [Int] -> [OpTree]
applyOps z [] = [z]
applyOps z (x:xs) = do
    op <- [Add, Mul]
    let t = Cons op z x
    applyOps t xs

applyOps1 :: [Int] -> [OpTree]
applyOps1 [] = undefined
applyOps1 (x:xs) = applyOps (First x) xs

evalOps :: OpTree -> Int
evalOps (First x) = x
evalOps (Cons op tree y) = case op of
    Add -> evalOps tree + y
    Mul -> evalOps tree * y

getTestOps :: (Int, [Int]) -> Int
getTestOps (result, inputs) =
    let
        valid = any (== result) $ evalOps <$> applyOps1 inputs
    in
        if valid then result else 0

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
    let eqns = parseEqn <$> lines input
    let summed = sum $ getTestOps <$> eqns

    putStrLn $ "total calibration result: " ++ (show summed)

    where
        findInputFile args =
            let
                pair = find (\(f, _) -> f == "-f" || f == "--file") $ pairs args
            in
                fmap snd pair
