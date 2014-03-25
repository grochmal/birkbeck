module Main (main) where

import Data.Char
import Data.List
import qualified Data.Map as M
import System.Environment
import System.Random

--try :: [Char]
--try = "gyog"

colours :: [Char]
colours = "roygbv"

prinput :: [Char] -> [Char]
prinput = filter (`elem` colours) . map toLower

getk :: Eq a => [a] -> [a] -> [Char]
getk t i = take (length $ filter (\(f,s) -> f == s) $ zip t i) $ repeat 'K'

getw :: Eq a => [a] -> [a] -> [Char]
getw t i = let ks = filter (\(f,s) -> f /= s) $ zip t i
               t' = map fst ks
               i' = map snd ks
           in  take (length $ filter (`elem` t') i') $ repeat 'W'

genkw :: [Char] -> [Char] -> [Char]
genkw c i = (getk c i) ++ (getw c i)

showTry :: [Char] -> [Char]
showTry = foldl (++) "" . map (++" ") . map (:[])

data Result a = NeedInput | Incorrect a | Correct a

goodInput :: Eq a => [a] -> [a] -> Result [a]
goodInput try i
    | try == i      = Correct i
    | length i == 4 = Incorrect i
    | otherwise     = NeedInput

gameChoice :: [Char] -> [[Char]] -> Result [Char] -> IO ()
gameChoice try p NeedInput     = do
    putStrLn "Guess the colors using the letters: roygbv"
    i <- fmap prinput getLine
    gameChoice try p $ goodInput try i
gameChoice try p (Incorrect i) = do
    putStrLn ""
    let p' = (i ++ "  " ++ genkw try i):p
    mapM (putStrLn.showTry) p'
    gameChoice try p' NeedInput
gameChoice try p (Correct i)   = do
    putStrLn ""
    mapM (putStrLn.showTry) $ (i++"  KKKK"):p
    putStrLn "Congratulations, Game Over!"

testInput :: Result [Char] -> IO [Char]
testInput NeedInput     = do
    putStrLn "Please give the code for the game"
    i <- fmap prinput getLine
    testInput $ goodInput "gyog" i
testInput (Incorrect i) = testInput (Correct i)
testInput (Correct i)   = do
    putStr "The code is now "
    putStrLn i
    return i

data Choices = Yes | No | Test

choice :: Choices -> [Integer] -> [Char] -> IO ()
choice No   rs try = do putStrLn "You will come back"
choice Yes  rs try = do gameChoice try [] NeedInput
                        userChoice rs Nothing
choice Test rs try = do try' <- testInput NeedInput
                        gameChoice try' [] NeedInput
                        userChoice rs Nothing

choices :: M.Map [Char] ([Integer] -> [Char] -> IO ())
choices = M.fromList [( "y" , choice Yes  )
                     ,( "n" , choice No   )
                     ,( "t" , choice Test )
                     ]

userChoice :: [Integer] -> Maybe ([Integer] -> [Char] -> IO ()) -> IO ()
userChoice rs Nothing  = do
    putStrLn "So you want to play a game?"
    putStrLn "y -> new game; n -> quit; t -> test game"
    line <- getLine
    userChoice rs $ M.lookup line choices
userChoice rs (Just x) =
    let try = map (colours !!) $ map fromInteger $ take 4 rs
        rs' = drop 4 rs
    in  x rs' try

main :: IO ()
main = do
    gen <- newStdGen
    let rs = randomRs (0,5) gen
    userChoice rs Nothing

