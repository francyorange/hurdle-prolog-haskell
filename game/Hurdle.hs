module Main where

import System.IO (hFlush, stdout)
import Data.List (delete)
import Data.Time.Clock.POSIX (getPOSIXTime)
import WordList (wordList)

type HurdleWord = String

-- === Logic ===
countExact :: String -> String -> ([(Char, Char)], Int)
countExact [] [] = ([], 0)
countExact (g:gs) (s:ss)
  | g == s    = let (rest, c) = countExact gs ss in ((g, '_'):rest, c + 1)
  | otherwise = let (rest, c) = countExact gs ss in ((g, s):rest, c)

removeExact :: [(Char, Char)] -> (String, String)
removeExact = foldr f ("", "")
  where
    f (g, '_') (gs, ss) = (gs, ss)
    f (g, s)   (gs, ss) = (g:gs, s:ss)

countInexact :: String -> String -> Int
countInexact [] _ = 0
countInexact (g:gs) s
  | g `elem` s = 1 + countInexact gs (delete g s)
  | otherwise  = countInexact gs s

guessFeedback :: String -> HurdleWord -> (Int, Int)
guessFeedback g s =
  let (paired, exact) = countExact g s
      (gLeft, sLeft) = removeExact paired
      inexact = countInexact gLeft sLeft
  in (exact, inexact)

-- === Pseudo-random word picker using time ===
pickTimeSeedWord :: IO HurdleWord
pickTimeSeedWord = do
  t <- getPOSIXTime
  let i = floor t `mod` length wordList
  return (wordList !! i)

-- === Main ===
main :: IO ()
main = do
  putStrLn "Welcome to Hurdle! You have 8 guesses to find the 5-letter word."
  solution <- pickTimeSeedWord
  loop 1 solution
  where
    maxTries = 8
    loop n answer
      | n > maxTries = putStrLn $ "Out of guesses! The correct word was: " ++ answer
      | otherwise = do
          putStr $ "Guess " ++ show n ++ ": "
          hFlush stdout
          guess <- getLine
          if length guess /= 5
            then putStrLn "Please enter a 5-letter word." >> loop n answer
            else if guess `notElem` wordList
              then putStrLn "Not a valid word!" >> loop n answer
              else do
                let (e, i) = guessFeedback guess answer
                putStrLn $ "Exact: " ++ show e ++ ", Inexact: " ++ show i
                if e == 5
                  then putStrLn $ "You win in " ++ show n ++ " guesses!"
                  else loop (n + 1) answer
