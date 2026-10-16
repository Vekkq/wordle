{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Data.List
import Data.Maybe
import Data.Char (toLower, isAlpha)
import Data.Ord
import Text.Read (readMaybe)
import System.IO

import Control.Exception

import Control.Monad
import Control.Monad.State.Lazy


-- a starting word: raise


main :: IO ()
main = catch (void wordleSolve) errhand

errhand :: IOException -> IO ()
errhand _ = putStrLn "cya!"


type WordleSolver = [(Char, Maybe Int)]

wordleSolve :: MonadIO m => m ()
wordleSolve = evalStateT wordleAsk []

wordleAsk :: (MonadIO m, MonadState WordleSolver m) => m ()
wordleAsk = do
	liftIO $ putStrLn "type your word"
	ss <- liftIO $ getLine
	liftIO $ putStrLn
		$  "\ncodify your word in order"
		++ "\n1. black, 2. yellow, 3. green"
		++ "\ne.g. type the numbers like your word: 23111"
	cs <- liftIO $ getLine
	liftIO $ putStrLn ""
	let ws = zipWith3 (\s c n -> (s, f c n)) ss cs [1..]
	modify $ \s -> ws ++ s
	printSolutions (length ss)
	wordleAsk
	where
		f c n = case c of
			'1' -> Nothing
			'2' -> Just (-n)
			'3' -> Just n



printSolutions :: (MonadIO m, MonadState WordleSolver m) => Int -> m ()
printSolutions i = do
	allWords <- fmap lines $ liftIO $ readFile "words.txt"
	let words' = map (map toLower) $ filter ((i==) . length) allWords

	cs <- get
	-- ~ liftIO $ mapM (\(c,n) -> putStrLn $ show c ++ " - " ++ show n) cs

	-- non-letter list
	let csn = map fst $ filter (\(c,mn) -> isNothing mn) cs

	let cs' = catMaybes $ map (\(c,mn) -> mn >>= \n -> return (c, n)) cs

	-- clearing black fields that were also green and yellow elsewhere
	-- TODO clear only for the specific position - requires state format change
	let csn' = filter (\c -> notElem c $ map fst cs') csn

	-- letter list without place and with place
	let (cs0,cs1) = partition ((<=0) . snd) cs'

	-- tha filter
	let fwords = do
		w <- words'
		guard $ and $ map isAlpha w
		mapM_ (\c -> guard $ notElem c w) csn'
		mapM_ (\(c,_) -> guard $ elem c w) cs0
		mapM_ (\(c,n) -> guard $ w !? (-succ n) /= Just c) cs0
		mapM_ (\(c,n) -> guard $ w !? pred n == Just c) cs1
		return w

	liftIO $ putStrLn $ unlines $ sortBy (comparing score) fwords


score :: String -> Float
score = sum . map letterScore . nub

letterScore :: Char -> Float
letterScore c = case c of
	'e' -> 0.11
	's' -> 0.089
	'i' -> 0.086
	'a' -> 0.078
	'r' -> 0.073
	'n' -> 0.072
	't' -> 0.067
	'o' -> 0.061
	'l' -> 0.053
	'c' -> 0.040
	'd' -> 0.038
	'u' -> 0.033
	'g' -> 0.030
	'p' -> 0.028
	'm' -> 0.027
	'h' -> 0.023
	'b' -> 0.020
	'y' -> 0.016
	'f' -> 0.014
	_ -> 0

