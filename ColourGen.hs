module ColourGen where

import Control.Monad.State
import Data.Fixed (mod')
import System.Random

import Colour

sampleColours :: [Colour]
sampleColours = zipWith3 Rgb (loop 0) (loop $ 1 / 3) (loop $ 2 / 3)
	where loop start =
		iterate (\c -> (c + 1 / 256) `mod'` 1) start

randomWalk :: (Num a, Ord a, Random a, RandomGen g) => (a, a) -> (a, a) {- -> Int -}
	-> State (a, g) [a]
randomWalk (min, max) (dMin, dMax) {-times-} =
	sequence $ repeat $ randomStep (min, max) (dMin, dMax)

randomStep :: (Num a, Ord a, Random a, RandomGen g) => (a, a) -> (a, a)
	-> State (a, g) a
randomStep (min, max) (dMin, dMax) {-times-} = do
	(prev, gen) <- get
	let (step, newGen) = randomR (dMin, dMax) gen
	let newVal = limit (min, max) $ prev + step
	put (newVal, newGen)
	return newVal

-- val の値を [min, max] の範囲内に限定する
limit :: (Ord a) => (a, a) -> a -> a
limit (min, max) val =
	if val < min then min else if max < val then max else val



rgbRandomWalk :: (RandomGen g) => (RgbComp, RgbComp) -> Colour
	-> State g [Colour]
rgbRandomWalk dRange init =
	let
		componentRandomWalk :: (RandomGen g) => State (RgbComp, g) [RgbComp]
		componentRandomWalk = randomWalk (0, 1) dRange

		(initR, initG, initB) = asRgb init
	in do
		gen <- get
		let [genR, genG, genB, genNext] = take 4 $ splitRandomGens gen
		put genNext
		
		let rs = evalState componentRandomWalk (initR, genR)
		let gs = evalState componentRandomWalk (initG, genG)
		let bs = evalState componentRandomWalk (initB, genB)
		return $ zipWith3 Rgb rs gs bs

-- TODO 大量に作ると out of memory を起こす。正格評価が必要？
splitRandomGens :: (RandomGen g) => g -> [g]
splitRandomGens g =
	let
		(g1, g2) = split g
	in
		g1 : splitRandomGens g2

-- リストの要素を偶数番目は先頭から、奇数番目は末尾から並べることで
-- 大体左右対称にする
synmetric :: [a] -> [a]
synmetric xs =
	let
		ixs = zip [0 .. ] $ xs
		evenOdd r = map snd $ filter (\(i, x) -> i `mod` 2 == r) ixs
		evens = evenOdd 0
		odds = evenOdd 1
	in
		evens ++ reverse odds

