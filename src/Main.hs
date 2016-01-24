module Main where

import Colour
import ColourGen
import Divider
import Draw
import Figure
import GraphicsContext
import Tiles

import Control.Monad.State
import System.Random

imgSize@(width, height) = (640, 480)

colours =
--	alternate [coloursFromSeed 42, coloursFromSeed 100, coloursFromSeed 1]
	coloursFromSeed 42
	where
		coloursFromSeed seed =
			evalState (rgbRandomWalk (-0.025, 0.025) (Rgb 0.5 0.5 0.5))
				(mkStdGen seed)
--colours = synmetric $ take (40 * 30) $ evalState (rgbRandomWalk (-0.05, 0.05) (Rgb 0.5 0.5 0.5)) (mkStdGen 42)
{-
figures = {-synmetric $-} compositeDivider
	[{-synmetric .-} tbDivider (height `div` 10), synmetric . lrDivider (width `div` 10)]
	$ Rect (0, 0) imgSize
-}


-- figures = rlbtDivider (16, 16) $ Rect (0, 0) imgSize

rootDivider = compositeDivider
	[lrtbDivider (96, 80), lrtbDivider (8, 8)]

figures = rootDivider $ Rect (0, 0) imgSize

--main = makeBitmapFileFromProc width height "main.bmp" (Rgb 0 0 0)
--	$ drawTiles (map fromColour colours) figures
main =
	drawTilesOnBitmapFile width height "main.bmp" (Rgb 0 0 0)
		(map fromColour colours) figures
