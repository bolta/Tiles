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

imgSize@(width, height) = (400, 300)

--colours = sampleColours
colours = synmetric $ take (40 * 30) $ evalState (rgbRandomWalk (-0.05, 0.05) (Rgb 0.5 0.5 0.5)) (mkStdGen 42)

figures = {-synmetric $-} compositeDivider
	[{-synmetric .-} tbDivider (height `div` 10), synmetric . lrDivider (width `div` 10)]
	$ Rect (0, 0) imgSize

main = makeBitmapFileFromProc width height "main.bmp" (Rgb 0 0 0)
	$ drawTiles (map fromColour colours) figures
