module Main where

import Control.Monad.State (evalState)
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random

import Colour
import ColourGen
import Figure hiding (size)
import GraphicsContext
import Options
import Settings
import Tiles

import DividerEval

main = do
	args <- getArgs
	s <- parseCommandLineToSettings args
	case s of
		Left error -> do
			hPutStr stderr error
			exitFailure
		Right settings -> runWith settings

runWith settings =
	let imgSize = size settings
	in drawTilesOnBitmapFile
		imgSize
		(outputFile settings)
		(backColour settings) 
		(map fromColour $ coloursFromSeed $ randomSeed settings)
		(divider settings $ Rect (0, 0) imgSize)


-- TODO •Ê‚Ì‚Æ‚±‚ë‚É‚â‚è‚½‚¢
coloursFromSeed seed =
	evalState (rgbRandomWalk (-0.025, 0.025) (Rgb 0.5 0.5 0.5))
		(mkStdGen seed)

