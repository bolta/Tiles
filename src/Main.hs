module Main where

import Control.Exception.Base
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

main = do
	args <- getArgs
	s <- parseCommandLineToSettings args
	case s of
		Left error -> do
			hPutStrLn stderr error
			exitFailure
		Right settings -> do
			-- クラッシュを回避するための儀式。末尾のコメント参照
			figures <- evaluate $ divider settings $ Rect (0, 0)
				$ size settings
			writeFile "dummy.txt" $ show figures

			runWith settings figures

runWith settings figures =
	drawTilesOnBitmapFile
		(size settings)
		(outputFile settings)
		(backColour settings)
		(map fromColour $ coloursFromSeed $ randomSeed settings)
		figures
		-- ↑を↓に替えるとクラッシュする。末尾のコメント参照
		-- (divider settings $ Rect (0, 0) $ size settings)

-- TODO 別のところにやりたい
coloursFromSeed seed =
	evalState (rgbRandomWalk (-0.025, 0.025) (Rgb 0.5 0.5 0.5))
		(mkStdGen seed)

{-
	--divider-expr オプションで Divider を指定した場合、
		・Divider による分割を前もって処理し、
		・その内容をダミーファイルに書き出す（？？）
	という儀式を先に行わないと、exe がクラッシュすることがある。
	
	さらに奇妙なことに、クラッシュしないこともある。どうも式の内容に依るらしく
	lrtbDivider (32, 32) だとクラッシュするが
	lrtbDivider (8, 8) だといけたりする…

	おそらく eval のためのライブラリ・ghc API のどこかにバグがあるのだと思う。
	なんか一応回避できているので、これ以上触れないことにする。

	ちなみに ghci では上記を行った現状でもクラッシュする。
	ghci は式を直接書けるので実用上の問題にはならないはず…
-}

