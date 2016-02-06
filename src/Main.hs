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
			-- �N���b�V����������邽�߂̋V���B�����̃R�����g�Q��
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
		-- �������ɑւ���ƃN���b�V������B�����̃R�����g�Q��
		-- (divider settings $ Rect (0, 0) $ size settings)

-- TODO �ʂ̂Ƃ���ɂ�肽��
coloursFromSeed seed =
	evalState (rgbRandomWalk (-0.025, 0.025) (Rgb 0.5 0.5 0.5))
		(mkStdGen seed)

{-
	--divider-expr �I�v�V������ Divider ���w�肵���ꍇ�A
		�EDivider �ɂ�镪����O�����ď������A
		�E���̓��e���_�~�[�t�@�C���ɏ����o���i�H�H�j
	�Ƃ����V�����ɍs��Ȃ��ƁAexe ���N���b�V�����邱�Ƃ�����B
	
	����Ɋ�Ȃ��ƂɁA�N���b�V�����Ȃ����Ƃ�����B�ǂ������̓��e�Ɉ˂�炵��
	lrtbDivider (32, 32) ���ƃN���b�V�����邪
	lrtbDivider (8, 8) ���Ƃ������肷��c

	�����炭 eval �̂��߂̃��C�u�����Eghc API �̂ǂ����Ƀo�O������̂��Ǝv���B
	�Ȃ񂩈ꉞ����ł��Ă���̂ŁA����ȏ�G��Ȃ����Ƃɂ���B

	���Ȃ݂� ghci �ł͏�L���s��������ł��N���b�V������B
	ghci �͎��𒼐ڏ�����̂Ŏ��p��̖��ɂ͂Ȃ�Ȃ��͂��c
-}

