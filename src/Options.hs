-- | コマンドラインオプションの解析を行うモジュール
module Options (parseCommandLineToSettings, Error) where

import Control.Monad.State
import Data.Char (isDigit)
import Data.List
import System.Console.GetOpt hiding (Option)
import qualified System.Console.GetOpt as O (OptDescr(Option))
import Text.Regex.Posix

import Colour
import Divider
import DividerEval
import Figure hiding (size)
import Settings

type Error = String

-- | コマンドラインオプションのリストを与えると、解析して Settings を返す。
-- | 失敗した場合はエラー文字列を返す
parseCommandLineToSettings :: [String] -> IO (Either Error Settings)
parseCommandLineToSettings args = do
	let (options, _, errors) = parseOptions args
	if errors /= [] then return $ Left $ intercalate " / " errors
	else do
		s <- makeSettings options
		case s of
			Left error -> return $ Left error
			Right settings -> return $ Right settings


-- | コマンドラインオプションの各項目を表す型
data Option =
	Size String
	| DividerExpr String
	| RandomSeed String
	| OutputFile String
	deriving Show

availableOptions :: [OptDescr Option]
availableOptions = [
	O.Option ['s'] ["size"] (ReqArg Size "SIZE")
		("specify image size in WIDTHxHEIGHT format, "
		++ "where x is an arbitrary non-digit char. default = 1024x768"),
	O.Option ['x'] ["divider-expr"] (ReqArg DividerExpr "EXPR")
		"specify divider by Haskell expression",
	O.Option ['r'] ["random-seed"] (ReqArg RandomSeed "SEED")
		"specify integer for random seed. default = 0",
	O.Option ['o'] ["output-file"] (ReqArg OutputFile "PATH")
		"specify output bitmap file path. default = tiles.bmp"
	]

parseOptions :: [String] -> ([Option], [String], [String])
parseOptions args =
	getOpt Permute availableOptions args

makeSettings :: [Option] -> IO (Either Error Settings)
makeSettings options = (flip execStateT) (Right defaultSettings) $ do
	forM_ options (\option -> do
		settings <- get
		newSettings <- lift $ makeSettings' settings option
		put newSettings
		)

makeSettings' :: (Either Error Settings) -> Option
	-> IO (Either Error Settings)
makeSettings' error@(Left _) option = return error
makeSettings' (Right settings) option =
	case option of
		Size sizeSpec -> do
			size <- makeSize sizeSpec
			return $ case size of
				Left error -> Left error
				Right sizeVal -> Right settings { size = sizeVal }
		DividerExpr expr -> do
			div <- evalToDivider expr
			return $ case div of
				Left error -> Left $ "error in evaluating divider: " ++ error
				Right divVal -> Right settings { divider = divVal }
		RandomSeed seedSpec -> do
			seed <- makeRandomSeed seedSpec
			return $ case seed of
				Left error -> Left error
				Right seedVal -> Right settings { randomSeed = seedVal }
		OutputFile fileSpec ->
			return $ Right settings { outputFile = fileSpec }


makeSize :: String -> IO (Either Error Vec2d)
makeSize sizeSpec =
	if sizeSpec == "display" || sizeSpec == "screen"
	then
		-- TODO 画面サイズを取得
		return $ Right (1024, 768)
	else
		return $ parseSize sizeSpec

parseSize :: String -> Either Error Vec2d
parseSize sizeSpec =
	let nums = concat (sizeSpec =~ "[0-9]+" :: [[String]])
	in case nums of
		[w, h] ->
			Right (read w :: Int, read h :: Int)
		_ ->
			Left $ "invalid size: " ++ sizeSpec

makeRandomSeed :: String -> IO (Either Error Int)
makeRandomSeed seedSpec =
	if seedSpec == "time"
	then
		-- TODO 現在時刻を取得
		-- このライブラリでいけそうなのだが、ちゃんとビルドできていない？…
		-- https://hackage.haskell.org/package/unix-time-0.3.6/docs/Data-UnixTime.html
		return $ Right 1234567890
	else
		return $ parseRandomSeed seedSpec

parseRandomSeed :: String -> Either Error Int
parseRandomSeed seedSpec =
	if all isDigit seedSpec
	then
		Right $ read seedSpec
	else
		Left $ "invalid random seed: " ++ seedSpec


