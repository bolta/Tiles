-- | 設定ファイル・コマンドラインで与えたオプションの解析を行うモジュール

{-# LANGUAGE TupleSections #-}

module Options (makeSettings, Error) where

import Control.Monad.State
import Data.Char (isDigit, toLower)
import Data.Maybe (fromMaybe, isNothing)
import Data.List (find, intercalate)
import Data.Yaml.Syck
import System.Console.GetOpt hiding (Option)
import qualified System.Console.GetOpt as O (OptDescr(Option))
import Text.Regex.Posix

import Colour
import Divider
-- import DividerEval
import Figure hiding (size)
import Settings
-- import Settings (Settings)

type Error = String

makeSettings :: [String] -> IO (Either Error Settings)
makeSettings args = do
	{-
		コマンドライン引数から設定ファイルのパスを抽出
		設定ファイル以外の引数を解析
		設定ファイルを読み込み、解析（Divider はあとで）
		デフォルト値を設定ファイルで上書き、さらに引数で上書き
		以上は文字列で処理
		ここで設定ファイルから Divider を解析
		各項目の文字列を解析、Divider と合わせて Settings を作る
	-}
	let (options, nonOptions, errors) = parseOptions args

	if errors /= [] then return $ Left $ intercalate " / " errors
	else case nonOptions of
		[] -> return $ Left $ "specify settings file"
		file : extra -> do
			when (length extra > 0) $
				putStrLn $ "extra non-option args ignored: " ++ intercalate " " extra -- とりあえずコンソールに出す
			yaml <- loadYaml file
			let
				settingTexts = makeSettingTexts defaultSettingTexts
					yaml options
				divider = parseDivider yaml
			time <- getCurrentTime
			return $ settingsFromTexts settingTexts divider time


loadYaml :: FilePath -> IO YamlNode
loadYaml = parseYamlFile

-- | テキストによる各種パラメータの設定。設定ファイルとコマンドライン引数で指定する際の表現
data SettingTexts =
	SettingTexts {
		sizeText :: String,
		backColourText :: String,
		-- TODO ここは Divider の種と Colour の種（その他必要に応じて）を分けた方がよいだろう
		randomSeedText :: String,
		outFileText :: String
	}

defaultSettingTexts = SettingTexts {
	sizeText = "1024x768",
	backColourText = "#000000",
	randomSeedText = "time",
	outFileText = ""
}

keySize = "size"
keyBackColour = "back-colour"
keyRandomSeed = "random-seed"
keyOutFile = "out-file"

-- | デフォルトの設定を、YAML とコマンドライン引数で上書きして設定を得る。
-- | 処理は文字列レベルで行う
makeSettingTexts :: SettingTexts -> YamlNode -> [Option] -> SettingTexts
makeSettingTexts difault yaml options =
	let
		overrideByYaml keyName prop =
			fromMaybe (prop difault) $ yaml !!! keyName >>= asString
		yamlApplied = SettingTexts {
			sizeText = overrideByYaml keySize sizeText,
			backColourText = overrideByYaml keyBackColour backColourText,
			randomSeedText = overrideByYaml keyRandomSeed randomSeedText,
			outFileText = overrideByYaml keyOutFile outFileText
		}

		overrideBySettings keyName prop =
			fromMaybe (prop yamlApplied) $ lookup keyName options 
	in
		SettingTexts {
			sizeText = overrideBySettings keySize sizeText,
			backColourText = overrideBySettings keyBackColour backColourText,
			randomSeedText = overrideBySettings keyRandomSeed randomSeedText,
			outFileText = overrideBySettings keyOutFile outFileText
		}
	
(!!!) :: YamlNode -> String -> Maybe YamlNode
node !!! keyName =
	case n_elem node of
		EMap entries ->
			let
				keyHits (k, _) = n_elem k == (EStr $ packBuf keyName)
			in
				case find keyHits entries of
					Just (_, v) -> Just v
					Nothing -> Nothing
		otherwise -> Nothing

-- | 文字列が入っていることが期待される YamlNode から、
-- | 可能であれば文字列を取り出す
asString :: YamlNode -> Maybe String
asString node =
	case n_elem node of
		EStr content -> Just $ unpackBuf content
		otherwise -> Nothing

-- | YAML で書かれた Divider 構成をパーズして値を得る
parseDivider :: YamlNode -> Divider
parseDivider _ = lrtbDivider (16, 16) -- TODO 仮

-- | 文字列で書かれた設定一式を別途組み立てた Divider と合わせて
-- | Settings の値に変換する
settingsFromTexts :: SettingTexts -> Divider -> Int -> Either Error Settings
settingsFromTexts texts divider timeSeed = do
	size <- parseSize $ sizeText texts
	backColour <- parseColour $ backColourText texts
	randomSeed <- parseRandomSeed (randomSeedText texts) timeSeed
	-- TODO 現在日時を引数で受け取ってファイル名に含める
	let outputFile = "hoge.bmp" -- "makeOutputFilePath (outputFile texts) 
	
	return $ Settings {
		size = size,
		divider = divider,
		backColour = backColour,
		randomSeed = randomSeed,
		outputFile = outputFile
	}

type Option = (String, String)

availableOptions :: [OptDescr Option]
availableOptions = [
	O.Option ['s'] [keySize] (ReqArg (keySize, ) keySize)
		("specify image size in WIDTHxHEIGHT format, "
		++ "where x is an arbitrary non-digit char. default = 1024x768"),
--	O.Option ['x'] ["divider-expr"] (ReqArg DividerExpr "EXPR")
--		"specify divider by Haskell expression",
	O.Option ['r'] [keyRandomSeed] (ReqArg (keyRandomSeed, ) "SEED")
		"specify integer for random seed. default = 0",
	O.Option ['o'] ["output-file"] (ReqArg (keyOutFile, ) "PATH")
		"specify output bitmap file path. default = tiles.bmp"
	]

parseOptions :: [String] -> ([Option], [String], [String])
parseOptions args =
	getOpt Permute availableOptions args

{-
-- | プログラムオプションのセットから設定を作る。
-- | outputFile は必ず有効な値が入る
makeSettings :: [Option] -> IO (Either Error Settings)
makeSettings options = (flip execStateT) (Right defaultSettings) $ do
	forM_ options (\option -> do
		settings <- get
		newSettings <- lift $ makeSettings' settings option
		put newSettings
		)
	-- サイズが決まらないとデフォルトのファイル名はつけられないので…
	get >>= \s -> case s of
		Right settings | isNothing $ outputFile settings -> do
			filename <- lift $ makeFilenameFromSizeAndTimestamp
				$ size settings
			put $ Right settings { outputFile = Just filename }
		otherwise -> return ()

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
--		DividerExpr expr -> do
--			div <- evalToDivider expr
--			return $ case div of
--				Left error -> Left $ "error in evaluating divider: " ++ error
--				Right divVal -> Right settings { divider = divVal }
		RandomSeed seedSpec -> do
			seed <- makeRandomSeed seedSpec
			return $ case seed of
				Left error -> Left error
				Right seedVal -> Right settings { randomSeed = seedVal }
		OutputFile fileSpec ->
			return $ Right settings { outputFile = Just fileSpec }


makeSize :: String -> IO (Either Error Vec2d)
makeSize sizeSpec =
	if sizeSpec == "display" || sizeSpec == "screen"
	then
		-- TODO 画面サイズを取得
		return $ Right (1024, 768)
	else
		return $ parseSize sizeSpec
-}
parseSize :: String -> Either Error Vec2d
parseSize sizeSpec =
	let nums = concat (sizeSpec =~ "[0-9]+" :: [[String]])
	in case nums of
		[w, h] ->
			Right (read w :: Int, read h :: Int)
		_ ->
			Left $ "invalid size: " ++ sizeSpec
			
parseColour :: String -> Either Error Colour
parseColour _ = Right $ Rgb 0 0 0 -- TODO 仮


getCurrentTime :: IO Int
getCurrentTime = return 1234567890 -- TODO 仮

{-
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
-}
parseRandomSeed :: String -> Int -> Either Error Int
parseRandomSeed seedSpec timeSeed =
	if map toLower seedSpec == "time" then
		Right $ timeSeed
	else if all isDigit seedSpec then
		Right $ read seedSpec
	else
		Left $ "invalid random seed: " ++ seedSpec

