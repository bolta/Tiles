module Log (writeLogFile) where

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Settings
import Time

{- |
	ログファイルを出力する。
	ログファイルの名前は出力する画像ファイル名の拡張子を ".log" に替えたもの。
	ログファイルには、画像を出力する際に使われたプログラムの設定が含まれる
	（ログファイルを見れば（プログラムが同じであれば）同一の出力を再現できる）
-}
writeLogFile :: Settings -> IO ()
writeLogFile settings =
	let
		settingsStr = formatSettings settings
		logFile = logFileName $ outputFile settings
	in do
		dateTime <- getCurrentTimeYmdhms >>= return . formatDateTime1
		args <- getArgs >>= return . unwords
		writeItemsToFile logFile [
			("date time", [dateTime]),
			("command line arguments", [args]),
			("settings", settingsStr)]

logFileName :: FilePath -> FilePath
logFileName imgFileName = case imgFileName of
	[] -> []
	c : cs ->
		if imgFileName == ".bmp" then ".log"
		else c : logFileName cs

formatSettings :: Settings -> [String]
formatSettings settings =
	-- TODO インデントした方がよい
	[show settings]

writeItemsToFile :: FilePath -> [(String, [String])] -> IO ()
writeItemsToFile filePath items =
	writeFile filePath $ formatItems items


formatItems :: [(String, [String])] -> String
formatItems =
	unlines . concatMap (\(name, values) ->
		(name ++ ":")
			: map ('\t' :) values
			++ [""])


