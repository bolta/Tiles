-- | プログラムの設定を表現するモジュール
module Settings (
	Settings,
		size, divider, backColour, randomSeed, outputFile,
	defaultSettings,
	makeFilenameFromSizeAndTimestamp
) where

import Data.List

import Colour
import Divider
import Figure hiding (size)
import Time

-- | プログラムの全ての設定を表す型。
-- | 各項目はオプションで与えられたり、
-- | オプションがない場合のデフォルトであったりする
data Settings = Settings {
	size :: Vec2d,
	divider :: Divider,
	backColour :: Colour,
	randomSeed :: Int,
	outputFile :: Maybe FilePath
	}

-- divider が show できないために show を再発明する羽目に…
-- 特定のメンバだけ外して show とかできないのかしら
instance Show Settings where
-- デフォルトではこれは書いちゃいけない。なんでやねん
--	show :: Settings -> String
	show settings =
		let
			members = [
				("size", show $ size settings),
				("backColour", show $ backColour settings),
				("randomSeed", show $ randomSeed settings),
				("outputFile", show $ outputFile settings)
				]
			membersStr = intercalate ", "
				$ map (\(label, content) -> label ++ " = " ++ content) members
		in
			"Settings {" ++ membersStr ++ "}"

-- | デフォルト設定。コマンドラインオプションで上書きすることができる
defaultSettings = Settings {
	size = (1024, 768),
	-- TODO このへんのものは必須にしたい…
	divider = lrtbDivider (8, 8),
	backColour = Rgb 0 0 0,
	randomSeed = 0,
	outputFile = Nothing
	}

-- | 画像のサイズと現在時刻からファイル名を作る
makeFilenameFromSizeAndTimestamp :: Vec2d -> IO FilePath
makeFilenameFromSizeAndTimestamp (wi, hi) =
	let (wi', hi') = (pad0 wi, pad0 hi)
	in do
		(y, m, d, h, i, s) <- getCurrentTimeYmdhms
		let
			y' = show y
			[m', d', h', i', s'] = map pad0 [m, d, h, i, s]

		return $ wi' ++ "x" ++ hi' ++ "_"
			++ y' ++ m' ++ d' ++ "-" ++ h' ++ i' ++ s' ++ ".bmp"
	
	where
		pad0 :: Int -> String
		pad0 i | i < 10 = '0' : show i
		pad0 i = show i


