{- |
	色に関するモジュール
-}
module Colour (RgbComp, Colour(..), asRgb, darker) where

--import Codec.Picture.Types

--type Colour = PixelRGB8
--colour = PixelRGB8

-- | RGB の成分。0 を最小、1 を最大とする
type RgbComp = Float

{- |
	色を表す型。RGB その他の表現方法をサポートすることができる（今は RGB だけ）
-}
data Colour =
	-- | 色を RGB で表現する
	Rgb { red :: RgbComp, green :: RgbComp, blue :: RgbComp }
	deriving Show

--rgb :: RgbComp -> RgbComp -> RgbComp -> Colour
--rgb r g b | r > 1.0 || g > 1.0 || b > 1.0 = undefined
--rgb r g b = Rgb r g b

{- |
	RGB に変換し、各成分を得る
-}
asRgb :: Colour -> (RgbComp, RgbComp, RgbComp)
asRgb (Rgb r g b) = (r, g, b)

{- |
	少し暗い色を得る
-}
darker :: Colour -> Colour
darker c =
	let
		(r, g, b) = asRgb c
		d = (0.75 *)
	in
		Rgb (d r) (d g) (d b)

