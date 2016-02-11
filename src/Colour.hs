{- |
	色に関するモジュール
-}
module Colour {- ( RgbComp, Colour(..), asRgb, darker, Angle(..) )-} where

import Data.Fixed (mod')

import Util

--type Colour = PixelRGB8
--colour = PixelRGB8

-- | RGB の成分。0 を最小、1 を最大とする
type RgbComp = Float

type Hue = Angle
type Saturation = Float
type Brightness = Float

{- |
	色を表す型。RGB その他の表現方法をサポートすることができる（今は RGB だけ）
-}
data Colour =
	-- | 色を RGB で表現する
	Rgb { red :: RgbComp, green :: RgbComp, blue :: RgbComp }
	| Hsb { hue :: Hue, saturation :: Saturation, brightness :: Brightness }
	deriving Show

--rgb :: RgbComp -> RgbComp -> RgbComp -> Colour
--rgb r g b | r > 1.0 || g > 1.0 || b > 1.0 = undefined
--rgb r g b = Rgb r g b

{- |
	RGB に変換し、各成分を得る
	TODO HSB からの変換はうまく動いていない
-}
asRgb :: Colour -> (RgbComp, RgbComp, RgbComp)
asRgb (Rgb r g b) = (r, g, b)
asRgb (Hsb h s v) =
	-- https://ja.wikipedia.org/wiki/HSV%E8%89%B2%E7%A9%BA%E9%96%93#HSV.E3.81.8B.E3.82.89RGB.E3.81.B8.E3.81.AE.E5.A4.89.E6.8F.9B
	-- を見てやってみたがうまく動いていない
	let
		c = s -- v * s
		h' = (asCycleCount $ normalise h) * 6
		x = c * (1 - (abs h' `mod'` 2 - 1))
		addRgb = zipWith (+)
		[r, g, b] = (map (* (v - c)) [1, 1, 1]) `addRgb` case h' of
			h' | h' < 1 -> [c, x, 0]
			h' | h' < 2 -> [x, c, 0]
			h' | h' < 3 -> [0, c, x]
			h' | h' < 4 -> [0, x, c]
			h' | h' < 5 -> [x, 0, c]
			h' | h' < 6 -> [c, 0, x]
			otherwise -> [-100,-100,-100] -- unreachable
	in (r, g, b)

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


data Angle = Degree Float | Radian Float | Cycle Float deriving Show

asCycleCount :: Angle -> Float
asCycleCount angle =
	case angle of
		Degree d -> d / 360
		Radian r -> r / 2 / pi
		Cycle c -> c

normalise :: Angle -> Angle
normalise angle =
	case angle of
		Degree d -> Degree $ normalise' 360 d
		Radian r -> Radian $ normalise' (2 * pi) r
		Cycle c -> Cycle $ normalise' 1 c

	where normalise' max value =
		let value' = value + if value < 0 then max else 0
		in value' `mod'` max
