module Colour (RgbComp, Colour(..), asRgb, darker) where

--import Codec.Picture.Types

--type Colour = PixelRGB8
--colour = PixelRGB8

type RgbComp = Float
data Colour = Rgb { red :: RgbComp, green :: RgbComp, blue :: RgbComp }
	deriving Show

--rgb :: RgbComp -> RgbComp -> RgbComp -> Colour
--rgb r g b | r > 1.0 || g > 1.0 || b > 1.0 = undefined
--rgb r g b = Rgb r g b

asRgb :: Colour -> (RgbComp, RgbComp, RgbComp)
asRgb (Rgb r g b) = (r, g, b)

darker :: Colour -> Colour
darker c =
	let
		(r, g, b) = asRgb c
		d = (0.75 *)
	in
		Rgb (d r) (d g) (d b)
