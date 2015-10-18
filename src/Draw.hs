{-# OPTIONS_GHC -XRankNTypes #-}

module Draw where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.Primitive -- (PrimMonad, PrimState, primToST)
import Control.Monad.ST

import Colour
import Figure
import GraphicsContext

type DrawProc = (forall s. MutableImage s PixelRGB8 -> ST s ())

toPixel :: Colour -> PixelRGB8
toPixel col =
	let
		(r, g, b) = asRgb col
		[r', g', b'] = map (fromIntegral . toPixelComp) [r, g, b]
		toPixelComp comp = normalise $ floor $ comp * 256 :: Integer
		normalise c = if c < 0 then 0 else if c > 255 then 255 else c
	in
		PixelRGB8 r' g' b'

drawFigure :: GraphicsContext -> Figure -> DrawProc
drawFigure ctx fig img =
	case fig of
		Dot (x, y) ->
			case strokeColour ctx of
				Just c -> writePixel img x y $ toPixel c
				Nothing -> return ()
		Rect (x, y) (w, h) -> do
			forM_ [(i, j) | i <- [x .. x + w - 1], j <- [y .. y + h - 1]]
				(\(i, j) ->
					let
						pixCol = case strokeColour ctx of
							Just c | isOnStroke x y w h i j -> Just c
							_ -> fillColour ctx
					in
						case pixCol of
							Just c -> writePixel img i j $ toPixel c
							Nothing -> return ()
					)
			where
				isOnStroke x y w h i j =
-- タイルが細かいとき枠線が重複して暗くなるので右・下を省く
-- （Processing 版と同様）
--					elem i [x, x + w - 1] || elem j [y, y + h - 1]
					i == x || j == y

makeBitmapFileFromProc :: Int -> Int -> FilePath -> Colour -> DrawProc -> IO ()
makeBitmapFileFromProc width height filePath backColour drawProc =
	writeBitmap filePath image
	where image = runST $ do
		img <- createMutableImage width height $ toPixel backColour
		drawProc img
		freezeImage img

makeBitmapFileFromFunc :: Int -> Int -> FilePath
	-> (Int -> Int -> Colour)
	-> IO ()
makeBitmapFileFromFunc width height filePath imageFunc =
	let dummyCol = (Rgb 0 0 0)
	in makeBitmapFileFromProc width height filePath dummyCol
		$ writeBitmapFunc width height imageFunc


writeBitmapFunc :: Int -> Int -> (Int -> Int -> Colour) -> DrawProc
writeBitmapFunc width height imageFunc img = do
	forM_ [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]] (\(x, y) ->
		writePixel img x y $ toPixel $ imageFunc x y)


-- test
d = drawFigure (GraphicsContext (Just $ Rgb 0 1 0) (Just $ Rgb 0 0 1)) $ Rect (100, 100) (200, 200)

