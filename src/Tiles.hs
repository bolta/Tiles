{-# OPTIONS_GHC -XRankNTypes #-}

{- |
	Tiles アプリケーション
-}
module Tiles where

import Codec.Picture.Types

import Colour
import Draw
import Figure
import GraphicsContext

drawTiles :: Vec2d -> [GraphicsContext] -> [Figure] -> DrawProc
drawTiles imgSize ctxs figs img =
	let procs = zipWith (drawFigure imgSize) ctxs figs
	in sequence_ $ map ($ img) procs

drawTilesOnBitmapFile :: Vec2d -> FilePath -> Colour
	-> [GraphicsContext] -> [Figure] -> IO ()
drawTilesOnBitmapFile imgSize filePath backColour ctxs figs =
	makeBitmapFileFromProc imgSize filePath backColour
		$ drawTiles imgSize ctxs figs

