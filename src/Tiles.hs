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

drawTiles :: Int -> Int -> [GraphicsContext] -> [Figure] -> DrawProc
drawTiles width height ctxs figs img =
	let procs = zipWith (drawFigure width height) ctxs figs
	in sequence_ $ map ($ img) procs

drawTilesOnBitmapFile :: Int -> Int -> FilePath -> Colour
	-> [GraphicsContext] -> [Figure] -> IO ()
drawTilesOnBitmapFile width height filePath backColour ctxs figs =
	makeBitmapFileFromProc width height filePath backColour
		$ drawTiles width height ctxs figs

