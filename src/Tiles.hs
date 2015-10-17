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

drawTiles :: [GraphicsContext] -> [Figure] -> DrawProc
drawTiles figs ctxs img =
	let procs = zipWith drawFigure figs ctxs
	in sequence_ $ map ($ img) procs
--		foldl (>>) (return ()) procs
		-- ((return () >> procs !! 0) >> procs !! 1)
		-- return ()
	

