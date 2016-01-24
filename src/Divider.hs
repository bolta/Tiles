{- |
	図形の分割に関するモジュール
-}
module Divider where

import Figure

-- | 図形をより小さい、順序づけられた任意個の図形へ分割する関数
type Divider = Figure -> [Figure]

lrDivider :: Int -> Divider
lrDivider count (Rect (l, t) (w, h)) =
	let
		-- 左端は 1 つ余分に必要なので count - 1 としない
		lefts = map (\i -> l + w * i `div` count) [0 .. count]
		widths = zipWith (\l r -> r - l) lefts $ drop 1 lefts
	in
		zipWith (\l w -> Rect (l, t) (w, h)) lefts widths
lrDivider _ _ = []

tbDivider :: Int -> Divider
tbDivider count (Rect (l, t) (w, h)) =
	let
		-- 上端は 1 つ余分に必要なので count - 1 としない
		tops = map (\i -> t + h * i `div` count) [0 .. count]
		heights = zipWith (\t b -> b - t) tops $ drop 1 tops
	in
		zipWith (\t h -> Rect (l, t) (w, h)) tops heights
tbDivider _ _ = []

{- |
	図形の順序を逆転する Divider
-}
reverseDivider :: Divider -> Divider
reverseDivider div fig = reverse $ div fig

{- |
	Divider を組み合わせる。
	与えられた図形を最初の Divider で割り、その結果を全て次の Divider で割り…
	という Divider が得られる
-}
compositeDivider :: [Divider] -> Divider
compositeDivider = foldl (\div accum -> concatMap accum . div) (: [])

{- |
	他の Divider で分割した結果の図形に一定の変換を施し、
	位置は変換前の位置に戻して結果とする Divider
-}
transformDivider :: (Figure -> Figure) -> Divider -> Divider
transformDivider trans div fig@(Rect (x, y) (w, h)) =
	let
		orig = div fig
		toOrigPos = translate $ position fig .- (position $ trans fig)
	in
		map (toOrigPos . trans) orig

-- | 位置はそのままで左右反転する Divider
mirrorXDivider = transformDivider mirrorX

-- | 位置はそのままで上下反転する Divider
mirrorYDivider = transformDivider mirrorY

{- |
	
-}
matrixDivider :: (Vec2d -> [Vec2d]) -> Vec2d -> Divider
matrixDivider arrangeTiles tileSize@(tileW, tileH)
		(Rect (rectL, rectT) (rectW, rectH)) =
	let
		wholeLen `divByTiles` tileLen = (wholeLen - 1) `div` tileLen + 1
		numTilesWH = (rectW `divByTiles` tileW, rectH `divByTiles` tileH)
		tileOrderByIndex = arrangeTiles numTilesWH
		tileIndexToRect (x, y) =
			Rect (rectL + tileW * x, rectT + tileH * y) tileSize
	in
		map tileIndexToRect tileOrderByIndex

-- タイルを左から右に並べた行を、上から下へ並べる Divider。
-- 引数はタイルのサイズ
lrtbDivider :: Vec2d -> Divider
lrtbDivider =
	matrixDivider (\(w, h) -> [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]])
rltbDivider = mirrorXDivider . lrtbDivider
lrbtDivider = mirrorYDivider . lrtbDivider
rlbtDivider = mirrorXDivider . lrbtDivider

