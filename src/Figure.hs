module Figure where

{- |
	座標やサイズを表す、2 次元ベクトルの型
-}
type Vec2d = (Int, Int)

(.+) :: Vec2d -> Vec2d -> Vec2d
(lx, ly) .+ (rx, ry) = (lx + rx, ly + ry)

(.-) :: Vec2d -> Vec2d -> Vec2d
(lx, ly) .- (rx, ry) = (lx - rx, ly - ry)

{- |
	図形を表す型。色や塗り方は別途定める
-}
data Figure =
	-- | 点。位置だけを持つ
	Dot { position :: Vec2d }

	{- |
		矩形。左上の位置と、サイズ（幅・高さ）を持つ。
		辺は x 軸・y 軸に平行である
	-}
	| Rect { position :: Vec2d, size :: Vec2d }
	deriving Show

-- | 平行移動
translate :: Vec2d -> Figure -> Figure
translate offset (Dot orig) = Dot (orig .+ offset)
translate offset rect@Rect { position = orig } =
	rect { position = orig .+ offset }

-- | 鏡像（x = 0 を軸とする）
mirrorX :: Figure -> Figure
mirrorX (Dot (x, y)) = Dot (-x, y)
mirrorX rect@(Rect (x, y) (w, h)) = rect { position = (-x - w, y) }

-- | 鏡像（y = 0 を軸とする）
mirrorY :: Figure -> Figure
mirrorY (Dot (x, y)) = Dot (x, -y)
mirrorY rect@(Rect (x, y) (w, h)) = rect { position = (x, -y - h) }


