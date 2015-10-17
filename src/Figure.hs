module Figure where

{- |
	座標やサイズを表す、2 次元ベクトルの型
-}
type Vec2d = (Int, Int)

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

