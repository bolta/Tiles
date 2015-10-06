module Figure where

type Vec2d = (Int, Int)

data Figure =
	Dot { position :: Vec2d }
	| Rect { position :: Vec2d, size :: Vec2d }
	deriving Show

