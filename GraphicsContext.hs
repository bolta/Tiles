module GraphicsContext where

import Colour

data GraphicsContext =
	GraphicsContext {
		strokeColour :: Maybe Colour,
		fillColour :: Maybe Colour
		}
	deriving Show

fromColour :: Colour -> GraphicsContext
fromColour col =
	GraphicsContext {
		strokeColour = Just $ darker col,
		fillColour = Just col }

