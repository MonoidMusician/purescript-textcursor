module DOM.Util.TextCursor.Monoid where

import Prelude
import DOM.Util.TextCursor
    ( TextCursor, empty
    , appendLeft, appendRight
    , isCursor, content
    )
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

newtype Leftmost = Leftmost TextCursor
newtype Rightmost = Rightmost TextCursor
derive instance leftmostTCNewtype :: Newtype Leftmost _
derive instance rightmostTCNewtype :: Newtype Rightmost _
derive newtype instance showLeftmost :: Show Leftmost
derive newtype instance showRightmost :: Show Rightmost
instance leftmostMonoid :: Monoid Leftmost where mempty = Leftmost empty
instance rightmostMonoid :: Monoid Rightmost where mempty = Rightmost empty

instance leftmostSemigroup :: Semigroup Leftmost where
    append (Leftmost l) (Leftmost r)
        | isCursor l = Leftmost (content l `appendLeft` r)
        | otherwise  = Leftmost (l `appendRight` content r)

instance rightmostSemigroup :: Semigroup Rightmost where
    append (Rightmost l) (Rightmost r)
        | isCursor r = Rightmost (l `appendRight` content r)
        | otherwise  = Rightmost (content l `appendLeft` r)
