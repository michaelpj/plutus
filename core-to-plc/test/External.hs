module External where

{-# INLINABLE myAnd #-}
myAnd :: Bool -> Bool -> Bool
myAnd x y = myNand (myNand x y) (myNand x y)

-- I can't persuade GHC to have an unfolding for this and *not* inline it, despite
-- putting it in a separate module, fiddling with optimization settings etc.
{-# INLINABLE myNand #-}
myNand :: Bool -> Bool -> Bool
myNand x y = if x then False else if y then False else True
