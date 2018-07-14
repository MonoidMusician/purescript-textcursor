module Test.Main where

import Web.Util.TextCursor

import Data.Array (concat)
import Data.String as S
import Data.Traversable (for_)
import Effect (Effect)
import Prelude (Unit, discard, flip, map, ($), (+), (<#>), (<$>), (>>=), (>>>))
import Test.QuickCheck (Result, arbitrary, quickCheck, (<?>), (===))
import Test.QuickCheck.Gen (Gen)

type Testing = Array (TextCursor -> Result)

test_idempotent :: Testing
test_idempotent = cases <#> prop where
  cases = [selectAll, placeCursorAtStart, placeCursorAtEnd]
  prop fn tc = fn tc === fn (fn tc)

test_contentPreserving :: Testing
test_contentPreserving = cases <#> prop where
  cases = [selectAll, placeCursorAtStart, placeCursorAtEnd]
  prop fn tc = content tc === content (fn tc)

test_resultPred :: Testing
test_resultPred = cases <#> prop where
  cases =
    [ { fn: placeCursorAtStart, pred: cursorAtStart }
    , { fn: placeCursorAtEnd, pred: cursorAtEnd }
    , { fn: selectAll, pred: allSelected }
    ]
  prop { fn, pred } tc = pred (fn tc) <?> "Predicate failed on function result"

test_contentGeneration :: Array (String -> Result)
test_contentGeneration = cases <#> prop where
  cases = [_before, _selected, _after]
  prop lens s = content (single lens s) === s

test_concat :: Array (String -> TextCursor -> Result)
test_concat = cases <#> prop where
  cases = [appendl, flip appendr, insert]
  prop fn s tc = length (fn s tc) === length tc + S.length s

testTCG :: (TextCursor -> Result) -> Gen Result
testTCG f = map f genTextCursor

testTC :: (TextCursor -> Result) -> Effect Unit
testTC = testTCG >>> quickCheck

main :: Effect Unit
main = do
  let tests = concat [test_idempotent, test_contentPreserving, test_resultPred]
  for_ tests testTC
  for_ test_contentGeneration quickCheck
  for_ test_concat \stcr -> quickCheck $
    stcr <$> arbitrary >>= testTCG
  quickCheck (testTCG \tc -> length tc === S.length (content tc))
