module DOM.Util.TextCursor.Test.Properties where

import DOM.Util.TextCursor
import Data.String as S
import Data.Array (concat)
import Data.Traversable (for_)
import Prelude (Unit, discard, flip, (+), (<#>))
import Test.QuickCheck (QC, Result, quickCheck, (<?>), (===))

type Testing = Array (TextCursor -> Result)

test_idempotent :: Testing
test_idempotent = cases <#> prop where
    cases = [selectAll, moveCursorToStart, moveCursorToEnd]
    prop fn tc = fn tc === fn (fn tc)

test_contentPreserving :: Testing
test_contentPreserving = cases <#> prop where
    cases = [selectAll, moveCursorToStart, moveCursorToEnd]
    prop fn tc = content tc === content (fn tc)

test_resultPred :: Testing
test_resultPred = cases <#> prop where
    cases =
        [ { fn: moveCursorToStart, pred: cursorAtStart }
        , { fn: moveCursorToEnd, pred: cursorAtEnd }
        , { fn: selectAll, pred: allSelected }
        ]
    prop { fn, pred } tc = pred (fn tc) <?> "Predicate failed on function result"

test_contentGeneration :: Array (String -> Result)
test_contentGeneration = cases <#> prop where
    cases = [_before, _selected, _after]
    prop lens s = content (single lens s) === s

test_concat :: Array (String -> TextCursor -> Result)
test_concat = cases <#> prop where
    cases = [appendLeft, flip appendRight, insert]
    prop fn s tc = length (fn s tc) === length tc + S.length s

main :: QC () Unit
main = do
    let tests = concat [test_idempotent, test_contentPreserving, test_resultPred]
    for_ tests quickCheck
    for_ test_contentGeneration quickCheck
    for_ test_concat quickCheck
    quickCheck (\tc -> length tc === S.length (content tc))
