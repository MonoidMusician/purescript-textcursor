module DOM.Util.TextCursor
    ( TextCursor(..)
    , content, length, null, empty, single
    , _before, _selected, _after, _all
    , atStart, atEnd, allSelected
    , isCursor, cursorAtStart, cursorAtEnd
    , isSelection, selectionAtStart, selectionAtEnd
    , selectAll, moveCursorToStart, moveCursorToEnd
    , appendLeft, appendRight
    , modifySelected, modifyAll
    , insert
    ) where

import Prelude
import Data.Lens (Lens', Traversal', over, wander, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Types (Setter')
import Data.Newtype (class Newtype)
import Data.String (length, null) as S
import Data.Symbol (SProxy(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

-- | The `TextCursor` type represents text selection within an input element.
-- | It consists of three regions of text: the text before the cursor, the text
-- | selected, and the text after the selection. This allows replacements to
-- | occur while keeping intact the cursor position/selection.
newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }

derive instance textCursorNewtype :: Newtype TextCursor _

instance eqTextCursor :: Eq TextCursor where
    eq (TextCursor l) (TextCursor r) =
        l.before == r.before &&
        l.selected == r.selected &&
        l.after == r.after

instance showTextCursor :: Show TextCursor where
    show = case _ of
        TextCursor { before: "", selected: "", after: "" } ->
            "«»"
        TextCursor { before, selected: "", after } ->
            "«" <> before <> "|" <> after <> "»"
        TextCursor { before, selected, after } ->
            "«" <> before <> "\27[4m" <> selected <> "\27[24m" <> after <> "»"

instance arbitraryTextCursor :: Arbitrary TextCursor where
    arbitrary = mkTextCursor <$> arbitrary <*> arbitrary <*> arbitrary

data ContentTest = Null | Any | Full
type ContentPredicate =
    { before :: ContentTest
    , selected :: ContentTest
    , after :: ContentTest
    }
testContent :: ContentPredicate -> TextCursor -> Boolean
testContent
    { before, selected, after }
    (TextCursor { before: b, selected: s, after: a }) =
        test before b && test selected s && test after a
    where
        test Any _ = true
        test Null t = S.null t
        test Full t = not (S.null t)

-- | Get the current text in the field. (Everything before, inside, and after
-- | the selection.)
content :: TextCursor -> String
content (TextCursor { before, selected, after }) = before <> selected <> after

-- | Get the content length.
-- check: length textcursor == length (content textcursor)
length :: TextCursor -> Int
length (TextCursor { before, selected, after }) =
    S.length before + S.length selected + S.length after

-- | Test whether there is no content.
-- check: null textcursor == null (content textcursor)
null :: TextCursor -> Boolean
null = testContent { before: Null, selected: Null, after: Null }

-- | An empty input field. No selection.
-- check: null empty
empty :: TextCursor
empty = TextCursor { before: "", selected: "", after: "" }

-- | Create a `TextCursor` from three strings.
mkTextCursor :: String -> String -> String -> TextCursor
mkTextCursor before selected after = TextCursor { before, selected, after }

-- | Apply a `Lens` setting a value to an empty `TextCursor`. When used with
-- | `_before`, `_selected`, or `_after` this will provide a `TextCursor` with
-- | only one non-empty field.
single :: Setter' TextCursor String -> String -> TextCursor
single l v = l .~ v $ empty

-- | Lens for the text before the selection. Empty if the cursor is at the
-- | beginning or the selection starts from the beginning.
_before :: Lens' TextCursor String
_before = _Newtype <<< prop (SProxy :: SProxy "before")

-- | Lens for the text that is selected. Empty if nothing is selected.
_selected :: Lens' TextCursor String
_selected = _Newtype <<< prop (SProxy :: SProxy "selected")

-- | Lens for the text after the selection. Empty if the cursor or selection
-- | reaches the end.
_after :: Lens' TextCursor String
_after = _Newtype <<< prop (SProxy :: SProxy "after")

-- | Lens for traversing/setting all three fields.
_all :: Traversal' TextCursor String
_all = wander trav
    where
        -- Monomorphic traverse
        trav :: forall m. Applicative m => (String -> m String) -> TextCursor -> m TextCursor
        trav f (TextCursor { before, selected, after }) =
            mkTextCursor <$> f before <*> f selected <*> f after

-- | Test whether the cursor or selection touches the start.
atStart :: TextCursor -> Boolean
atStart = testContent { before: Null, selected: Null, after: Any }

-- | Test whether the cursor or selection reaches the end.
atEnd :: TextCursor -> Boolean
atEnd = testContent { before: Any, selected: Any, after: Null }

-- | Test whether the cursor has selected the whole field.
allSelected :: TextCursor -> Boolean
allSelected = testContent { before: Null, selected: Any, after: Null }

-- | Test whether the selection is collapsed, i.e. acts like a cursor.
isCursor :: TextCursor -> Boolean
isCursor = testContent { before: Any, selected: Null, after: Any }

-- | Test whether the cursor is at the start with no selection.
cursorAtStart :: TextCursor -> Boolean
cursorAtStart = isCursor && atStart

-- | Test whether the cursor is at the end with no selection.
cursorAtEnd :: TextCursor -> Boolean
cursorAtEnd = isCursor && atEnd

-- | Test whether some text is selected.
isSelection :: TextCursor -> Boolean
isSelection = testContent { before: Any, selected: Full, after: Any }

-- | Test whether there is a selection that starts at the beginning.
selectionAtStart :: TextCursor -> Boolean
selectionAtStart = isSelection && atStart

-- | Test whether there is a selection that reaches the end.
selectionAtEnd :: TextCursor -> Boolean
selectionAtEnd = isSelection && atEnd

-- | Select all of the text in a field.
-- |
-- | Note: selection direction is not specified.
-- Content-preserving idempotent endomorphism
selectAll :: TextCursor -> TextCursor
selectAll tc = TextCursor
    { before: ""
    , selected: content tc
    , after: ""
    }

-- | Move the cursor to the start of a field, preserving the overall text
-- | content.
-- Content-preserving idempotent endomorphism
moveCursorToStart :: TextCursor -> TextCursor
moveCursorToStart tc = TextCursor
    { before: ""
    , selected: ""
    , after: content tc
    }

-- | Move the cursor to the end of a field, preserving the overall text content.
-- Content-preserving idempotent endomorphism
moveCursorToEnd :: TextCursor -> TextCursor
moveCursorToEnd tc = TextCursor
    { before: content tc
    , selected: ""
    , after: ""
    }

-- | Prepend a string, on the left.
appendLeft :: String -> TextCursor -> TextCursor
appendLeft s tc = over _before (s <> _) tc

-- | Append a string, on the right.
appendRight :: TextCursor -> String -> TextCursor
appendRight tc s = over _after (_ <> s) tc

-- | Modify just the selected region with an endomorphism.
modifySelected :: (String -> String) -> TextCursor -> TextCursor
modifySelected = over _selected

-- | Map all three regions of the `TextCursor` with an endomorphism, performing
-- | a replacement or other transformation such as normalization.
modifyAll :: (String -> String) -> TextCursor -> TextCursor
modifyAll = over _all

-- | Insert a string at the cursor position. If text is selected, the insertion
-- | will be part of the selection. Otherwise it is inserted before the cursor.
-- check:
--     length (insert insertion textcursor)
--  == length insertion + length textcursor
insert :: String -> TextCursor -> TextCursor
insert insertion = case _ of
    TextCursor { before, selected: "", after } -> TextCursor
        { before: before <> insertion
        , selected: ""
        , after: after
        }
    TextCursor { before, selected, after } -> TextCursor
        { before: before
        , selected: selected <> insertion
        , after: after
        }
