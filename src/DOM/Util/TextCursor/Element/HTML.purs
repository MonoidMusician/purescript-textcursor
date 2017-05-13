module DOM.Util.TextCursor.Element.HTML
    ( value, setValue
    , selectionStart, setSelectionStart
    , selectionEnd, setSelectionEnd
    ) where

import Prelude
import DOM.Util.TextCursor.Element.Type (TextCursorElement(TextArea, Input))
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLInputElement, HTMLTextAreaElement)
import DOM.HTML.HTMLInputElement as HInput
import DOM.HTML.HTMLTextAreaElement as HTextArea

-- | Lift a pair of getters (Input/TextArea).
getter
    :: forall a.
    (HTMLInputElement -> a) ->
    (HTMLTextAreaElement -> a) ->
    TextCursorElement -> a
getter f _ (Input e) = f e
getter _ g (TextArea e) = g e

-- | Lift a pair of setters (Input/TextArea).
setter
    :: forall a b.
    (b -> HTMLInputElement -> a) ->
    (b -> HTMLTextAreaElement -> a) ->
    b -> TextCursorElement -> a
setter f _ v (Input e) = f v e
setter _ g v (TextArea e) = g v e

-- | Get the current text value of a `TextCursorElement`.
value :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) String
value = getter HInput.value HTextArea.value

-- | Set the text value of a `TextCursorElement` to the specified string.
setValue :: forall eff. String -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setValue = setter HInput.setValue HTextArea.setValue

-- | Get the index of the start of the selection.
selectionStart :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) Int
selectionStart = getter HInput.selectionStart HTextArea.selectionStart

-- | Set the index of the start of the selection.
setSelectionStart :: forall eff. Int -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setSelectionStart = setter HInput.setSelectionStart HTextArea.setSelectionStart

-- | Get the index of the end of the selection.
selectionEnd :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) Int
selectionEnd = getter HInput.selectionEnd HTextArea.selectionEnd

-- | Set the index of the end of the selection.
setSelectionEnd :: forall eff. Int -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setSelectionEnd = setter HInput.setSelectionEnd HTextArea.setSelectionEnd
