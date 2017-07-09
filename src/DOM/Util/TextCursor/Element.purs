module DOM.Util.TextCursor.Element
    ( module DOM.Util.TextCursor.Element.Type
    , module DOM.Util.TextCursor.Element.HTML
    , textCursor, setTextCursor
    , modifyTextCursor, modifyTextCursorST
    , focusTextCursor, focusTextCursorById
    ) where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.String (length, splitAt)
import Data.Lens (Lens', (.~))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.State.Class (class MonadState, modify)
import DOM (DOM)
import DOM.Node.Types (ElementId)
import DOM.HTML.HTMLElement (focus)
import DOM.Util.TextCursor (TextCursor(..), content)
import DOM.Util.TextCursor.Element.Type
    ( TextCursorElement(..)
    , htmlTextCursorElementToHTMLElement
    , read, readEventTarget
    , validate, validate'
    , lookupAndValidate
    , lookupValidateAndDo
    )
import DOM.Util.TextCursor.Element.HTML
    ( value, setValue
    , selectionStart, setSelectionStart
    , selectionEnd, setSelectionEnd
    )

-- | Helper to split a `String` at a specific position without worrying about
-- | `Nothing`.
splitAtRec :: Int -> String -> { before :: String, after :: String }
splitAtRec i s = case splitAt i s of
    Just split    -> split
    _ | i > 0     -> { before: s, after: "" }
      | otherwise -> { before: "", after: s }

-- | Get the `TextCursor` from a `TextCursorElement`.
textCursor :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) TextCursor
textCursor element = do
    val <- value element
    start <- selectionStart element
    end <- selectionEnd element
    let { before: prior, after } = splitAtRec end val
    let { before, after: selected } = splitAtRec start prior
    pure $ TextCursor
        { before
        , selected
        , after
        }

-- | Set the `TextCursor` on a `TextCursorElement`. Calls `setValue`,
-- | `setSelectionStart`, and `setSelectionEnd`.
setTextCursor :: forall eff. TextCursor -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
setTextCursor (tc@TextCursor { before, selected, after }) element = do
    setValue (content tc) element
    let start = length before
    let end = start + length selected
    setSelectionStart start element
    setSelectionEnd end element

-- | Modifies the `TextCursor` on an element through the given endomorphism.
modifyTextCursor :: forall eff. (TextCursor -> TextCursor) -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
modifyTextCursor f element = do
    tc <- f <$> textCursor element
    setTextCursor tc element

-- | Modifies the `TextCursor` on an element as well as setting the result in a
-- | State+Eff monad via a lens. Useful for components processing input events!
modifyTextCursorST :: forall eff m s.
    MonadState s m =>
    MonadEff ( dom :: DOM | eff ) m =>
    Lens' s TextCursor ->
    (TextCursor -> TextCursor) ->
    TextCursorElement -> m Unit
modifyTextCursorST l f element = do
    tc <- liftEff $ f <$> textCursor element
    liftEff $ setTextCursor tc element
    modify $ l .~ tc

-- | Focuses an element after setting the `TextCursor`.
focusTextCursor :: forall eff. TextCursor -> TextCursorElement -> Eff ( dom :: DOM | eff ) Unit
focusTextCursor tc element = do
    setTextCursor tc element
    focus (htmlTextCursorElementToHTMLElement element)

-- | Looks up an element by id to focus with a `TextCursor`.
focusTextCursorById :: forall eff. ElementId -> TextCursor -> Eff ( dom :: DOM | eff ) Unit
focusTextCursorById name tc = do
    lookupValidateAndDo name (focusTextCursor tc)
