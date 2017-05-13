module DOM.Util.TextCursor.Element.Type
    ( TextCursorElement(..)
    , htmlTextCursorElementToHTMLElement
    , read, read', readEventTarget
    , validate, validate'
    , lookupAndValidate
    , lookupValidateAndDo
    ) where

import Prelude (Unit, bind, map, pure, (<$>), (<#>), (<<<), (>>=))
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Array (elem)
import Data.Traversable (traverse_)
import Data.Foreign (F, Foreign, toForeign)
import Control.Alternative ((<|>))
import Control.Monad.Except (runExcept)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Event (Event, target)
import DOM.HTML.Types
    ( HTMLElement, HTMLInputElement, HTMLTextAreaElement
    , htmlDocumentToNonElementParentNode
    , htmlInputElementToHTMLElement
    , htmlTextAreaElementToHTMLElement
    , readHTMLInputElement
    , readHTMLTextAreaElement
    )
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.HTMLInputElement (type_)
import DOM.Node.Types (Element, ElementId)
import DOM.Node.NonElementParentNode (getElementById)

-- | A container for the two usable `Element` types:
-- | - `HTMLInputElement`
-- | - `HTMLTextAreaElement`
-- |
-- | Note that not all `HTMLInputElement` nodes are valid, as they must contain
-- | text content. See `validate` for specifics.
-- |
-- | Common operations are defined in `TextCursor.Element.HTML`.
data TextCursorElement = Input HTMLInputElement | TextArea HTMLTextAreaElement

-- | Convert a `TextCursorElement` to a generic `HTMLElement`. Useful for
-- | `focus`.
htmlTextCursorElementToHTMLElement :: TextCursorElement -> HTMLElement
htmlTextCursorElementToHTMLElement (Input e) = htmlInputElementToHTMLElement e
htmlTextCursorElementToHTMLElement (TextArea e) = htmlTextAreaElementToHTMLElement e

-- | Read a `TextCursorElement` from a `Foreign` type.
read' :: Foreign -> F TextCursorElement
read' e = ta <|> i
    where
        -- prefer TextArea, which needs no validation
        ta = TextArea <$> readHTMLTextAreaElement e
        i = Input <$> readHTMLInputElement e

read :: Element -> F TextCursorElement
read = read' <<< toForeign

-- | Read a `TextCursorElement` from the `target` field of an `Event`.
readEventTarget :: Event -> F TextCursorElement
readEventTarget = read' <<< toForeign <<< target

-- | Validate a `TextCursorElement`. Input fields need to have one of the
-- | following types when this is called:
-- | - text (default)
-- | - email
-- | - search
-- | - url
validate :: forall eff. TextCursorElement -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
validate = case _ of
    tae@(TextArea e) -> pure (Just tae)
    Input e -> map Input <$> validateInput e
    where
        validateInput :: HTMLInputElement -> Eff ( dom :: DOM | eff ) (Maybe HTMLInputElement)
        validateInput e = do
            inputtype <- type_ e
            pure if elem inputtype whitelist
                then Just e
                else Nothing
            where
                whitelist = ["", "text", "email", "search", "url"]

-- | Convert from a `Foreign` error computation (type `F`) to a validated
-- | `TextCursorElement`.
validate' :: forall eff. F TextCursorElement -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
validate' f =
    case runExcept f of
        Left _ -> pure Nothing
        Right e -> validate e

-- | Look up a `TextCursorElement` in the document by id.
lookupAndValidate :: forall eff. ElementId -> Eff ( dom :: DOM | eff ) (Maybe TextCursorElement)
lookupAndValidate name = do
    win <- window
    doc <- htmlDocumentToNonElementParentNode <$> document win
    getElementById name doc <#> map read >>= maybe (pure Nothing) validate'

-- | Look up a `TextCursorElement` by id and run an action if found.
lookupValidateAndDo :: forall eff. ElementId -> (TextCursorElement -> Eff ( dom :: DOM | eff ) Unit) -> Eff ( dom :: DOM | eff ) Unit
lookupValidateAndDo name action =
    lookupAndValidate name >>= traverse_ action
