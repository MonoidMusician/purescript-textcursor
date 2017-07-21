# purescript-textcursor
An `TextCursor` isomorphism for selections in DOM elements (`<input>` and `<textarea>`). Provides shared DOM methods and TextCursor manipulations.

## The problem
In the DOM, selections within input elements are represented by a starting and ending index, and the value of the element. If the value is changed, the selection is reset, so the user loses their place in the field. The selection can be added back to the element, but accurately predicting the indices is near impossible with complex operations. This library provides a sane way of dealing with selections.

## The solution: TextCursor
A `TextCursor` is defined as the following type:
```purescript
newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }
```

It represents a selection in an element in three regions: the text `before` the cursor/selection, the text inside the `selection` (empty if there is just a cursor), and the text `after`.

This allows transformations to occur (such as replacements, white-space simplification) while preserving the relative location of the cursor or selection.

A simple replacement that matches on single characters in each field of a `TextCursor` will have the same content as replacing the entire string, but it will also preserve the locations of the cursor or selection within the field:

```purescript
replacement = toUpper
tc = TextCursor
  { before: "Hello, "
  , selected: "World"
  , after: "! Hi!"
  }
tcreplaced = modifyAll replacement tc
content tcreplaced == replacement (content tc)
```

Note that certain transformations will depend on the position of the text cursor or selection within the field, particularly if they rely on matching a string of characters versus single characters. For example, a replacement `s/''/"/g` depends on two characters being adjacent and will not match if the cursor or edge of a selection lies in between the two single quotes. But for a simple case like this, checks can be added to handle edge cases, if desired:

```purescript
q = "'"
q2 = q <> q
qq = "\""
replaceQuote = replaceAll (wrap q2) (wrap qq)
replaceLast s =
  case stripSuffix (wrap q) of
    Nothing -> s
    Just s' -> s' <> qq
-- left biased for quotes matched across an edge
stitch left right
  | endsWith q left
  && startsWith q right
    = Tuple (replaceLast left) (drop 1 right)
  | otherwise = Tuple left right
replaceAcross left right =
  stitch (replaceQuote left) right
    <#> replaceQuote
replaceQuotes = case _ of
  TextCursor { before, selected: "", after } ->
    let
      Tuple before' after' =
        replaceAcross before after
    in TextCursor
      { before: before'
      , selected: ""
      , after: after'
      }
  TextCursor { before, selected, after } ->
    let
      Tuple before' (Tuple selected' after') =
        replaceAcross before selected
          <#> flip replaceAcross after
    in TextCursor
      { before: before'
      , selected: selected'
      , after: after'
      }
```

## Supported DOM elements
All `<textarea>` elements and `<input>` elements with the `type` attribute set to one of the following:
- input (default)
- email
- search
- url
