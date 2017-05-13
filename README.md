# purescript-textcursor
An isomorphic `TextCursor` type for selections in DOM elements (`<input>` and `<textarea>`). Provides shared DOM methods and TextCursor manipulations.

## Supported elements
All `<textarea>` elements and `<input>` elements with the `type` attribute set to one of the following:
- input (default)
- email
- search
- url

## TextCursor
A `TextCursor` is defined as the following type:
```purescript
newtype TextCursor = TextCursor
    { before :: String
    , selected :: String
    , after :: String
    }
```

It represents a selection in an element in three regions: the text `before` the cursor/selection, the text inside the `selection` (empty if there is just a cursor), and the text `after`.

What this does is allow transformations to occur (such as replacements, white-space simplification) while preserving the relative location of the cursor or selection.

Note that certain transformations will depend on the position of the text cursor or selection within the field, particularly if they rely on a string of characters versus single characters (e.g. a replacement ` ``|'' -> "`, which depends on quote characters being adjacent, as opposed to `[A-Z] -> [a-z]`, which does not care where the cursor falls as it selects a single character without context).
