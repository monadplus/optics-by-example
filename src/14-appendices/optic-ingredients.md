### Optic Ingredients

Van Laarhoven encoding:

```haskell
(a -> f b) -> (s -> f t)
```

Profunctor encoding:

```haskell
p a (f b) -> p s (f t)
```

Composing optics **adds constraints** together, then running na optic with an action matches a data type which fulfills those constraints.

| Optic     | Constraints                           |
| --------- | ------------------------------------- |
| Lens      | Functor f                             |
| Fold      | Contravariant f, Applicative f        |
| Traversal | Applicative f                         |
| Setter    | Settable f                            |
| Getter    | Contravariant f, Functor f            |
| Iso       | Functor f, Profunctor p               |
| Prism     | Applicative f, Choice p               |
| Review    | Settable f, Profunctor p, Bifunctor p |

Here's a table of lens actions and the data-type they use to "run" the optics you pass them:

| Action                                | Data Type |
| ------------------------------------- | --------  |
| view (^.)                             | Const     |
| set (.~)                              | Identity  |
| over (%~)                             | Identity  |
| fold (toListOf, sumOf, lengthOf, etc) | Const     |
| review (#)                            | Tagged    |
| traverseOf (%%~)                      | None      |
| matching                              | Market    |
