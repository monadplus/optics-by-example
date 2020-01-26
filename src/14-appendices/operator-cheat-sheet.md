## Operator Cheat Sheet

### Legend for Getters

| Symbol | Description                                     |
| ------ | ----------------------------------------------- |
| ^      | Denotes a Getter                                |
| @      | Includes the index with the result              |
| .      | Get a single value                              |
| ..     | Get a List of values                            |
| ?      | Maybe get the first value                       |
| !      | Force a result or throw an exception if missing |

#### Examples

```
(^@..) :: s -> IndexedFold i s a -> [(i, a)]

  A getter (^) which includes the index (@) in a list of all focuses (..)

>>> "Yarrr" ^@.. folded
[(0, 'Y), (1, 'a'), (2, 'r'), (3, 'r'), (4, 'r')]
```

More :

```
(^?!) :: s -> Traversal' s a -> a

  A getter (^) which forcibly gets (!) a possibly missing (?) value.

>>> Just Just "Nemo ^?! _Just
"Nemo"
>>> Nothing ^!? _Just
*** Exception
```

### Legend for Setters/Modifiers

| Symbol | Description                                        |
| ------ | -----------------------------------------------    |
| .      | Set the focus                                      |
| %      | Modify the focus                                   |
| ~      | Denotes a Setter/Modifier                          |
| =      | Denotes a Setter/Modifer over a MonadState context |
| <      | Include the altered focus with the result          |
| <<     | Include the unaltered focus with the result        |
| %%     | Perform a traversal over the focus                 |
| <>     | mappend over the focus                             |
| ?      | Wrap in Just before setting                        |
| +      | Add to the focus                                   |
| -      | Substract from the focus                           |
| *      | Multiply the focus                                 |
| //     | Divide the focus                                   |
| ||     | Logically or the focus                             |
| &&     | Logically and the focus                            |
| @      | Pass the index to the modification function        |
