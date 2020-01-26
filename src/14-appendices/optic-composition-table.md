## Optic Composition Table


Each cell denotes the most general type you can avhieve by composing the column header with the row header.

```
Example:

`traversed` (Traversal) with `_Just` (Prism) => Traversal.
```

|           | Fold | Traversal | Prism     | Lens      | Iso       |
| --------- | ---- | --------- | ------    | ------    | ------    |
| Fold      | Fold | Fold      | Fold      | Fold      | Fold      |
| Traversal | Fold | Traversal | Traversal | Traversal | Traversal |
| Prism     | Fold | Traversal | Prism     | Traversal | Prism     |
| Lens      | Fold | Traversal | Traversal | Lens      | Lens      |
| Iso       | Fold | Traversal | Prism     | Lens      | Iso       |
