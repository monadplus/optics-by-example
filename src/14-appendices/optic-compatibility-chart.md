## Optic Compatibility Chart

Which optics are valid substitutions for one another.

```
Example: all Prisms are a valid Traversal ?

Prism (row) - Traversal (column) => Yes
```

|           | Fold | Traversal | Lens | Review | Prism | Iso |
| --------- | ---- | --------- | ---- | ------ | ----- | --- |
| Fold      | Yes  | No        | No   | No     | No    | No  |
| Traversal | Yes  | Yes       | No   | No     | No    | No  |
| Lens      | Yes  | Yes       | Yes  | No     | No    | No  |
| Review    | No   | No        | No   | Yes    | No    | No  |
| Prism     | Yes  | Yes       | No   | Yes    | Yes   | No  |
| Iso       | Yes  | Yes       | Yes  | Yes    | Yes   | Yes |
