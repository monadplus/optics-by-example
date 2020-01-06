## Optics

### What are optics

Optics in its more general sense is a full field of study.

In a slightly more concrete sense, optics are a family of tools which are interoperable with one another.

Lenses, Folds, Traversal, Prisms, and Isos are all types of optics.

In a nutshell, __optics are a family of inter-composable combinators for building bidirectional data transformations__.

### Strengths

Optics solve _a lot_ of very common data-manipulation problems in a __composable, performant, and concise__ way.

- __Composition__: each optic focuses on some subset of data, when composing with other optics they can pick up from where the previous optic left off and diving down even further.

- __Separation of concerns__: optics allow us to specify which portions of data we wish to work with separately from the operations we wish to perform on them. We can swap out either of the data-selector or the action without affecting the other.

- __Concision__: most task can be expressed in a single line of code, and in many cases the resulting code even reads like a simple sentence. For instance, `sumOf (key "transactions" . values . key "cost" . _Number)`

- __Enforcing interface boundaries__: optics can serve as an external interface which remains consistent despite changes to your data layer.

- __A prncipled and mature ecosystem__

### Weaknesses

- __Type-Errors__: pretty ugle type errors when something goes wrong.

- __Complex Implementation__

- __Vast collection of combinators__ (strength in disguise)
