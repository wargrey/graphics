# The Book

Here is the **Extra Dimensional Space** built for developers to set up
all the prerequistes of the project itself.

## The **D-Ark** Scripts
* [**prerequisites.sh**](prerequisites.sh): Build the latest Racket and
  Swi-Prolog from source.
* [**makemd.rkt**](makemd.rkt): Make all the `readme.md`s if updated.

## Project Conventions

How to build a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the _Digimon Series_.

### Hierarchy

**Note** Project or Subprojects are organized as the _digimons_ within
**village**. Each project may be separated into several repositories
within **island**, **tamer**, and so on.
* **dot book** is a mystery that sets up all the prerequistes of the
  world. Sounds like `local`.
* **d-ark** is the interface for users to talk with _digimons_. Namely
  it works like `bin`.
* **village** is the birth place of _digimons_. Namely it works like
  `src`.
* **digitama** is the egg of _digimons_. Namely it works like
  `libraries` or `frameworks`.
* **island** is the living environment of _digimons_. Namely it works
  like `share` or `collection`.
  - _nature_ defines _digimons_' look and feel.
  - _stone_ stores the ancient sources to be translated.
* **tamer** is the interface for developers to train the _digimons_.
  Namely it works like `test`.

### Version

Obviousely, our _digimons_ have their own life cycle.
* [X] **Baby I** is the first stage of _digimons_ evolution straight
  from her _digitama_. Namely it's the `Alpha Version`.
