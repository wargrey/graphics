# Digital World

## The Book Project

This, the project of subprojects, is the Extra Dimensional Space built
for developers to set up all essentials. **Namely all subprojects can
and should be built on their own.**

### The **D-ark** Scripts
* [**prerequisites.sh**](prerequisites.sh): Build the latest Racket and
  Swi-Prolog from offical source.
* [**makefile.rkt**](makefile.rkt): As its name shows, it is the
  alternative to Makefile.

## Subprojects
* [**Sakuyamon**](https://github.com/digital-world/sakuyamon) is in
  charge of gyoudmon.org
* [**Nanomon**](https://github.com/digital-world/nanomon) is a cyberpunk
  that digging data from Chaos

## Project Conventions

How to build a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the _Digimon Series_.

### Hierarchy

**Note** Project or Subprojects are organized as the _digimons_ within
**village**. Each project may be separated into several repositories
within **island**, **tamer**, and so on.
* **The Book Project** is a mystery that sets up all essentials of the
  world.
* **village** is the birth place of _digimons_. Namely it works like
  `src`.
* **digitama** is the egg of _digimons_. Namely it works like
  `libraries` or `frameworks`.
* **island** is the living environment of _digimons_. Namely it works
  like `share` or `collection`.
  - _nature_ defines _digimons_' look and feel.
  - _stone_ stores the ancient sources to be translated.
* **d-ark** is the interface for users to talk with _digimons_. Namely
  it works like `bin`.
* **tamer** is the interface for developers to train the _digimons_.
  Namely it works like `test`.

### Version

Obviousely, our _digimons_ have their own life cycle.
* **Baby I**: The 1st stage of _digimon evolution_ hatching straighty
  from her _digitama_. Namely it's the `Alpha Version`.
* [ ] **Baby II**: The 2nd stage of _digimon evolution_ evolving quickly
  from **Baby I**. Namely it's the `Beta Version`.
* [ ] **Child**: The 3rd stage of _digimon evolution_ evolving from
  **Baby II**. At the time _digimons_ are strong enough to live on their
  own.
