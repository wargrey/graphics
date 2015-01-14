# Digital World

## 1. Digimon-Gnome

DigiGnome itself is built for developers to set up all essentials.

## 2. Subprojects

* [**Sakuyamon**](https://github.com/digital-world/sakuyamon) is in
  charge of gyoudmon.org

* [**Nanomon**](https://github.com/digital-world/nanomon) is a cyberpunk
  that digging data from Chaos

## 3. Project Conventions

How to build a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the [Digital
Monsters](http://en.wikipedia.org/wiki/Digimon).

### 3.1. Building Scripts

* [**prerequisites.sh**](DigiGnome/prerequisites.sh): Build the latest
  Racket and Swi-Prolog from offical source.

* [**makefile.rkt**](DigiGnome/makefile.rkt): As its name shows, it is
  the replacement of Makefile, and it's the toplevel one.

* [**submake.rkt**](DigiGnome/submake.rkt): As its name shows, it is the
  sub makefile that should exist in every _digimon_ directory.

### 3.2. Hierarchy

For the sake of consistency and better architecture, we follow the
concept of [Racket Multi-collection
Package](http://docs.racket-lang.org/pkg/Package\_Concepts.html\#%28tech.\_multi.\_collection.\_package%29).
Projects/subcollections listed in the root directory are organized as
the _digimons_, and each of them may be separated into several
repositories.

* **DigiGnome** is the reserved _digimon_ who's duty is making things
  easy for developers.

* **digicore** is also reserved as the _kernel_ (rather than _digimon_)
  shared by all _digimons_.  Namely it works like `src` **and**
  `libraries`/`frameworks`, just the same as _digimons_.

  * **stone** stores immutable meta-information or ancient sources to be
    translated. Yes, it's the _Rosetta Stone_.

  * **digitama** is the egg of _digimons_.  Namely sources within it are
    **hidden** to others. **Compatibility will not be maintained and Use
    at your own risk!**

  * **digivice** is the interface for users to talk with _digimons_.
    Namely sources within it implement executable tools that will be
    called by wrappers from `bin`.

  * **tamer** is the interface for developers to train the _digimons_.
    Namely it works like `test` that follows  the principles of
    [Behavior Driven
    Development](http://en.wikipedia.org/wiki/Behavior-driven\_development).

  * **terminus** manages guilds of _digimons_. Hmm... Sounds weird,
    nonetheless, try `htdocs` or `webroot`.

### 3.3. Version

Obviousely, our _digimons_ have their own life cycle.

> The **Baby I** and **Baby II** are merged as the **Baby** to make life
> simple.

* **Baby**: The 1st stage of _digimon evolution_ hatching straightly
  from her _digitama_. Namely it's the `Alpha Version`.

* [ ] **Child**: The 2nd stage of _digimon evolution_ evolving quickly
  from **Baby**. Namely it's the `Beta Version`.

* [ ] **Adult**: The 3rd stage of _digimon evolution_ evolving from
  **Child**. At this stage _digimons_ are strong enough to live on their
  own.
