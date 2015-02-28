# Digital World

How to construct a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the [Digital
Monsters](http://en.wikipedia.org/wiki/Digimon).

## 1. Living Digimons

* **[Digimon-Gnome: The Meta-Project built for developers to make life
  simple](digignome.gyoudmon.org)**

* [Nanomon: Digging data from Chaos such as iPhone
  Backups.](nanomon.gyoudmon.org)

* [Sakuyamon: Be in charge of
  [gyoudmon.org](http://gyoudmon.org).](sakuyamon.gyoudmon.org)

## 2. Project Conventions

### 2.1. Building Scripts

* **[prerequisites.sh](/DigiGnome/prerequisites.sh)**: Build the latest
  Racket and Swi-Prolog from offical source.

* **[makefile.rkt](/makefile.rkt)**: As its name shows, it is the
  replacement of Makefile, and it's the toplevel one.

* **submake.rkt**: As its name shows, it is the sub makefile that may
  exist in every _digimon_ directory.

### 2.2. Hierarchy

For the sake of consistency and better architecture, we follow the
concept of [Racket Multi-collection
Package](http://docs.racket-lang.org/pkg/Package\_Concepts.html\#%28tech.\_multi.\_collection.\_package%29).
Projects/subcollections listed in the root directory are organized as
the _digimons_, and each of them may be separated into several
repositories.

* **DigiGnome** is the reserved _digimon_ who's duties are making life
  easy for developers and sharing code base for other _digimons_.
  Namely _digimon_ works like `src` **and** `libraries`/`frameworks`.

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

### 2.3. Version

Obviousely, our _digimons_ have their own life cycle.

> The **Baby I** and **Baby II** are merged as the **Baby** to make life
> simple.

* **Baby**: The 1st stage of _digimon evolution_ hatching straightly
  from her _digitama_. Namely it's the `Alpha Version`.

* **Child**: The 2nd stage of _digimon evolution_ evolving quickly from
  **Baby**. Namely it's the `Beta Version`.

* **Adult**: The 3rd stage of _digimon evolution_ evolving from
  **Child**. At this stage _digimons_ are strong enough to live on their
  own.
