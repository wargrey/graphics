# [🏡<sub>🐈</sub>](http://gyoudmon.org/~wargrey:digignome)Digital World

How to construct a _Digital World_? Okay, we don't start with the _File
Island_, but we own some concepts from the [Digital
Monsters](http://en.wikipedia.org/wiki/Digimon).

## 1. Living Digimons

* **[Digimon-Gnome: The Meta-Project built for developers to make life
  simple](http://gyoudmon.org/~wargrey:digignome)**

* [Kuzuhamon: An instance of Per-Tamer Terminus as the part of
  gyoudmon.org.](http://gyoudmon.org/~wargrey:kuzuhamon)

* [Nanomon: Digging data from Chaos such as iPhone
  Backups.](http://gyoudmon.org/~wargrey:nanomon)

* [Sakuyamon: Manage and protect gyoudmon.org as well as the digital
  world.](http://gyoudmon.org/~wargrey:sakuyamon)

## 2. Project Conventions

Since I'm an indenpendent developer and communicating costs almost
nothing, I make decision at my risk to model software system in _[Formal
Methods](http://en.wikipedia.org/wiki/Formal\_methods)_ and document it
with the _[Literate
Programming](http://en.wikipedia.org/wiki/Literate\_programming)_
approach.

### 2.1. Building Scripts

* **[prerequisites.sh](/DigiGnome/prerequisites.sh)**: Build the latest
  Racket and Swi-Prolog from offical source.

* **[makefile.rkt](/DigiGnome/makefile.rkt)**: It is the replacement of
  Makefile, and it's the toplevel one.

* **submake.rkt**: It is the sub makefile that might exist in every
  _digimon_ directory.

### 2.2. Hierarchy

> The _Digital World_ itself as a whole is usually placed within your
> `$HOME` directory     named `DigitalWorld` if you want to take
> advantages of the infrastructures.

For the sake of consistency and better architecture, we follow the
concept of _[Racket Multi-collection
Package](http://docs.racket-lang.org/pkg/Package\_Concepts.html\#%28tech.\_multi.\_collection.\_package%29)_.
Projects/subcollections listed in the root directory are organized as
the _digimons_, and each of them may be separated into several
repositories.

* **[DigiGnome](/DigiGnome)** is the reserved _digimon_ whose duties are
  making life easy for developers and  sharing code base for other
  _digimons_. Namely _digimon_ works like `src` **as well as**
  `libraries`/`frameworks`.

  * **stone** stores immutable meta-information or ancient sources to be
    translated. Yes, it's the _Rosetta Stone_.

  * **digitama** is the egg of _digimons_.  Namely sources within it are
    **hidden** to others. **Compatibility will not be maintained and Use
    at your own risk!**

  * **digivice** is the interface for users to talk with _digimons_.
    Namely sources within it implement executable tools that will be
    called by wrappers from `bin`.

  * **tamer** is the interface for developers to tame the _digimons_.
    Namely it works like `test`.  After all the _tamer/handbook.scrbl_
    plays the role of  the [Test
    Report](http://en.wikipedia.org/wiki/Behavior-driven\_development)
    along with  the [Design
    Documentation](http://en.wikipedia.org/wiki/Design\_by\_contract)
    so that the production code could keep elegant.

* **[Kuzuhamon](https://github.com/digital-world/Kuzuhamon)** is another
  reserved _digimon_ who works as the _Per-Tamer Website_  if
  [sakuyamon](https://github.com/digital-world/sakuyamon) is deployed in
  the same machine.

  * **terminus** manages guilds of _digimons_. Hmm... Sounds weird,
    nonetheless, try `htdocs`.

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
