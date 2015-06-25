# [🏡](http://gyoudmon.org/~wargrey:DigiGnome)[<sub>🐈</sub>](http://gyoudmon.org/~wargrey:DigiGnome)Tamer's Handbook: Digimon-Gnome

> _Translating is the most complex human activity in the Universe._

This _handbook_ shows my _[Programming
Methodology](https://github.com/digital-world/DigiGnome)_ that the
entire project should follow. Hmm... nonetheless, in fact it's all right
to forget this sample after reading due to the complexity of the real
world problems.

Good Luck!

---

> + 📚Behaviors and Features
>     + 📖
[infrastructure.rkt](http://gyoudmon.org/~wargrey:DigiGnome/infrastructure.rkt)
>       + 📑Ready? It works!
>         + 📑make: simple options
>           + 📑make --silent --help
>             - 💚 1 - should exit normally
>             - 💚 2 - should keep quiet
>           + 📑make --silent love
>             - 💚 1 - should exit abnormally
>             - 💚 2 - should report errors
>         + 📑make: complex options
>           - 💔 1 - make --always-make ++only DigiGnome README.md
>           - 💚 2 - make --dry-run --touch ++only gnome
>       + 📑Rules serve you!
>         + 📑info.rkt settings
>           + 📑/info.rkt
>             - 💚 1 - multi
>           + 📑/DigiGnome/info.rkt
>             - 💚 1 - version
>             - 💚 2 - collection
>             - 💚 3 - compile-omit-paths
>             - 💚 4 - test-omit-paths
>           + 📑/Kuzuhamon/info.rkt
>             - 💚 1 - version
>             - 💚 2 - collection
>             - 💚 3 - compile-omit-paths
>             - 💚 4 - test-omit-paths
>           + 📑/gnome/info.rkt
>             - 💚 1 - version
>             - 💚 2 - collection
>             - 💚 3 - compile-omit-paths
>             - 💚 4 - test-omit-paths
>           + 📑/nanomon/info.rkt
>             - 💚 1 - version
>             - 💚 2 - collection
>             - 💚 3 - compile-omit-paths
>             - 💚 4 - test-omit-paths
>           + 📑/sakuyamon/info.rkt
>             - 💚 1 - version
>             - 💚 2 - collection
>             - 💚 3 - compile-omit-paths
>             - 💚 4 - test-omit-paths
>         + 📑README.md dependencies
>           + 📑/DigiGnome/readme.md
>             - 💚 1 - DigiGnome/tamer/handbook.scrbl
>           + 📑/Kuzuhamon/readme.md
>             - 💚 1 - Kuzuhamon/tamer/handbook.scrbl
>           + 📑/gnome/readme.md
>             - 💚 1 - gnome/tamer/handbook.scrbl
>           + 📑/nanomon/readme.md
>             - 💚 1 - nanomon/tamer/handbook.scrbl
>           + 📑/sakuyamon/readme.md
>             - 💚 1 - sakuyamon/tamer/handbook.scrbl
>         + 📑infrastructure specifications
>           + 📑/DigiGnome/tamer
>             - 💚 1 - ./robots.txt
>           + 📑/Kuzuhamon/tamer
>             - 💚 1 - ./robots.txt
>           + 📑/gnome/tamer
>             - 💚 1 - ./robots.txt
>           + 📑/nanomon/tamer
>             - 💚 1 - ./robots.txt
>           + 📑/sakuyamon/tamer
>             - 💚 1 - ./robots.txt
>     + 📖
[digivice.rkt](http://gyoudmon.org/~wargrey:DigiGnome/digivice.rkt)
>       + 📑Make the demo from scratch
>         - 💚 1 - digivice should be updated!
>         - 💚 2 - action should be updated!
>         - 💚 3 - exec racket digivice
>       + 📑That's it, Help!
>         + 📑digivice \[action\]
>           - 💚 1 - digivice help \['help' can be omitted if you want\]
>           - 💚 2 - digivice --help \[a kind of mistyped action\]
>           - 💚 3 - digivice action \[mission start\]
>         + 📑digivice action \[option\]
>           - 💚 1 - digivice action --help \[pass option to action\]
>           - 💚 2 - digivice action --version \[show version
information\]
>           - 💚 3 - digivice action --unknown \[a kind of mistyped
option\]
>           - 💚 4 - digivice action job done
>       + 📑Restore the filesystem
>         - 💚 1 - digivice should be deleted!
>         - 💚 2 - actions directory should be deleted recursively!
>         - 💚 3 - /DigiGnome/digivice should be deleted if empty!
>
> 📌50 examples, 1 failure, 0 errors, 98.00% Okay.
