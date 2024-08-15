#lang scribble/manual

@(require digimon/tamer)
@(require geofun/version)

@(require (for-label typed/racket/base))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-typed-module-story[bitmap/misc]{Miscellaneous}

@handbook-scenario{Versions}

@deftogether[(@defproc[(cairo-version) Integer]
               @defproc[(cairo-version-string) String])]{
 Get the version info of @cite{Cairo} shipped with Racket.
}

@deftogether[(@defproc[(pango-version) Integer]
               @defproc[(pango-version-string) String])]{
 Get the version info of @cite{Pango} shipped with Racket.
}

@handbook-reference[#:auto-hide? #true]
