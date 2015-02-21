#lang typed/racket/base

(define-type Term-Color-Exp (Option (U String Byte)))

(require/typed/provide "runtime.rkt"
                       [/dev/null Output-Port]
                       [digimon-world String]
                       [digimon-gnome String]

                       [digimon-setenv {Path-String -> Void}]
                       [digimon-path {Path-String [#:digimon Path-String] -> Path}]
                       [find-digimon-files {{Path-String -> Boolean} Path-String -> (Listof Path)}]
                       [echof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}]
                       [eechof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}])
