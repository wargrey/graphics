#lang typed/racket

(define-type Term-Color-Exp (Option (U String Byte)))

(require/typed/provide "runtime.rkt"
                       [/dev/null Output-Port]
                       [current-digimon (Parameterof Path-String)]
                       [digimon-world (Parameterof Path-String)]
                       [digimon-gnome (Parameterof Path-String)]
                       [digimon-zone (Parameterof Path-String)]
                       [digimon-digivice (Parameterof Path-String)]
                       [digimon-digitama (Parameterof Path-String)]
                       [digimon-stone (Parameterof Path-String)]
                       [digimon-tamer (Parameterof Path-String)]
                       [digimon-terminus (Parameterof Path-String)]
                       
                       [find-digimon-files {{Path-String -> Boolean} Path-String -> (Listof Path-String)}]
                       [echof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}]
                       [eechof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}])
