#lang typed/racket

(provide (all-defined-out))

(define-type Term-Color-Exp (Option (U String Byte)))
(define-type Racket-Main {String * -> Void})

(require/typed/provide "digicore.rkt"
                       [:house-garden: Char]
                       [:cat: Char]
                       [:paw: Char]
                       [:macroscope: Char]
                       [:telescope: Char]
                       [:book: Char]
                       [:books: Char]
                       [:open-book: Char]
                       [:memo: Char]
                       [:page: Char]
                       [:bookmark: Char]
                       [:heart: Char]
                       [:broken-heart: Char]
                       [:bomb: Char]
                       [:collision: Char]
                       [:pin: Char]
                       [:crystal-ball: Char]
                       [:backhand: Char]
                       
                       [/dev/stdin Input-Port]
                       [/dev/stdout Output-Port]
                       [/dev/stderr Output-Port]
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
                       [call-as-normal-termination {{-> Any} -> Void}]
                       [~n_w {Nonnegative-Integer String -> String}]
                       [~w=n {Nonnegative-Integer String -> String}]
                       [echof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}]
                       [eechof {String [#:fgcolor Term-Color-Exp] [#:bgcolor Term-Color-Exp] [#:attributes (Listof Symbol)] Any * -> Void}])
