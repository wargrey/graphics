#lang typed/racket/base

(provide (all-defined-out))

(require "gomamon.rkt")
(require "../dc/path.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (gomamon-dsl stx)
  (syntax-parse stx #:datum-literals [=> <- --]
    [(_ self [#:seq [move-expr (~optional (~or (~seq (~or => #:=>) submove-expr ...)
                                               [(~or => #:=>) submove-expr ...])
                                          #:defaults [((submove-expr 1) null)])] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (gomamon-dsl self move-expr)
                (gomamon-dsl self [=> submove-expr ...]))
              ...))]
    [(_ self [#:tree move-expr [(~or -- <- => #:-- #:<- #:=>) submove-expr ...] ...])
     (quasisyntax/loc stx
       (begin (gomamon-dsl self move-expr)
              (gomamon-dsl self [=> submove-expr ...])
              ...))]
    [(_ self [#:jump [pos-expr (~optional (~or (~seq (~or => #:=>) submove-expr ...)
                                               [(~or => #:=>) submove-expr ...])
                                          #:defaults [((submove-expr 1) null)])] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (geo-path-jump-to self pos-expr #false)
                (gomamon-dsl self submove-expr) ...)
              ...))]
    [(_ self [(~or => #:=>) submove-expr ...])
     (with-syntax ([here (gensym 'goma:dsl:)])
       (quasisyntax/loc stx
         (let ([here (geo:path-here self)])
           (gomamon-dsl self submove-expr) ...
           (geo-path-jump-to self here))))]
    [(_ self (move argl ...))
     (with-syntax ([goma-move! (format-id #'move "gomamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (goma-move! self argl ...)))]))
