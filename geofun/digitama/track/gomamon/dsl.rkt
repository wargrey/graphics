#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../primitives.rkt")

(require "primitive.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE:
; The fundemnatal semantics of each move is that "move and produce a node or a structure",
; so, the `#:fork` is a noun, meaning "the next move produces a tree structure",
;   rather then "split the continuation now".
(define-syntax (gomamon-dsl stx)
  (syntax-parse stx #:datum-literals [=> <- -- let where]
    [(_ self [(~or #:tree #:fork)
              move-expr
              [(~or => #:=>) submove-expr ...] ...])
     (quasisyntax/loc stx
       (begin (gomamon-dsl self move-expr)
              (gomamon-dsl self [=> submove-expr ...])
              ...))]
    [(_ self [#:seq [move-expr (~optional (~or (~seq (~or => #:=>) submove-expr ...)
                                               [(~or => #:=>) submove-expr ...])
                                          #:defaults [((submove-expr 1) null)])] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (gomamon-dsl self move-expr)
                (gomamon-dsl self [=> submove-expr ...]))
              ...))]
    [(_ self [#:jump [pos-expr (~optional (~or (~seq (~or => #:=>) submove-expr ...)
                                               [(~or => #:=>) submove-expr ...])
                                          #:defaults [((submove-expr 1) null)])] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (geo-track-jump-to self pos-expr gomamon-grid-position)
                (gomamon-dsl self submove-expr) ...)
              ...))]
    [(_ self [#:zone id
              (~optional type:id #:defaults ([type #'#false]))
              (~alt (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))
                    (~optional (~seq #:options options) #:defaults ([options #'null])))
              ...
              internal-move:expr ...])
     (quasisyntax/loc stx
       (parameterize ([current-rubber-zone (geo-create-rubber-zone! self id 'type desc options)])
         (gomamon-dsl self internal-move) ...))]
    [(_ self [#:with-zone id internal-move:expr ...])
     (quasisyntax/loc stx
       (parameterize ([current-rubber-zone (geo-rubber-zone-ref self id)])
         (gomamon-dsl self internal-move) ...))]
    [(_ self [(~or => #:=>) submove-expr ...])
     (with-syntax ([here (gensym 'goma:dsl:)])
       (quasisyntax/loc stx
         (let ([here (geo:track-here self)])
           (gomamon-dsl self submove-expr) ...
           (geo-track-jump-to-position self here))))]
    [(_ self ((~or let where #:let #:where) (let-expr ...) move-expr ...))
     (syntax/loc stx (let (let-expr ...) (gomamon-dsl self move-expr) ...))]
    [(_ self (move:id argl ...))
     (with-syntax ([goma-move! (format-id #'move "gomamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (goma-move! self argl ...)))]))
