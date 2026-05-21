#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../primitives.rkt")

(require "primitive.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (renamon-dsl stx)
  (syntax-parse stx #:datum-literals [=> +> @ ~]
    [(_ self [(~or #:tree #:fork #:cond #:case)
              move-expr
              [(~optional (~and (~or => #:=> +> #:+>) >) #:defaults ([> #'=>])) submove-expr ...] ...])
     (quasisyntax/loc stx
       (begin (renamon-dsl self move-expr)
              (renamon-dsl self [> submove-expr ...])
              ...))]
    [(_ self [#:seq [move-expr (~optional (~or (~seq (~and (~or => #:=> +> #:+>) >) submove-expr ...)
                                               [(~and (~or => #:=> +> #:+>) >) submove-expr ...])
                                          #:defaults ([(submove-expr 1) null]
                                                      [> #'=>]))] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (renamon-dsl self move-expr)
                (renamon-dsl self [> submove-expr ...]))
              ...))]
    [(_ self [#:jump [pos-expr (~optional (~or (~seq (~and (~or => #:=> +> #:+>) >) submove-expr ...)
                                               [(~and (~or => #:=> +> #:+>) >) submove-expr ...])
                                          #:defaults ([(submove-expr 1) null]
                                                      [> #'=>]))] ...])
     (quasisyntax/loc stx
       (begin (let ()
                (geo-track-jump-to self pos-expr #false renamon-position)
                [> (renamon-dsl self submove-expr) ...])
              ...))]
    [(_ self [#:zone id type
              (~alt (~optional (~seq #:anchor anchor) #:defaults ([anchor #''ct]))
                    (~optional (~seq #:desc desc) #:defaults ([desc #'#false]))
                    (~optional (~seq #:stereotype sotype) #:defaults ([sotype #'#false])))
              ...
              [internal-move-expr ...]])
     (quasisyntax/loc stx
       (parameterize ([current-flex-zone (geo-create-flex-zone! self id type desc anchor sotype)])
         (renamon-dsl self internal-move-expr) ...))]
    [(_ self [#:with-zone id [internal-move-expr ...]])
     (quasisyntax/loc stx
       (parameterize ([current-flex-zone (geo-flex-zone-ref self id)])
         (renamon-dsl self internal-move-expr) ...))]
    [(_ self [(~or => #:=>) submove-expr ...])
     (with-syntax ([here (gensym 'rena:dsl:)]
                   [info (gensym 'rena:dsl:)])
       (quasisyntax/loc stx
         (let ([here (geo:track-here self)]
               [info (unbox (renamon-box self))])
           (renamon-dsl self submove-expr) ...
           (geo-track-jump-to-position self here)
           (set-box! (renamon-box self) info))))]
    [(_ self [(~or +> #:+>) submove-expr ...])
     (with-syntax ([here (gensym 'rena:dsl:)])
       (quasisyntax/loc stx
         (let ([here (geo:track-here self)])
           (renamon-dsl self submove-expr) ...
           (geo-track-jump-to-position self here))))]
    [(_ self (move:id argl ... (~or @ ~ #:~ #:@ #:avatar) (~optional avatar #:defaults ([avatar #'(void)]))))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (begin (rena-move! self argl ...)
                (renamon-evolve self avatar))))]
    [(_ self (move:id argl ...))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (rena-move! self argl ...)))]
    [(_ self move:id)
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (rena-move! self)))]))

