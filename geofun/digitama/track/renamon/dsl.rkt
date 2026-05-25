#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")
(require "../self.rkt")
(require "../primitives.rkt")

(require "../../self.rkt")

(require "primitive.rkt")

(require digimon/measure)
(require racket/format)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(require (for-syntax racket/string))
(require (for-syntax racket/symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE:
; The fundemnatal semantics of each move is that "move and produce a node or a structure",
; so, the `#:fork` is a noun, meaning "the next move produces a tree structure",
;   rather then "split the continuation now".
(define-syntax (renamon-dsl stx)
  (syntax-parse stx #:datum-literals [=> +> ~> @ ~ ~@ @~ let where]
    [(_ self [(~or #:tree #:fork)
              move-expr
              [(~and (~or => #:=> +> #:+>) >) submove-expr ...] ...])
     (quasisyntax/loc stx
       (begin (renamon-dsl self move-expr)
              (renamon-dsl self [> submove-expr ...])
              ...))]
    [(_ self [(~or #:seq #:linear)
              [move-expr (~optional (~or (~seq (~and (~or => #:=> +> #:+>) >) submove-expr ...)
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
    [(_ self [(~or #:weight #:~> ~>) [p%:nat (~or #:= #:-) submove-expr ...] ...])
     (with-syntax ([(P accum% ...) (let ([ns (syntax->list #'(p% ...))])
                                     (let accum ([sum 0] [sums null] [addends ns])
                                       (if (pair? addends)
                                           (let* ([<addend> (car addends)]
                                                  [sum++ (+ sum (syntax-e <addend>))])
                                             (accum sum++ (cons (datum->syntax <addend> sum++) sums) (cdr addends)))
                                           (cons sum (reverse sums)))))])
       (quasisyntax/loc stx
         (begin (let ([p (if (<= P 0) 0 (random 0 P))])
                  (cond [(< p accum%) (renamon-dsl self submove-expr) ...]
                        ...)))))]
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
    [(_ self ((~or let where #:let #:where) (let-expr ...) move-expr ...))
     (syntax/loc stx (let (let-expr ...) (renamon-dsl self move-expr) ...))]
    [(_ self (move:id argl:expr ... (~or ~ #:~) (~or @ #:@) (~optional avatar:expr #:defaults ([avatar #'(void)]))))
     (syntax/loc stx (renamon-dsl self (move argl ... #:~ (current-renamon-ribbon) #:@ avatar)))]
    [(_ self (move:id argl:expr ... (~or ~@ #:~@) (~optional avatar:expr #:defaults ([avatar #'(void)]))))
     (syntax/loc stx (renamon-dsl self (move argl ... #:~ (current-renamon-ribbon) #:@ avatar)))]
    [(_ self (move:id argl:expr ... (~or @ #:@) (~or ~ #:~) (~optional ribbon:expr #:defaults ([ribbon #'(current-renamon-ribbon)]))))
     (syntax/loc stx (renamon-dsl self (move argl ... #:@ (void) #:~ ribbon)))]
    [(_ self (move:id argl:expr ... (~or @~ #:@~) (~optional ribbon:expr #:defaults ([ribbon #'(current-renamon-ribbon)]))))
     (syntax/loc stx (renamon-dsl self (move argl ... #:@ (void) #:~ ribbon)))]
    [(_ self (move:id argl:expr ... (~or ~ #:~) ribbon:expr (~or @ #:@) avatar:expr ...))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx ; order matters
         (let ([prev (geo:track-here self)])
           (rena-move! self argl ...)
           (renamon-evolve self avatar ...)
           (renamon-pave self prev (geo:track-here self) ribbon))))]
    [(_ self (move:id argl:expr ... (~or @ #:@) avatar:expr (~or ~ #:~) ribbon:expr ...))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx ; order matters
         (let ([prev (geo:track-here self)])
           (rena-move! self argl ...)
           (renamon-pave self prev (geo:track-here self) ribbon ...)
           (renamon-evolve self avatar))))]
    [(_ self (move:id argl:expr ... (~or @ #:@) (~optional avatar:expr #:defaults ([avatar #'(void)]))))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (begin (rena-move! self argl ...)
                (renamon-evolve self avatar))))]
    [(_ self (move:id argl:expr ... (~or ~ #:~) (~optional ribbon:expr #:defaults ([ribbon #'(current-renamon-ribbon)]))))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (let ([here (geo:track-here self)])
           (rena-move! self argl ...)
           (renamon-pave self here (geo:track-here self) ribbon))))]
    [(_ self (move:id argl ...))
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (rena-move! self argl ...)))]
    [(_ self move:id)
     (with-syntax ([rena-move! (format-id #'move "renamon-~a!" (syntax->datum #'move))])
       (quasisyntax/loc stx
         (rena-move! self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-renamon-rule! stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ name:id (~optional (~seq #:with [(argv : Type defval ...) ...])
                           #:defaults ([(argv 1) null]
                                       [(Type 1) null]
                                       [(defval 2) null]))
        (~alt (~optional (~seq #:make-ribbon make-ribbon) #:defaults ([make-ribbon #'void]))
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:anchor-abbr abbr) #:defaults ([abbr #'#false])))
        ...
        #:= rule-expand:expr ...
        (~optional (~seq #:- terminate:expr ...) #:defaults ([(terminate 1) null])))
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (begin (define (make-name-ribbon [argv : Type] ...) : Renamon-Ribbon
                  (λ [[start : Float-Complex] [end : Float-Complex]]
                    (make-ribbon argv ... start end)))
                
                (define (rena-move! [self : Renamon] [argv : Type defval ...] ...) : Void
                  (define order (min (default-renamon-order) (default-renamon-terminal-order)))
                  
                  (parameterize* ([current-renamon-anchor-format fmt]
                                  [current-renamon-primitive-name (renamon-anchor-prefix (or abbr 'name))]
                                  [current-renamon-ribbon (if (eq? make-ribbon void) (void) (make-name-ribbon argv ...))])
                    (if (> order 0)
                        (parameterize ([default-renamon-order (- order 1)])
                          (renamon-dsl self rule-expand) ... (void))
                        (begin (renamon-dsl self terminate) ... (void))))))))]
    [(_ name:id #:with [(argv : Type defval ...) ...]
        (~alt (~optional (~seq #:make-ribbon make-ribbon) #:defaults ([make-ribbon #'void]))
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:anchor-abbr abbr) #:defaults ([abbr #'#false])))
        ...
        [pred:expr (~optional (~or #:= #:-)) rule-expand:expr ...] ...
        (~optional (~seq #:- terminate:expr ...) #:defaults ([(terminate 1) null])))
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (begin (define (make-name-ribbon [argv : Type] ...) : Renamon-Ribbon
                  (λ [[start : Float-Complex] [end : Float-Complex]]
                    (make-ribbon argv ... start end)))
                
                (define (rena-move! [self : Renamon] [argv : Type defval ...] ...) : Void
                  (parameterize* ([current-renamon-anchor-format fmt]
                                  [current-renamon-primitive-name (renamon-anchor-prefix (or abbr 'name))]
                                  [current-renamon-ribbon (if (eq? make-ribbon void) (void) (make-name-ribbon argv ...))])
                    (cond [pred (renamon-dsl self rule-expand) ... (void)]
                          ...
                          [else (renamon-dsl self terminate) ... (void)]))))))]
    [(_ name:id (~optional (~seq #:with [(argv : Type defval ...) ...])
                           #:defaults ([(argv 1) null]
                                       [(Type 1) null]
                                       [(defval 2) null]))
        (~alt (~optional (~seq #:make-ribbon make-ribbon) #:defaults ([make-ribbon #'void]))
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:anchor-abbr abbr) #:defaults ([abbr #'#false])))
        ...
        #:- move-expr ...)
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (begin (define (make-name-ribbon [argv : Type] ...) : Renamon-Ribbon
                  (λ [[start : Float-Complex] [end : Float-Complex]]
                    (make-ribbon argv ... start end)))
                
                (define (rena-move! [self : Renamon] argv ...) : Void
                  (parameterize* ([current-renamon-anchor-format fmt]
                                  [current-renamon-primitive-name (renamon-anchor-prefix (or 'abbr 'name))]
                                  [current-renamon-ribbon (if (eq? make-ribbon void) (void) (make-name-ribbon argv ...))])
                    (renamon-dsl self move-expr) ...
                    (void))))))]))

(define-syntax (define-renamon-generator! stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ id (~optional (~seq #:with [(argv-expr : Type defval ...) ...])
                      #:defaults ([(argv-expr 1) null]
                                  [(Type 1) null]
                                  [(defval 2) null]))
        (~alt (~optional (~seq #:desc description) #:defaults ([description #'#false]))
              (~optional (~seq #:order n:nat) #:defaults ([n #'(default-renamon-order)]))
              (~optional (~seq #:closed? closed?) #:defaults ([closed? #'#false]))
              (~optional (~seq #:terminal-order terminal-order:nat) #:defaults ([terminal-order #'(default-renamon-terminal-order)]))
              (~optional (~seq #:angle delta) #:defaults ([delta #'(default-renamon-angle)])))
        ...
        #:- move-expr ...)
     (with-syntax ([id! (format-id #'id "~a!" (syntax->datum #'id))]
                   [id: (format-id #'id "~a:" (syntax->datum #'id))]
                   [id*! (format-id #'id "~a*!" (syntax->datum #'id))]
                   [fallback-desc (datum->syntax #'id (string-titlecase (string-replace (symbol->immutable-string (syntax-e #'id)) #px"[-_]" " ")))])
       (syntax/loc stx
         (begin (define (id! #:order [order : Integer n]
                             #:angle [angle : Flonum delta]
                             [self : Renamon] (argv-expr : Type defval ...) ...) : Renamon
                  (parameterize ([default-renamon-order order]
                                 [default-renamon-angle angle]
                                 [default-renamon-terminal-order terminal-order])
                    (renamon-dsl self move-expr) ...)
                  (when closed? (geo-track-close self))
                  self)
                
                (define (id*! #:order [order : Integer n]
                              #:angle [angle : Flonum delta]
                              #:id [name : (Option Symbol) #false]
                              #:desc [desc : (Option String) #false]
                              #:desc-format [desc-fmt : String (default-renamon-description-format)]
                              #:frame [frame : Geo-Frame-Datum #false]
                              #:trusted-anchors [trusted-anchors : (Option Geo-Trusted-Anchors) #false]
                              #:truncate? [truncate? : Boolean #true]
                              #:anchor->sticker [anchor->sticker : Geo-Track-Anchor->Sticker void]
                              [self : Renamon] (argv-expr : Type defval ...) ...) : Geo:Trail
                  (geo-track-stick #:id (or name (gensym 'id:)) #:frame frame
                                   #:desc (format desc-fmt
                                            (or desc description (geo-desc self) fallback-desc)
                                            (min order terminal-order)
                                            (~r (~deg (or (renamon-angle-delta self) angle) 'rad) #:precision 1))
                                   #:trusted-anchors trusted-anchors #:truncate? truncate?
                                   (id! self argv-expr ... #:order order #:angle angle)
                                   anchor->sticker)))))]))
