#lang at-exp typed/racket

(provide (all-defined-out))

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))

(define-type (Identity Type) Type)
(define-type Info-Ref (->* [Symbol] [(-> Any)] Any))

(require/typed/provide setup/getinfo
                       [get-info/full (-> Path-String
                                          [#:namespace (Option Namespace)]
                                          [#:bootstrap? Any]
                                          (Option Info-Ref))])

(require/typed racket/class
               [#:struct (exn:fail:object exn:fail) ()
                #:extra-constructor-name make-exn:fail:object])

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     #'(require/typed/provide/batch modpath [id ...] type-definition)]
    [(_ modpath [id ...] type-definition)
     #'(require/typed/provide modpath [id type-definition] ...)]))

(define-syntax (#%full-module stx)
  #'(let ([rmp (variable-reference->resolved-module-path (#%variable-reference))])
      (resolved-module-path-name (cast rmp Resolved-Module-Path))))

(define-syntax (#%file stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) full]
            [else (with-handlers ([exn:fail:contract? (const (current-directory))])
                    ((inst car Path (Listof Symbol)) (cast full (Pairof Path (Listof Symbol)))))])))

(define-syntax (#%module stx)
  #'(let ([full (ann (#%full-module) (U Symbol Path))])
      (cond [(path? full) ((compose1 string->symbol path->string)
                           (path-replace-extension (cast (file-name-from-path full) Path) ""))]
            [else (with-handlers ([exn:fail:contract? (λ _ '<anonymous>)])
                    (last (cdr (cast full (Pairof (U Path Symbol) (Listof Symbol))))))])))

(define-syntax (throw stx)
  (syntax-case stx []
    [(_ [st-id st-argl ...] message)
     #'(raise (st-id message (current-continuation-marks) st-argl ...))]
    [(_ [st-id st-argl ...] msgfmt fmtargl ...)
     #'(throw [st-id st-argl ...] (format msgfmt fmtargl ...))]
    [(_ st-id message)
     #'(throw [st-id] message)]
    [(_ st-id msgfmt message ...)
     #'(throw [st-id] (format msgfmt message ...))]))

(define-syntax (rethrow stx)
  (syntax-case stx []
    [(_ [st-id st-argl ...] prefix)
     #'(lambda [[src : exn]]
         (throw [st-id st-argl ...] (~a prefix #\: #\space (exn-message src))))]
    [(_ [st-id st-argl ...] fmt argl ...)
     #'(rethrow [st-id st-argl ...] (format fmt argl ...))]
    [(_ st-id prefix)
     #'(rethrow [st-id] prefix)]
    [(_ st-id fmt argl ...)
     #'(rethrow [st-id] (format fmt argl ...))]))

(define-syntax (defconsts stx)
  (syntax-case stx [:]
    [(_ : Type [id val] ...)
     #'(begin (define id : Type val) ...)]))

(define-syntax (define-type/enum stx)
  (syntax-case stx [: quote]
    [(_ id : TypeU (quote enum) ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU [enum comments ...] ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU enum ...)
     (with-syntax ([TypeU* (format-id #'TypeU "~a*" (syntax-e #'TypeU))])
     #'(begin (define-type TypeU (U 'enum ...))
              (define-type TypeU* (Listof TypeU))
              (define id : TypeU* (list 'enum ...))))]))

;; prefab structures cannot be converted to contract in typed racket;
;; transparent structures are not allowed as place message.
(define-type UInt32 Nonnegative-Fixnum)  ; this is a bit smaller than uint32
(define-type UInt64 Nonnegative-Integer) ; this is larger than uint64
(define-type MPInteger Integer)
(define-type (nBytes n) Bytes)           ; the length is prefixed when n is String

(define-type Primitive-Type (Rec PT (U Symbol (List 'Listof PT) (List 'nBytes (U Natural 'String 'Bytes)))))

(define-syntax (define-type/consts stx)
  (syntax-case stx [: of as]
    [(_ cs : TypeU of Type (const val comments ...) ...)
     (with-syntax ([$%cs (format-id #'cs "$%~a" (syntax-e #'cs))]
                   [$#cs (format-id #'cs "$#~a" (syntax-e #'cs))])
       #'(begin (define-type TypeU (U 'const ...))
                (define $#cs : (-> TypeU Type)
                  (let ([cs : (HashTable TypeU Type) ((inst make-immutable-hasheq TypeU Type) (list (cons 'const val) ...))])
                    (lambda [sym] ((inst hash-ref TypeU Type Type) cs sym))))
                (define $%cs : (-> Type (Option TypeU))
                  (let ([cs : (HashTable Type TypeU) ((inst make-immutable-hasheq Type TypeU) (list (cons val 'const) ...))])
                    (lambda [v] ((inst hash-ref Type TypeU False) cs v (lambda [] #false)))))))]
    [(_ cs : TypeU of Type as parent (const val ([field : DataType] ...)) ...)
     (with-syntax* ([$*cs (format-id #'cs "$*~a" (syntax-e #'cs))]
                    [$:cs (format-id #'cs "$:~a" (syntax-e #'cs))]
                    [?parent (format-id #'parent "?~a" (syntax-e #'parent))])
       #'(begin (define-type/consts cs : TypeU of Type (const val) ...)
                (struct parent () #:prefab)
                (struct ?parent parent ([id : Type]) #:prefab)
                (struct const parent ([field : DataType] ...) #:prefab) ...
                (define $*cs : (-> (U TypeU Type) (Listof Any) parent)
                  ;;; use `val` instead of `const` does not work.
                  (lambda [sym argl] (case sym [(const) (apply const (cast argl (List DataType ...)))] ... [else (?parent (cast sym Type))])))
                (define $:cs : (-> TypeU (Listof Primitive-Type))
                  (let ([cs : (HashTable TypeU (Listof Primitive-Type))
                         ((inst make-immutable-hasheq TypeU (Listof Primitive-Type))
                          (list (cons 'const (list 'DataType ...)) ...))])
                    (lambda [sym] ((inst hash-ref TypeU (Listof Primitive-Type) (Listof Primitive-Type)) cs sym))))))]))

(define-syntax (define/extract-λref stx)
  (syntax-case stx [:-]
    [(_ ref :- (make Typeof) defines ...)
     (with-syntax ([(extract ...)
                    (for/list ([def-idl (in-list (syntax->list #'(defines ...)))])
                      (syntax-case def-idl [: = =>]
                        [([renamed-id key] : Type = def-exp => [fvalue ...])
                         #'(define renamed-id : (Typeof Type)
                             (make (match (ref 'key (thunk def-exp)) fvalue ...)))]
                        [([renamed-id key] : Type => [fvalue ...] = def-exp)
                         #'(define renamed-id : (Typeof Type)
                             (make (with-handlers ([exn:misc:match? (const def-exp)])
                                     (match (ref 'key void) fvalue ...))))]
                        [([renamed-id key] : Type => [fvalue ...])
                         #'(define renamed-id : (Typeof Type)
                             (make (match (ref 'key void) fvalue ...)))]
                        [([renamed-id key] : Type = def-exp)
                         #'(define renamed-id : (Typeof Type)
                             (make (cast (ref 'key (thunk def-exp)) Type)))]
                        [([renamed-id key] : Type)
                         #'(define renamed-id : (Typeof Type)
                             (make (cast (ref 'key) Type)))]
                        [(id : Type = def-exp => [fvalue ...])
                         #'(define id : (Typeof Type)
                             (make (match (ref 'id (thunk def-exp)) fvalue ...)))]
                        [(id : Type => [fvalue ...] = def-exp)
                         #'(define id : (Typeof Type)
                             (make (with-handlers ([exn:misc:match? (const def-exp)])
                                     (match (ref 'key void) fvalue ...))))]
                        [(id : Type => [fvalue ...])
                         #'(define id : (Typeof Type)
                             (make (match (ref 'id void) fvalue ...)))]
                        [(id : Type = def-exp)
                         #'(define id : (Typeof Type)
                             (make (cast (ref 'id (thunk def-exp)) Type)))]
                        [(id : Type)
                         #'(define id : (Typeof Type)
                             (make (cast (ref 'id) Type)))]))])
       #'(begin extract ...))]
    [(_ ref defines ...)
     #'(define/extract-λref ref :- (values Identity) defines ...)]))

(define-syntax (define/extract-symtable stx)
  (syntax-case stx []
    [(_ (symtable-sexp ...) rest ...)
     (with-syntax ([symtable (format-id #'symtable "~a" (gensym 'symboltable))])
       #'(begin (define symtable : (HashTable Symbol Any) (symtable-sexp ...))
                (define/extract-symtable symtable rest ...)))]
    [(_ symtable rest ...)
     (with-syntax ([table-ref (format-id #'symtable "~a" (gensym 'symtable))])
       #'(begin (define table-ref : (->* (Symbol) ((Option (-> Any))) Any)
                  (lambda [key [defval #false]]
                    (hash-ref symtable key (or defval (thunk (error 'table-ref "no such key found in this symbol table: ~a" key))))))
                (define/extract-λref table-ref rest ...)))]))

(define-syntax (define/extract-info stx)
  (syntax-case stx []
    [(_ infodir rest ...)
     (with-syntax ([info-ref (format-id #'info-ref "~a" (gensym 'inforef))])
       #'(begin (define info-ref : Info-Ref
                  (let ([ref (get-info/full infodir #:bootstrap? #true)])
                    (if (false? ref) (throw [exn:fail:filesystem] "info.rkt not found in ~a" infodir) ref)))
                (define/extract-λref info-ref rest ...)))]))

(define-syntax (define/abstract stx)
  (syntax-case stx []
    [(_ (method-id args ...))
     #'(define/public (method-id args ...)
         (throw exn:fail:object
                "~a@~a is an abstract method!"
                'method-id
                (object-name this)))]))

(define-syntax (match/handlers stx)
  (syntax-case stx [:]
    [(_ s-exp : type? match-clause ...)
     #'(match (with-handlers ([exn? values]) s-exp)
         match-clause ...
         [(? type? no-error-value) no-error-value])]
    [(_ s-exp match-clause ...)
     #'(match (with-handlers ([exn? values]) s-exp)
         match-clause ...
         [escaped-value escaped-value])]))
