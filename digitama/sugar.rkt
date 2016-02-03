#lang at-exp typed/racket

(provide (except-out (all-defined-out) #%full-module))

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))

(define-type Info-Ref (->* [Symbol] [(-> Any)] Any))

(require/typed/provide setup/getinfo
                       [get-info/full (-> Path-String
                                          [#:namespace (Option Namespace)]
                                          [#:bootstrap? Any]
                                          (Option Info-Ref))])

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
                           (path-replace-suffix (cast (file-name-from-path full) Path) ""))]
            [else (with-handlers ([exn:fail:contract? (λ _ '<anonymous>)])
                    (last (cdr (cast full (Pairof (U Path Symbol) (Listof Symbol))))))])))

(define-syntax (require/typed/provide/batch stx)
  (syntax-case stx [id:]
    [(_ modpath [id: id ...] type-definition)
     #'(require/typed/provide/batch modpath [id ...] type-definition)]
    [(_ modpath [id ...] type-definition)
     #'(require/typed/provide modpath [id type-definition] ...)]))

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
    [(_ [st-id st-argl ...] message)
     #'(lambda [[src : exn]]
         (throw [st-id st-argl ...] (~a message #\: #\space (exn-message src))))]
    [(_ [st-id st-argl ...] fmt argl ...)
     #'(rethrow [st-id st-argl ...] (format fmt argl ...))]
    [(_ st-id fmt message)
     #'(rethrow [st-id] message)]
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

(define-syntax (define-strdict stx)
  (syntax-case stx [:]
    [(_ id : Type)
     #'(define-strdict id : Type null)]
    [(_ id : Type init-vals)
     (with-syntax ([%id  (format-id #'id "%~a"  (syntax-e #'id))]  ; make-hash
                   [$id  (format-id #'id "$~a"  (syntax-e #'id))]  ; hash-ref
                   [$id? (format-id #'id "?~a"  (syntax-e #'id))]  ; hash-has-key?
                   [$id# (format-id #'id "$~a#" (syntax-e #'id))]  ; hash-count
                   [$id@ (format-id #'id "$~a@" (syntax-e #'id))]  ; hash-keys
                   [$id* (format-id #'id "$~a*" (syntax-e #'id))]  ; hash-values
                   [$id+ (format-id #'id "$~a+" (syntax-e #'id))]  ; hash-ref!
                   [$id- (format-id #'id "$~a-" (syntax-e #'id))]) ; hash-remove!
       #'(begin (define %id : (HashTable String Type) ((inst make-hash String Type) init-vals))
                (define ($id@) : (Listof String) ((inst hash-keys String Type) %id))
                (define ($id*) : (Listof Type) ((inst hash-values String Type) %id))
                (define ($id#) : Index ((inst hash-count String Type) %id))
                (define ($id? [key : String]) : Boolean (hash-has-key? %id key))
                (define ($id- [key : String]) : Void ((inst hash-remove! String Type) %id key))
                (define $id+ : (-> String (-> Type) Type)
                  (lambda [key setval]
                    ((inst hash-ref! String Type) %id key setval)))
                (define $id : (case-> [String -> Type] [String (-> Type) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref String Type Type) %id key)]
                    [(key defval) ((inst hash-ref String Type Type) %id key defval)]))))]))

(define-syntax (define-symdict stx)
  (syntax-case stx [:]
    [(_ id : Type)
     #'(define-symdict id : Type null)]
    [(_ id : Type init-vals)
     (with-syntax ([%id  (format-id #'id "%~a"  (syntax-e #'id))]  ; make-hasheq
                   [$id  (format-id #'id "$~a"  (syntax-e #'id))]  ; hash-ref
                   [$id? (format-id #'id "?~a"  (syntax-e #'id))]  ; hash-has-key?
                   [$id# (format-id #'id "$~a#" (syntax-e #'id))]  ; hash-count
                   [$id@ (format-id #'id "$~a@" (syntax-e #'id))]  ; hash-keys
                   [$id* (format-id #'id "$~a*" (syntax-e #'id))]  ; hash-values
                   [$id+ (format-id #'id "$~a+" (syntax-e #'id))]  ; hash-ref!
                   [$id- (format-id #'id "$~a-" (syntax-e #'id))]) ; hash-remove!
       #'(begin (define %id : (HashTable Symbol Type) ((inst make-hasheq Symbol Type) init-vals))
                (define ($id@) : (Listof Symbol) ((inst hash-keys Symbol Type) %id))
                (define ($id*) : (Listof Type) ((inst hash-values Symbol Type) %id))
                (define ($id#) : Index ((inst hash-count Symbol Type) %id))
                (define ($id? [key : Symbol]) : Boolean (hash-has-key? %id key))
                (define (!id+ [key : Symbol] [val : Type]) : Void ((inst hash-set! Symbol Type) %id key val))
                (define ($id- [key : Symbol]) : Void ((inst hash-remove! Symbol Type) %id key))
                (define $id+ : (-> Symbol (-> Type) Type)
                  (lambda [key setval]
                    ((inst hash-ref! Symbol Type) %id key setval)))
                (define $id : (case-> [Symbol -> Type] [Symbol (-> Type) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref Symbol Type Type) %id key)]
                    [(key defval) ((inst hash-ref Symbol Type Type) %id key defval)]))))]))

(define-syntax (define/extract-symtable stx)
  (syntax-case stx []
    [(_ (symtable-sexp ...) defines ...)
     (with-syntax ([symtable (format-id #'symtable "~a" (gensym 'symboltable))])
       #'(begin (define symtable : (HashTable Symbol Any) (cast (symtable-sexp ...) (HashTable Symbol Any)))
                (define/extract-symtable symtable defines ...)))]
    [(_ symtable defines ...)
     (with-syntax ([(extract ...)
                    (for/list ([def-idl (in-list (syntax->list #'(defines ...)))])
                      (syntax-case def-idl [: =]
                        [([renamed-id key] : Type)
                         #'(define renamed-id : Type (cast (hash-ref symtable 'key) Type))]
                        [([renamed-id key] : Type = def-exp)
                         #'(define renamed-id : Type (cast (hash-ref symtable 'key (thunk def-exp)) Type))]
                        [(id : Type)
                         #'(define id : Type (cast (hash-ref symtable 'id) Type))]
                        [(id : Type = def-exp)
                         #'(define id : Type (cast (hash-ref symtable 'id (thunk def-exp)) Type))]))])
       #'(begin extract ...))]))

(define-syntax (define-parameter/extract-info stx)
  (syntax-case stx []
    [(_ infodir defines ...)
     (with-syntax* ([info-ref (format-id #'info-ref "~a" (gensym 'inforef))]
                    [(extract ...)
                     (for/list ([def-idl (in-list (syntax->list #'(defines ...)))])
                       (syntax-case def-idl [: = =>]
                         [([renamed-id key] : Type = def-exp => [fvalue ...])
                          #'(define renamed-id : (Parameterof Type) (make-parameter (match (info-ref 'key (thunk def-exp)) fvalue ...)))]
                         [([renamed-id key] : Type => [fvalue ...] = def-exp)
                          #'(define renamed-id : (Parameterof Type) (make-parameter (match (info-ref 'key void) fvalue ... [(? void?) def-exp])))]
                         [([renamed-id key] : Type => [fvalue ...])
                          #'(define renamed-id : (Parameterof Type) (make-parameter (match (info-ref 'key void) fvalue ...)))]
                         [([renamed-id key] : Type = def-exp)
                          #'(define renamed-id : (Parameterof Type) (make-parameter (cast (info-ref 'key (thunk def-exp)) Type)))]
                         [([renamed-id key] : Type)
                          #'(define renamed-id : (Parameterof Type) (make-parameter (cast (info-ref 'key) Type)))]
                         [(id : Type = def-exp => [fvalue ...])
                          #'(define id : (Parameterof Type) (make-parameter (match (info-ref 'id (thunk def-exp)) fvalue ...)))]
                         [(id : Type => [fvalue ...] = def-exp)
                          #'(define id : (Parameterof Type) (make-parameter (match (info-ref 'id void) fvalue ... [(? void?) def-exp])))]
                         [(id : Type => [fvalue ...])
                          #'(define id : (Parameterof Type) (make-parameter (match (info-ref 'id void) fvalue ...)))]
                         [(id : Type = def-exp)
                          #'(define id : (Parameterof Type) (make-parameter (cast (info-ref 'id (thunk def-exp)) Type)))]
                         [(id : Type)
                          #'(define id : (Parameterof Type) (make-parameter (cast (info-ref 'id) Type)))]))])
       #'(begin (define info-ref : Info-Ref
                  (let ([ref (get-info/full infodir)])
                    (if (false? ref) (throw [exn:fail:filesystem] "info.rkt not found in ~a" infodir) ref)))
                extract ...))]))
