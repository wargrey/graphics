#lang at-exp typed/racket

(provide (except-out (all-defined-out) #%full-module))

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))

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

(define-syntax (throw stx)
  (syntax-case stx []
    [(_ [st-id st-argl ...] message)
     #'(let* ([px.this (regexp (regexp-quote (path->string (#%file))))]
              [ccm (current-continuation-marks)]
              [msg (~a (let find-first-named-function-in ([stack (continuation-mark-set->context ccm)])
                         (or (and (regexp-match? px.this (~a (cdar stack))) (caar stack))
                             (find-first-named-function-in (cdr stack))))
                       #\: #\space message)])
         (raise (st-id msg ccm st-argl ...)))]
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
     #'(begin (define id : Type val)
              ...)]))

(define-syntax (define-type/enum stx)
  (syntax-case stx [: quote]
    [(_ id : TypeU (quote enum) ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU [enum comments ...] ...)
     #'(define-type/enum id : TypeU enum ...)]
    [(_ id : TypeU enum ...)
     #'(begin (define-type TypeU (U 'enum ...))
              (define id : (Listof TypeU) (list 'enum ...)))]))

(define-syntax (define-type/consts stx)
  (syntax-case stx [: as]
    [(_ cs : TypeU of Type (enum val comments ...) ...)
     (with-syntax ([$%cs (format-id #'cs "$%~a" (syntax-e #'cs))]
                   [$#cs (format-id #'cs "$#~a" (syntax-e #'cs))])
       #'(begin (define-type TypeU (U 'enum ...))
                (define $#cs : (-> TypeU Type)
                  (let ([cs : (HashTable TypeU Type) ((inst make-immutable-hasheq TypeU Type) (list (cons 'enum val) ...))])
                    (lambda [sym] ((inst hash-ref TypeU Type Type) cs sym))))
                (define $%cs : (-> Type TypeU)
                  (let ([cs : (HashTable Type TypeU) ((inst make-immutable-hash Type TypeU) (list (cons val 'enum) ...))])
                    (lambda [v] ((inst hash-ref Type TypeU TypeU) cs v))))))]))

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
                (define $id+ : (-> String (U Type (-> Type)) Type)
                  (lambda [key setval]
                    ((inst hash-ref! String Type) %id key (if (procedure? setval) setval (thunk setval)))))
                (define $id : (case-> [String -> Type] [String (U Type (-> Type)) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref String Type Type) %id key)]
                    [(key defval) ((inst hash-ref String Type Type) %id key (if (procedure? defval) defval (thunk defval)))]))))]))

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
                (define $id+ : (-> Symbol (U Type (-> Type)) Type)
                  (lambda [key setval]
                    ((inst hash-ref! Symbol Type) %id key (if (procedure? setval) setval (thunk setval)))))
                (define $id : (case-> [Symbol -> Type] [Symbol (U Type (-> Type)) -> Type])
                  (case-lambda
                    [(key) ((inst hash-ref Symbol Type Type) %id key)]
                    [(key defval) ((inst hash-ref Symbol Type Type) %id key (if (procedure? defval) defval (thunk defval)))]))))]))

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
