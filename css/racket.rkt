#lang typed/racket

;;; Interacting with Racket

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "recognizer.rkt")

(begin-for-syntax
  (require racket/list)

  (define (λpool-set! the-pool id λid)
    (define-values (rkws akws) (procedure-keywords λid))
    (define arities (let ([a (procedure-arity λid)]) (if (list? a) a (list a))))
    (define mbarty (last arities))
    (hash-set! the-pool id (vector rkws akws (filter integer? arities)
                                   (cond [(integer? mbarty) +inf.0]
                                         [else (arity-at-least-value mbarty)]))))
  
  (define (make-pool <the-@λ-pool> λnames)
    (define the-pool (make-hasheq))
    (define hints (if (list? λnames) λnames (list λnames)))
    (define regexps (filter regexp? hints))
    (for ([id (in-list (filter symbol? hints))])
      (define λid (eval id))
      (when (procedure? λid) (λpool-set! the-pool id λid)))
    (when (pair? regexps)
      (for ([id (in-list (namespace-mapped-symbols))])
        (with-handlers ([exn? void])
          (define λid (eval id))
          (when (and (procedure? λid)
                     (let ([strid (symbol->string id)])
                       (ormap (λ [regexp] (regexp-match? regexp strid)) regexps)))
            (λpool-set! the-pool id λid)))))
    (datum->syntax <the-@λ-pool> the-pool)))

(define-type CSS-@λ-Metainfo (Vector (Listof Keyword) (Listof Keyword) (Listof Natural) (U Natural +inf.0)))
(define-type CSS-@λ-Pool (HashTable Symbol CSS-@λ-Metainfo))
(define-type CSS-@λ-Filter (case-> [Symbol Keyword -> (U (CSS:Filter CSS-Datum) Void)]
                                   [Symbol False -> (U (CSS-Parser (Listof CSS-Datum)) Void)]))

(define-css-value css-@λ #:as CSS-@λ ([sexp : (Pairof Symbol (Listof Any))]))

(define-syntax (define-@λ-pool stx)
  (syntax-case stx []
    [(_ the-@λ-pool #:λnames λnames modpaths ...)
     (with-syntax ([the-pool (parameterize ([current-namespace (make-base-namespace)])
                               (eval `(require ,@(syntax->datum #'(modpaths ...))))
                               (make-pool #'the-@λ-pool (syntax->datum #'λnames)))])
       #'(define the-@λ-pool : CSS-@λ-Pool the-pool))]))

(define-syntax (define-css-racket-value-filter stx)
  (syntax-case stx []
    [(_ <racket-value> #:with ?value #:as ValueType asserts ...)
     #'(define <racket-value> : (->* () (Namespace) (CSS:Filter ValueType))
         (lambda [[ns (current-namespace)]]
           (λ [[token : CSS-Syntax-Any]] : (CSS-Option ValueType)
             (and (css:racket? token)
                  (let ([?value (css-eval-value token ns)])
                    (cond asserts ... [(exn:css? ?value) ?value]
                          [else (make-exn:css:contract token)]))))))]
    [(_ <racket-value> #:is-a? class% #:as ValueType)
     #'(define-css-racket-value-filter <racket-value> #:with ?value #:as ValueType
         [(is-a? ?value class%) (cast ?value ValueType)])]
    [(_ <racket-value> #:? type? #:as ValueType)
     #'(define-css-racket-value-filter <racket-value> #:with ?value #:as ValueType
         [(type? ?value) ?value])]))

(define-css-atomic-filter <css:@λ> #:-> CSS-@λ
  #:with [[token : css:λracket?] [λpool : CSS-@λ-Pool] [λfilter : CSS-@λ-Filter] [λids : (Listof Symbol) null]]
  (define λid : Symbol (css:λracket-datum token))
  (cond [(and (pair? λids) (not (memq λid λids))) (make-exn:css:range token)]
        [(hash-ref λpool λid (thunk #false)) => (λ [λinfo] (do-filter token λid λinfo))]
        [else (make-exn:css:range token)])
  #:where
  [(define (do-filter [<λ> : CSS:λRacket] [λname : Symbol] [λinfo : CSS-@λ-Metainfo]) : (U CSS-@λ CSS-Syntax-Error)
     (define λ:all : (Listof Keyword) (vector-ref λinfo 1))
     (define λarities : (Listof Natural) (vector-ref λinfo 2))
     (define λmin-arty : (U Natural +inf.0) (vector-ref λinfo 3))
     (let λ-fold ([swk : (Listof CSS-Datum) null]
                  [λ:mkws : (Listof Keyword) (vector-ref λinfo 0)]
                  [args : (Listof CSS-Token) (css:λracket-arguments <λ>)])
       (define-values (head tail) (css-car/cdr args))
       (cond [(css:hash? head)
              (define-values (value rest) (css-car/cdr tail))
              (define λ:kw : Keyword (css:hash-datum head))
              (cond [(eof-object? value) (make-exn:css:arity head)]
                    [(not (memq λ:kw λ:all)) (make-exn:css:range head)]
                    [else (let ([kw:filter (λfilter λname λ:kw)])
                            (cond [(void? kw:filter) (λ-fold swk λ:mkws rest)]
                                  [else (let ([datum (kw:filter value)])
                                          (cond [(exn:css? datum) datum]
                                                [(false? datum) (make-exn:css:type value)]
                                                [else (λ-fold (cons datum (cons λ:kw swk)) (remq λ:kw λ:mkws) rest)]))]))])]
             [(pair? λ:mkws) (make-exn:css:arity <λ>)]
             [else (let ([λparser (λfilter λname #false)])
                     (define-values (argl rest) (if (void? λparser) (values swk args) (λparser swk args)))
                     (cond [(exn:css? argl) argl]
                           [(false? argl) (make-exn:css:type rest)]
                           [(pair? rest) (make-exn:css:overconsumption rest)]
                           [else (let ([count (- (length argl) (length swk))])
                                   (cond [(nor (memq count λarities) (>= count λmin-arty)) (make-exn:css:arity <λ>)]
                                         [else (css-@λ (cons λname (reverse argl)))]))]))])))])

(define-css-atomic-filter <css-#boolean> #:-> (List 'values Boolean)
  #:with [[token : css:ident?]]
  (case (css:ident-norm token)
    [(false False) '(values #false)]
    [(true True)   '(values #true)]
    [else (make-exn:css:range token)]))

(define-css-disjoined-filter <css-#false> #:-> (List 'values False)
  (CSS:<=> (<css:ident> '(False false)) '(values #false)))

(define css-eval-value : (-> CSS:Racket Namespace (U Any CSS-Syntax-Error))
  (lambda [<thing> ns]
    (with-handlers ([exn? (λ _ (make-exn:css:racket <thing>))])
      (define id : Symbol (css:racket-datum <thing>))
      (define v : Any (call-with-values (thunk (eval id ns)) (λ _ (car _))))
      (if (parameter? v) (v) v))))

(define css-eval-@λ : (->* (CSS-@λ Namespace) ((Option Real)) Any)
  (lambda [datum ns [maybe-100% #false]]
    (define (fsexp [datum : Any]) : Any
      (cond [(single-flonum? datum) (* datum (or maybe-100% 1.0f0))]
            [(css:length? datum) (css:length->scalar datum #true)]
            [(css-@λ? datum) (map fsexp (css-@λ-sexp datum))]
            [else datum]))
    (call-with-values (thunk (eval (map fsexp (css-@λ-sexp datum)) ns))
      (λ do-not-support-multiple-values (car do-not-support-multiple-values)))))