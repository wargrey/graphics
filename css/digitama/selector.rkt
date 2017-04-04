#lang typed/racket

;;; https://drafts.csswg.org/selectors

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

;; https://drafts.csswg.org/selectors/#grammar
;; https://drafts.csswg.org/selectors/#structure
;; https://drafts.csswg.org/selectors/#data-model
(define-syntax (define-selectors stx)
  (syntax-case stx []
    [(_ [s-id #:+ S-ID rest ...] ...)
     #'(begin (struct: s-id : S-ID rest ...) ...)]))

(define-syntax (define-exclusive-:classes stx)
  (syntax-case stx []
    [(_ id [v1 v2] ...) ; TODO: multiple exclusives
     #'(define id : (HashTable Symbol Symbol)
         (make-hasheq (list (cons 'v1 'v2) ...
                            (cons 'v2 'v1) ...)))]))

(define-type CSS-Namespace-Hint (U (HashTable Symbol String) (Listof Symbol) False))
(define-type CSS-Complex-Selector (Listof+ CSS-Compound-Selector))

(define-selectors
  [css-attribute-selector #:+ CSS-Attribute-Selector ([name : Symbol] [quirk : Symbol] [namespace : (U Symbol Boolean)])]
  [css-attribute~selector #:+ CSS-Attribute~Selector css-attribute-selector ([type : Char] [value : (U Symbol String)] [i? : Boolean])]  

  [css-:class-selector    #:+ CSS-:Class-Selector ([name : Symbol] [arguments : (Option (Listof CSS-Token))])]
  [css-::element-selector #:+ CSS-::Element-Selector
                          ([name : Symbol]
                           [arguments : (Option (Listof CSS-Token))]
                           [:classes : (Listof CSS-:Class-Selector)])]

  [css-compound-selector  #:+ CSS-Compound-Selector
                          ([combinator : (Option CSS-Selector-Combinator)]
                           [type : (U Symbol True)]
                           [quirk : (U Symbol True)]
                           [namespace : (U Symbol Boolean)]
                           [ids : (Listof Keyword)]
                           [classes : (Listof Symbol)]
                           [attributes : (Listof CSS-Attribute-Selector)]
                           [:classes : (Listof CSS-:Class-Selector)]
                           [::element : (Option CSS-::Element-Selector)])])

(define css-selector-match : (->* (CSS-Complex-Selector (Listof CSS-Subject)) (Boolean) (Option Nonnegative-Fixnum))
  ;;; https://drafts.csswg.org/selectors/#evaluating-selectors
  (lambda [selectors stnemele [quirk? #false]]
    ; TODO: define a better object model
    (define root : Symbol (css-root-element-type))
    (let evaluate : (Option Nonnegative-Fixnum) ([srotceles : (Listof CSS-Compound-Selector) (reverse selectors)]
                                                 [stcejbus : (Listof CSS-Subject) stnemele]
                                                 [specificity : Nonnegative-Fixnum 0])
      (cond [(null? stcejbus) (and (null? srotceles) specificity)]
            [(null? srotceles) specificity]
            [else (let* ([element (car stcejbus)]
                         [root? (eq? (css-subject-type element) root)]
                         [specificity++ (css-compound-selector-match (car srotceles) element root? quirk?)])
                    (and specificity++ (evaluate (cdr srotceles) (cdr stcejbus) (fx+ specificity specificity++))))]))))

(define css-compound-selector-match : (->* (CSS-Compound-Selector CSS-Subject Boolean) (Boolean) (Option Nonnegative-Fixnum))
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#case-sensitive
  (lambda [selector element root? [quirk? #false]] ; WARNING: `quirk?` only affects type name and attribute names
    (and (css-combinator-match? (css-compound-selector-combinator selector) (css-subject-combinator element))
         (css-namespace-match? (css-compound-selector-namespace selector) (css-subject-namespace element))
         (let ([s:type : (U Symbol True) (if quirk? (css-compound-selector-quirk selector) (css-compound-selector-type selector))])
           (or (eq? s:type #true) (eq? s:type (css-subject-type element))))
         (let ([s:ids : (Listof Keyword) (css-compound-selector-ids selector)]
               [id : (U Keyword (Listof+ Keyword)) (css-subject-id element)])
           (cond [(null? s:ids) #true]
                 [(keyword? id) (and (null? (cdr s:ids)) (eq? (car s:ids) id))]
                 [else (and (list? s:ids) (set=? (list->set s:ids) (list->set id)))]))
         (let ([s:classes : (Listof Symbol) (css-compound-selector-classes selector)]
               [classes : (Listof Symbol) (css-subject-classes element)])
           (for/and : Boolean ([s:c (in-list s:classes)]) (and (memq s:c classes) #true)))
         (let ([s:attrs : (Listof CSS-Attribute-Selector) (css-compound-selector-attributes selector)]
               [attrs : (HashTable Symbol CSS-Attribute-Value) (css-subject-attributes element)])
           (css-attribute-match? s:attrs attrs quirk?))
         (let ([s:classes : (Listof CSS-:Class-Selector) (css-compound-selector-:classes selector)]
               [:classes : (Listof Symbol) (css-subject-:classes element)])
           (or (null? s:classes)
               (and root? (css-:classes-match? s:classes (cons 'root :classes)))
               (and (pair? :classes) (css-:classes-match? s:classes :classes))))
         (let-values ([(a b c) (css-selector-specificity-ABC selector)])
           (css-ABC->specificity a b c)))))

(define css-selector-specificity-ABC : (-> CSS-Compound-Selector (values Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum))
  ;;; https://drafts.csswg.org/selectors/#specificity-rules
  (lambda [static-unit]
    (values (length (css-compound-selector-ids static-unit))
            (fx+ (length (css-compound-selector-classes static-unit))
                 (fx+ (length (css-compound-selector-:classes static-unit))
                      (length (css-compound-selector-attributes static-unit))))
            (fx+ (if (css-compound-selector-::element static-unit) 1 0)
                 (if (symbol? (css-compound-selector-type static-unit)) 1 0)))))

(define css-declared-namespace : (-> CSS-Namespace-Hint (U CSS:Ident CSS:Delim Symbol) (U Symbol Boolean))
  (lambda [namespaces namespace]
    (or (css:delim? namespace)        ; *
        (let ([ns (if (css:ident? namespace) (css:ident-datum namespace) namespace)])
          (if (or (false? namespaces) ; application does not care namespaces
                  (and (hash? namespaces) (hash-has-key? namespaces ns))
                  (and (list? namespaces) (memq ns namespaces) #true))
              ns #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-exclusive-:classes css-exclusive-:classes
  [link visited]
  [playing paused]
  [enable disable]
  [read-only read-write]
  [valid invalid]
  [in-range out-of-range]
  [required optional])

(define css-ABC->specificity : (-> Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum)
  (lambda [A B C]
    (fxior (fxlshift A 16) (fxior (fxlshift B 8) C))))

(define css-selector-list-specificity : (->* (CSS-Complex-Selector) ((-> Natural Natural Natural Natural)) Natural)
  (lambda [selectors [-> css-ABC->specificity]]
    (define-values (A B C)
      (for/fold ([A : Nonnegative-Fixnum 0] [B : Nonnegative-Fixnum 0] [C : Nonnegative-Fixnum 0])
                ([selector (in-list selectors)])
        (define-values (a b c) (css-selector-specificity-ABC selector))
        (values (fx+ A a) (fx+ B b) (fx+ C c))))
    (-> A B C)))

(define css-combinator-match? : (-> (Option CSS-Selector-Combinator) CSS-Selector-Combinator Boolean)
  (lambda [s:combinator combinator]
    (case s:combinator
      [(> +) (eq? s:combinator combinator)]
      [(>>) (or (eq? combinator '>>) (eq? combinator '>))] ; the children are also descendents
      [(~) (or (eq? combinator '~) (eq? combinator '+))]   ; the next sibling is also a following sibling.
      [else (or (eq? combinator '>>) (eq? combinator '>))])))

(define css-namespace-match? : (-> (U Symbol Boolean) (U Symbol Boolean) Boolean)
  (lambda [src ns]
    (cond [(eq? src #true) #true]
          [(false? src) (false? ns)]
          [else (eq? src ns)])))

(define css-attribute-match? : (-> (Listof CSS-Attribute-Selector) (HashTable Symbol CSS-Attribute-Value) Boolean Boolean)
  (lambda [s:attrs attrs quirk?]
    (for/and : Boolean ([attr : CSS-Attribute-Selector (in-list s:attrs)])
      (and (hash-has-key? attrs (if quirk? (css-attribute-selector-quirk attr) (css-attribute-selector-name attr)))
           (let*-values ([(ns.val) (hash-ref attrs (css-attribute-selector-name attr))]
                         [(ns datum) (cond [(not (vector? ns.val)) (values #false ns.val)]
                                           [else (values (vector-ref ns.val 0) (vector-ref ns.val 1))])])
             (and (css-namespace-match? (css-attribute-selector-namespace attr) ns)
                  (or (not (css-attribute~selector? attr)) ; [attr]
                      (let* ([px:val : String (regexp-quote (~a (css-attribute~selector-value attr)))]
                             [mode : String (if (or quirk? (css-attribute~selector-i? attr)) "i" "-i")]
                             [val : String (if (list? datum) (string-join ((inst map String Any) ~a datum)) (~a datum))])
                        (and (non-empty-string? px:val)
                             (case (css-attribute~selector-type attr)
                               [(#\=) (regexp-match? (pregexp (format "(?~a:^~a$)" mode px:val)) val)]
                               [(#\~) (regexp-match? (pregexp (format "(?~a:\\b~a\\b)" mode px:val)) val)]
                               [(#\|) (regexp-match? (pregexp (format "(?~a:^~a(-|$))" mode px:val)) val)]
                               [(#\^) (regexp-match? (pregexp (format "(?~a:^~a)" mode px:val)) val)]
                               [(#\$) (regexp-match? (pregexp (format "(?~a:~a$)" mode px:val)) val)]
                               [(#\*) (regexp-match? (pregexp (format "(?~a:~a)" mode px:val)) val)]
                               [else #false]))))))))))

(define css-:classes-match? : (-> (Listof+ CSS-:Class-Selector) (Listof+ Symbol) Boolean)
  (lambda [s:classes :classes]
    (define excluded : (Listof Symbol) (filter-map (λ [[:c : Symbol]] (hash-ref css-exclusive-:classes :c (λ _ #false))) :classes))
    (and (or (null? excluded)
             (for/and : Boolean ([s:c (in-list s:classes)])
               (not (memq (css-:class-selector-name s:c) excluded))))
         (for/or : Any ([s:c (in-list s:classes)])
           ; TODO: deal with functional (especially dynamical) pseudo classes
           (memq (css-:class-selector-name s:c) :classes))
         #true)))
