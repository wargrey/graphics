#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.w3.org/Style/CSS/specs.en.html                                                   ;;;
;;;                                                                                             ;;;
;;; https://drafts.csswg.org/css-syntax                                                         ;;;
;;; https://drafts.csswg.org/css-values                                                         ;;;
;;; https://drafts.csswg.org/css-cascade                                                        ;;;
;;; https://drafts.csswg.org/selectors                                                          ;;;
;;; https://drafts.csswg.org/css-namespaces                                                     ;;;
;;; https://drafts.csswg.org/css-variables                                                      ;;;
;;; https://drafts.csswg.org/css-conditional                                                    ;;;
;;; https://drafts.csswg.org/mediaqueries                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (except-out (all-from-out (submod "." digitama)) css-log-syntax-error struct: Listof+))
(provide (except-out (all-from-out (submod "." grammar)) css-stylesheet-placeholder))

; https://drafts.csswg.org/css-syntax/#parser-entry-points
(provide css-parse-stylesheet
         css-parse-rule
         css-parse-rules
         css-parse-declaration
         css-parse-declarations
         css-parse-component-value
         css-parse-component-values
         css-parse-component-valueses
         css-parse-media-queries
         css-parse-feature-query
         css-parse-selectors)

(provide define-css-parser-entry
         css-consume-stylesheet
         css-query-support?
         css-components->declaration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama typed/racket
  (provide (except-out (all-defined-out) define-tokens define-token define-token-interface
                       define-symbolic-tokens define-numeric-tokens define-dimensional-tokens
                       define-prefab-keyword define-syntax-error css-make-syntax-error
                       css-tee-computed-value css-ref-raw))

  (provide (rename-out [exact-nonnegative-integer? natural?]))
  (provide (all-from-out racket/fixnum))
  (provide (all-from-out racket/flonum))

  (require racket/fixnum)
  (require racket/flonum)

  (require (for-syntax racket/string))
  (require (for-syntax racket/syntax))
  (require (for-syntax syntax/parse))

  (define-syntax (define-preference stx)
    (syntax-case stx [:]
      [(self preference #:as Preference (fields ...) options ...) #'(self preference #:as Preference #:with [] (fields ...) options ...)]
      [(self preference #:as Preference #:with [[bindings BindTypes ...] ...] ([property : DataType info ...] ...) options ...)
       (with-syntax* ([make-preference (format-id #'preference "make-~a" (syntax-e #'preference))]
                      [([?-property ArgType defval ...] ...)
                       (for/list ([field-info (in-list (syntax->list #'([property DataType info ...] ...)))])
                         (syntax-case field-info [Option]
                           [(p T #:= dv) #'[(if (css-wide-keyword? p) dv p) (U T CSS-Wide-Keyword) dv]]
                           [(p T) (raise-syntax-error (syntax-e #'self) "property or attribute requires a default value" #'p)]))]
                      [(args ...)
                       (for/fold ([args null])
                                 ([argument (in-list (syntax->list #'([property : ArgType defval ...] ...)))])
                         (cons (datum->syntax argument (string->keyword (symbol->string (car (syntax->datum argument)))))
                               (cons argument args)))]
                      [([pref-bindings properties ...] ...)
                       (for/list ([binding (in-list (syntax->list #'(bindings ...)))]
                                  [Types (in-list (syntax->list #'([BindTypes ...] ...)))])
                         (define types (syntax->datum Types))
                         (cons (format-id #'preference "~a-~a" (syntax-e #'preference) (syntax-e binding))
                               (for/fold ([properties null])
                                         ([property (in-list (syntax->list #'(property ...)))]
                                          [type (in-list (syntax->list #'(DataType ...)))])
                                 (cond [(not (memq (syntax-e type) types)) properties]
                                       [else (cons (syntax-e property) properties)]))))])
         #'(begin (define-type Preference preference)
                  (struct preference ([property : DataType] ...) options ...)
                  (define (make-preference args ...) : Preference (preference ?-property ...))
                  (define pref-bindings : (Listof Symbol) (list 'properties ...)) ...))]))
  
  (define-syntax (struct: stx)
    (syntax-case stx [:]
      [(_ id : ID rest ...)
       (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))])
         #'(begin (define-type ID id)
                  (struct id rest ... #:prefab #:extra-constructor-name make-id)))]))
  
  (define-syntax (define-token-interface stx)
    (syntax-case stx [:]
      [(_ symbolic-prefix : Type id? id-datum #:+ CSS:ID #:eq? type=?)
       (with-syntax ([id-filter (format-id #'symbolic-prefix "~a-filter" (syntax-e #'symbolic-prefix))]
                     [id=<-? (format-id #'symbolic-prefix "~a=<-?" (syntax-e #'symbolic-prefix))]
                     [id=:=? (format-id #'symbolic-prefix "~a=:=?" (syntax-e #'symbolic-prefix))])
         #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                    [Any (U (-> Type Boolean) (Listof Type)) -> (Option Type) : #:+ CSS:ID]))
                    (lambda [token range?]
                      (and (id? token) (let ([datum : Type (id-datum token)])
                                         (cond [(procedure? range?) (and (range? datum) datum)]
                                               [else (and (member datum range? type=?) datum)])))))

                  (define id-filter : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                       [(U (-> Type Boolean) (Listof Type) Type) -> (CSS:Filter Type)]
                                                       [-> (CSS:Filter Type)]))
                    (case-lambda
                      [() (λ [[t : (U CSS-Token EOF)]] (and (id? t) (id-datum t)))]
                      [(range?) (cond [(procedure? range?)
                                       (λ [[t : (U CSS-Token EOF)]]
                                         (or (id=<-? t range?)
                                             (and (id? t) (make-exn:css:range t))))]
                                      [(list? range?)
                                       (λ [[t : (U CSS-Token EOF)]]
                                         (and (id? t) ; TODO: Some symbolic tokens may have list-like data
                                              (or (let ([d (id-datum t)]) (and (member d range? type=?) d))
                                                  (make-exn:css:range t))))]
                                      [else (λ [[t : (U CSS-Token EOF)]]
                                              (and (id? t)
                                                   (or (let ([d (id-datum t)]) (and (type=? d range?) d))
                                                       (make-exn:css:range t))))])]))

                  (define id=:=? : (-> Any Type (Option Type) : #:+ CSS:ID)
                    (lambda [t v]
                      (and (id? t)
                           (let ([d : Type (id-datum t)])
                             (and (type=? d v) d)))))))]
      [(_ numeric-prefix : Type id? id-datum #:+ CSS:ID #:= type=?)
       (with-syntax ([id-filter (format-id #'numeric-prefix "~a-filter" (syntax-e #'numeric-prefix))]
                     [id=<-? (format-id #'numeric-prefix "~a=<-?" (syntax-e #'numeric-prefix))])
         #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                    [Any (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                    [Any Type (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                    [Any (Listof Type) -> (Option Type) : #:+ CSS:ID]))
                    (case-lambda [(token op n)   (and (id? token) (let ([d : Type (id-datum token)]) (and (op d n) d)))]
                                 [(token l op r) (and (id? token) (let ([m : Type (id-datum token)]) (and (op l m) (op m r) m)))]
                                 [(token range?) (and (id? token) (let ([d : Type (id-datum token)])
                                                                    (cond [(procedure? range?) (and (range? d) d)]
                                                                          [else (for/or : (Option Type) ([v (in-list range?)])
                                                                                  (and (type=? d v) d))])))]))

                  (define id-filter : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                       [(-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                       [Type (-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                       [(Listof Type) -> (CSS:Filter Type)]
                                                       [-> (CSS:Filter Type)]))
                    (case-lambda
                      [() (λ [[t : (U CSS-Token EOF)]] (and (id? t) (id-datum t)))]
                      [(op n) (λ [[t : (U CSS-Token EOF)]] (or (id=<-? t op n) (and (id? t) (make-exn:css:range t))))]
                      [(l op r) (λ [[t : (U CSS-Token EOF)]] (or (id=<-? t l op r) (and (id? t) (make-exn:css:range t))))]
                      [(num?) (λ [[t : (U CSS-Token EOF)]] (or (id=<-? t num?) (and (id? t) (make-exn:css:range t))))]))))]))

  (define-syntax (define-token stx)
    (syntax-parse stx #:literals [: Symbol↯ Keyword↯]
      [(_ id : Number parent #:as Type #:=? type=? #:with id? id-datum)
       (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
         #'(begin (struct: id : Number parent ([datum : Type]))
                  (define (id=? [t1 : Number] [t2 : Number]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                  (define-token-interface id : Type id? id-datum #:+ Number #:= type=?)))]
      [(_ id : Identifier parent ((~and (~or Symbol↯ Keyword↯) Type) rest ...) #:with id? id-datum)
       (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))]
                     [id-norm=? (format-id #'id "~a-norm=?" (syntax-e #'id))]
                     [id-norm (format-id #'id "~a-norm" (syntax-e #'id))])
         #'(begin (struct: id : Identifier parent ([datum : Type] [norm : Type] rest ...))
                  (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                  (define (id-norm=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-norm t1) (id-norm t2)))
                  (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq?)
                  (define-token-interface id-norm : Type id? id-norm  #:+ Identifier #:eq? eq?)))]
      [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
       (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                     [id=? (format-id #'id "~a=?" (syntax-e #'id))])
         #'(begin (struct: id : Otherwise parent ([datum : Type] rest ...))
                  (define (id=? [t1 : Otherwise] [t2 : Otherwise]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                  (define-token-interface id : Type id? id-datum #:+ Otherwise #:eq? type=?)))]))

  (define-syntax (define-symbolic-tokens stx)
    (syntax-parse stx
      [(_ token #:+ Token [id #:+ ID #:as Type rest ...] ...)
       (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                     [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
                     [([id? id-datum] ...)
                      (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                        (list (format-id <id> "~a?" (syntax-e <id>))
                              (format-id <id> "~a-datum" (syntax-e <id>))))])
         #'(begin (struct: token : Token css-token ())
                  (define-token id : ID token (Type rest ...) #:with id? id-datum) ...
                  (define-type Token-Datum (U Type ...))
                  (define (token->datum [t : Token]) : (Option Token-Datum) (cond [(id? t) (id-datum t)] ... [else #false]))))]))

  (define-syntax (define-lazy-tokens stx)
    (syntax-parse stx
      [(_ token #:+ Token [id #:+ ID #:with components #:as Type] ...)
       (with-syntax ([([id? id-copy Component] ...)
                      (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                        (list (format-id <id> "~a?" (syntax-e <id>))
                              (format-id <id> "~a-copy" (syntax-e <id>))
                              (if (eq? (syntax-e <id>) 'css:url) #'CSS-URL-Modifier #'CSS-Token)))])
         #'(begin (define-symbolic-tokens token #:+ Token [id #:+ ID #:as Type [components : (Listof Component)] [lazy? : Boolean]] ...)

                  (define id-copy : (-> ID (Listof Component) Boolean ID)
                    (lambda [instance subcoms ?]
                      (struct-copy id instance [components (if (css-pair? subcoms) subcoms null)] [lazy? ?])))
                  ...))]))

  (define-syntax (define-numeric-tokens stx)
    (syntax-case stx []
      [(_ token #:+ Token #:nan nan [id #:+ ID #:as Type] ...)
       (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                     [([id? id=? id-datum type=?] ...)
                      (for/list ([<id> (in-list (syntax->list #'(id ...)))]
                                 [<type> (in-list (syntax->list #'(Type ...)))])
                        (list (format-id <id> "~a?" (syntax-e <id>))
                              (format-id <id> "~a=?" (syntax-e <id>))
                              (format-id <id> "~a-datum" (syntax-e <id>))
                              (let ([type-name (symbol->string (syntax-e <type>))])
                                (cond [(string-contains? type-name "Single-Flonum") #'=]
                                      [(string-contains? type-name "Flonum") #'fl=]
                                      [(string-contains? type-name "Fixnum") #'fx=]
                                      [else #'=]))))])
         #'(begin (struct: token : Token css-numeric ())
                  (define-token id : ID token #:as Type #:=? type=? #:with id? id-datum) ...
                  (define (token->datum [t : Token]) : (U Type ...) (cond [(id? t) (id-datum t)] ... [else nan]))))]))
  
  (define-syntax (define-dimensional-tokens stx)
    (syntax-case stx []
      [(_ parent #:+ _ [id #:+ ID #:=> canonical-unit [transforms ...]] ...)
       (with-syntax ([token->datum (format-id #'parent "~a->datum" (syntax-e #'parent))]
                     [([id? +id? css:id->scalar css-id->scalar id-filter +id-filter] ...)
                      (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                        (define varname (symbol->string (syntax-e <id>)))
                        (list (format-id <id> "~a?" (syntax-e <id>))
                              (format-id <id> "~a?" (string-replace varname ":" "+"))
                              (format-id <id> "~a->scalar" (syntax-e <id>))
                              (format-id <id> "~a->scalar" (string-replace varname ":" "-"))
                              (format-id <id> "~a-filter" (string-replace varname "css:" "css-"))
                              (format-id <id> "~a-filter" (string-replace varname "css:" "css+"))))]
                     [([Flonum/Font Flunum/Font !font?] ...)
                      (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                        (if (not (eq? (syntax-e <id>) 'css:length))
                            (list #'Flonum #'Nonnegative-Flonum #'#true)
                            (list #'(U Flonum CSS:Length:Font) #'(U Nonnegative-Flonum CSS:Length:Font)
                                  #'(not (css:length:font? desc-value)))))])
         #'(begin (struct: id : ID parent ()) ...

                  (define css-id->scalar : (case-> [Nonnegative-Flonum Symbol -> Nonnegative-Flonum]
                                                   [Flonum Symbol -> Flonum])
                    (lambda [canonical-unit unit]
                      (case unit
                        [(canonical-unit) canonical-unit]
                        transforms ...
                        [else +nan.0])))
                  ...

                  (define +id? : (-> Any Boolean : #:+ (U ID CSS-Zero))
                    (lambda [token]
                      (or (and (id? token) (fl>= (css:dimension-datum token) 0.0))
                          (css-zero? token))))
                  ...

                  (define css:id->scalar : (case-> [(U ID CSS-Zero) -> Nonnegative-Flonum]
                                                   [(U ID CSS-Zero) False -> Nonnegative-Flonum]
                                                   [(U ID CSS-Zero) '#:direction -> Flonum])
                    (lambda [token [direction? #false]]
                      (cond [(not (id? token)) 0.0]
                            [(and direction?) (css-id->scalar (css:dimension-datum token) (css:dimension-unit token))]
                            [else (css-id->scalar (flabs (css:dimension-datum token)) (css:dimension-unit token))])))
                  ...

                  (define id-filter : (case-> [-> (CSS:Filter Flonum/Font)]
                                              [False -> (CSS:Filter Flonum/Font)]
                                              [True -> (CSS:Filter Flonum)])
                    (lambda [[ignore-font? #false]]
                      (λ [[desc-value : (U CSS-Token EOF)]]
                        (cond [(id? desc-value) (if (or ignore-font? !font?) (css:id->scalar desc-value) desc-value)]
                              [(css:dimension? desc-value) (make-exn:css:unit desc-value)]
                              [else #false]))))
                  ...

                  (define +id-filter : (case-> [-> (CSS:Filter Flunum/Font)]
                                               [False -> (CSS:Filter Flunum/Font)]
                                               [True -> (CSS:Filter Nonnegative-Flonum)])
                    (lambda [[ignore-font? #false]]
                      (λ [[desc-value : (U CSS-Token EOF)]]
                        (cond [(+id? desc-value) (if (or ignore-font? !font?) (css:id->scalar desc-value) desc-value)]
                              [(id? desc-value) (make-exn:css:range desc-value)]
                              [(css:dimension? desc-value) (make-exn:css:unit desc-value)]
                              [else #false]))))
                  ...
                  
                  (define token->datum : (-> (U CSS:Dimension CSS-Zero) Flonum)
                    (lambda [instance]
                      (cond [(id? instance) (css-id->scalar (css:dimension-datum instance) (css:dimension-unit instance))] ...
                            [(css-zero? instance) 0.0]
                            [else +nan.0])))))]))
  
  (define-syntax (define-tokens stx)
    (syntax-case stx []
      [(_ token #:+ Token header
          [[ptoken #:+ PToken #:-> pparent pfields] ...]
          [[ctoken #:+ CToken #:-> cparent] ...]
          (define-typical-tokens group #:+ Group rest ...) ...)
       (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                     [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
                     [([type? type->datum] ...)
                      (for/list ([<type> (in-list (syntax->list #'(group ...)))]
                                 #:unless (eq? (syntax-e <type>) 'css:dimension))
                        (list (format-id <type> "~a?" (syntax-e <type>))
                              (format-id <type> "~a->datum" (syntax-e <type>))))]
                     [(Symbolic-Datum ...)
                      (for/list ([<define> (in-list (syntax->list #'(define-typical-tokens ...)))]
                                 [<Type> (in-list (syntax->list #'(Group ...)))]
                                 #:when (eq? (syntax-e <define>) 'define-symbolic-tokens))
                        (format-id <Type> "~a-Datum" (syntax-e <Type>)))])
         #'(begin (struct: token : Token header)
                  (struct: ptoken : PToken pparent pfields) ...
                  (define-typical-tokens group #:+ Group rest ...) ...
                  (struct: ctoken : CToken cparent ()) ...

                  (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                  (define token->datum : (-> Token Token-Datum)
                    (lambda [instance]
                      (cond [(css:dimension? instance) (cons (css:dimension-datum instance) (css:dimension-unit instance))]
                            [(type? instance) (type->datum instance)] ...
                            [else (assert (object-name instance) symbol?)])))))]))

  ;;; https://drafts.csswg.org/css-syntax/#tokenization
  ;; https://drafts.csswg.org/css-syntax/#component-value
  ;; https://drafts.csswg.org/css-syntax/#current-input-token
  (define-type Symbol↯ Symbol)
  (define-type Keyword↯ Keyword)
  (define-type CSS-URL-Modifier (U CSS:Ident CSS-Lazy-Token))
  (define-type CSS-Zero (U CSS:Zero CSS:Flzero))
  (define-type CSS-One (U CSS:One CSS:Flone))

  (define-tokens css-token #:+ CSS-Token ([source : Any] [line : Natural] [column : Natural] [position : Natural] [span : Natural])
    [[css-numeric         #:+ CSS-Numeric         #:-> css-token   ([representation : String])]
     [css:dimension       #:+ CSS:Dimension       #:-> css-numeric ([datum : Flonum] [unit : Symbol])]]

    [[css:one             #:+ CSS:One             #:-> css:integer]
     [css:zero            #:+ CSS:Zero            #:-> css:integer]
     
     [css:flone           #:+ CSS:Flone           #:-> css:flonum]
     [css:flzero          #:+ CSS:Flzero          #:-> css:flonum]

     [css:colon           #:+ CSS:Colon           #:-> css:delim]
     [css:semicolon       #:+ CSS:Semicolon       #:-> css:delim]
     [css:comma           #:+ CSS:Comma           #:-> css:delim]
     [css:slash           #:+ CSS:Slash           #:-> css:delim]
     [css:vbar            #:+ CSS:VBar            #:-> css:delim]
     [css:cdo             #:+ CSS:CDO             #:-> css:cd]
     [css:cdc             #:+ CSS:CDC             #:-> css:cd]

     [css:length:font     #:+ CSS:Length:Font     #:-> css:length]
     [css:length:viewport #:+ CSS:Length:Viewport #:-> css:length]

     [css:bad:eof         #:+ CSS:Bad:EOF         #:-> css:bad]
     [css:bad:eol         #:+ CSS:Bad:EOL         #:-> css:bad]
     [css:bad:char        #:+ CSS:Bad:Char        #:-> css:bad]
     [css:bad:blank       #:+ CSS:Bad:Blank       #:-> css:bad]
     [css:bad:range       #:+ CSS:Bad:Range       #:-> css:bad]
     [css:bad:stdin       #:+ CSS:Bad:StdIn       #:-> css:bad]]

    ; TODO: Typed Racket is buggy if there are more than 11 conditions for (token->datum)
    (define-symbolic-tokens css-special-token #:+ CSS-Special-Token
      [css:bad            #:+ CSS:Bad             #:as (Pairof Symbol Datum)]
      [css:close          #:+ CSS:Close           #:as Char]

      ; Following tokens are remade by the parser instead of being produced by the tokenizer.
      [css:ratio          #:+ CSS:Ratio           #:as Positive-Exact-Rational]
      [css:racket         #:+ CSS:Racket          #:as Symbol])

    (define-symbolic-tokens css-symbolic-token #:+ CSS-Symbolic-Token
      [css:delim          #:+ CSS:Delim           #:as Char]
      [css:ident          #:+ CSS:Ident           #:as Symbol↯]
      [css:@keyword       #:+ CSS:@Keyword        #:as Keyword↯]
      [css:hash           #:+ CSS:Hash            #:as Keyword]
      [css:string         #:+ CSS:String          #:as String]
      [css:match          #:+ CSS:Match           #:as Char]
      [css:cd             #:+ CSS:CD              #:as Symbol]
      [css:urange         #:+ CSS:URange          #:as (Pairof Index Index)]
      [css:whitespace     #:+ CSS:WhiteSpace      #:as (U String Char)])

    (define-lazy-tokens css-lazy-token #:+ CSS-Lazy-Token
      [css:url            #:+ CSS:URL             #:with modifiers       #:as (U String 'about:invalid)]
      [css:block          #:+ CSS:Block           #:with components      #:as Char]
      [css:function       #:+ CSS:Function        #:with arguments       #:as Symbol↯]
      [css:λracket        #:+ CSS:λRacket         #:with arguments       #:as Symbol]
      [css:var            #:+ CSS:Var             #:with fallback        #:as Symbol])

    (define-numeric-tokens css-number #:+ CSS-Number #:nan +nan.0
      [css:integer        #:+ CSS:Integer         #:as Integer]
      [css:flonum         #:+ CSS:Flonum          #:as Flonum])

    (define-numeric-tokens css-fraction #:+ CSS-Fraction #:nan +nan.f
      [css:percentage     #:+ CSS:Percentage      #:as Single-Flonum])
  
    (define-dimensional-tokens css:dimension #:+ <Placeholder>
      ;;; https://drafts.csswg.org/css-values/#absolute-lengths
      ;;; https://drafts.csswg.org/css-values/#relative-lengths
      [css:length         #:+ CSS:Length          #:=> px
                          [[(cm)    (fl* px (fl/ 96.0 2.54))]
                           [(mm)    (fl* px (fl/ 96.0 25.4))]  ; 1cm/10
                           [(q)     (fl* px (fl/ 96.0 101.6))] ; 1cm/40
                           [(in)    (fl* px 96.0)]
                           [(pc)    (fl* px 16.0)]             ; 1in/6
                           [(pt)    (fl* px (fl/ 96.0 72.0))]  ; 1in/72
                           [(em)    (fl* px (flcss%-em length%))]
                           [(ex)    (fl* px (flcss%-ex length%))]
                           [(ch)    (fl* px (flcss%-ch length%))]
                           [(ic)    (fl* px (flcss%-ic length%))]
                           [(rem)   (fl* px (flcss%-rem length%))]
                           [(vw vi) (fl* px (fl* 0.01 (flcss%-vw length%)))]
                           [(vh vb) (fl* px (fl* 0.01 (flcss%-vh length%)))]
                           [(vmin)  (fl* px (fl* 0.01 (min (flcss%-vw length%) (flcss%-vh length%))))]
                           [(vmax)  (fl* px (fl* 0.01 (max (flcss%-vw length%) (flcss%-vh length%))))]]]
      ;;; https://drafts.csswg.org/css-values/#angles
      [css:angle          #:+ CSS:Angle           #:=> deg
                          [[(grad)  (fl* deg 0.9)]
                           [(rad)   (fl* deg (fl/ 180.0 pi))]
                           [(turn)  (fl* deg 360.0)]]]
      ;;; https://drafts.csswg.org/css-values/#time
      [css:time           #:+ CSS:Time            #:=> s
                          [[(ms)    (fl* s 0.001)]
                           [(min)   (fl* s 60.0)]
                           [(h)     (fl* s 3600.0)]]]
      ;;; https://drafts.csswg.org/css-values/#frequency
      [css:frequency      #:+ CSS:Frequency       #:=> kz
                          [[(khz)   (fl* kz 0.001)]]]
      ;;; https://drafts.csswg.org/css-values/#resolution
      [css:resolution     #:+ CSS:Resolution      #:=> dppx
                          [[(dpcm)  (css-length->scalar dppx 'cm)]
                           [(dpi)   (css-length->scalar dppx 'in)]]]))
  
  (define-syntax (css-remake-token stx)
    (syntax-case stx []
      [(_ [start-token end-token] make-css:token datum extra ...)
       #'(make-css:token (css-token-source start-token) (css-token-line start-token)
                         (css-token-column start-token) (css-token-position start-token)
                         (max (- (+ (css-token-position end-token) (css-token-span end-token))
                                 (css-token-position start-token)) 0)
                         datum extra ...)]
      [(_ here-token make-css:token datum ...)
       #'(css-remake-token [here-token here-token] make-css:token datum ...)]))

  (define nonnegative-fixnum? : (-> Any Boolean : #:+ Nonnegative-Fixnum) (λ [v] (and (fixnum? v) (fx>= v 0))))
  (define nonnegative-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [v] (and (flonum? v) (fl>= v 0.0))))
  (define nonnegative-single-flonum? : (-> Any Boolean : #:+ Nonnegative-Single-Flonum) (λ [v] (and (single-flonum? v) (>= v 0.0f0))))

  (define css-zero? : (-> Any Boolean : #:+ CSS-Zero) (λ [v] (or (css:zero? v) (css:flzero? v))))
  (define css-one? : (-> Any Boolean : #:+ CSS-One) (λ [v] (or (css:one? v) (css:flone? v))))

  (define css-token->syntax : (-> CSS-Token Syntax)
    (lambda [instance]
      (datum->syntax #false (css-token->datum instance)
                     (list (css-token-source instance)
                           (css-token-line instance) (css-token-column instance)
                           (css-token-position instance) (css-token-span instance)))))

  (define css-token-datum->string : (-> CSS-Token String)
    (lambda [instance]
      (cond [(css:ident? instance) (symbol->string (css:ident-datum instance))]
            [(css-fraction? instance) (string-append (css-numeric-representation instance) "%")]
            [(css:dimension? instance) (~a (css-numeric-representation instance) (css:dimension-unit instance))]
            [(css-numeric? instance) (css-numeric-representation instance)]
            [(css:@keyword? instance) (keyword->string (css:@keyword-datum instance))]
            [(css:hash? instance) (~a "#" (keyword->string (css:hash-datum instance)))]
            [(css:match? instance) (~a (css:match-datum instance) '=)]
            [(css:delim=:=? instance #\tab) "||"]
            [else (~a (css-token->datum instance))])))
  
  (define css-token->string : (->* (CSS-Token) ((Option Any)) String)
    (lambda [instance [alt-object #false]]
      (format "~a:~a:~a: ~a: ~a" (css-token-source instance) (css-token-line instance) (add1 (css-token-column instance))
              (or (object-name alt-object) (object-name instance)) (css-token-datum->string instance))))

  (define css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error)
                                      (U CSS-Syntax-Any (Listof CSS-Token))
                                      CSS-Syntax-Error)
    (lambda [exn:css any]
      (define tokens : (Listof CSS-Token)
        (cond [(eof-object? any) null]
              [(css:function? any) (cons any (css:function-arguments any))]
              [(css:λracket? any) (cons any (css:λracket-arguments any))]
              [(css:block? any) (cons any (css:block-components any))]
              [(list? any) (filter-not css:whitespace? any)]
              [else (list any)]))
      (define message : String
        (let-values ([(head others) (css-car/cdr tokens)])
          (cond [(eof-object? head) (format "~a" (if (eof-object? any) eof null))]
                [(null? others) (format "~a" (css-token->string head exn:css))]
                [else (format "~a ~a" (css-token->string head exn:css) (map css-token-datum->string others))])))
      (exn:css message (continuation-marks #false) (map css-token->syntax tokens))))

  (define css-log-syntax-error : (->* (CSS-Syntax-Error) ((Option CSS:Ident) Log-Level) Void)
    (lambda [errobj [property #false] [level 'warning]]
      (define in : String (if (false? property) "" (format " #:in ~a" (css:ident-datum property))))
      (log-message (current-logger) level 'exn:css:syntax (string-append (exn-message errobj) in) errobj)))

  ;;; https://drafts.csswg.org/css-syntax/#parsing
  (define-type (Listof+ css) (Pairof css (Listof css)))
  (define-type CSS-StdIn (U Input-Port Path-String Bytes (Listof CSS-Token)))
  (define-type CSS-Syntax-Any (U CSS-Token EOF))
  (define-type CSS-Syntax-Terminal (U CSS:Delim CSS:Close EOF))
  (define-type CSS-Syntax-Rule (U CSS-Qualified-Rule CSS-@Rule))
  (define-type CSS-Declarations (Listof CSS-Declaration))

  (define-syntax (define-syntax-error stx)
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid                
    (syntax-case stx []
      [(_ exn:css #:as Syntax-Error [subexn #:-> parent] ...)
       (with-syntax ([([make-exn make+exn throw-exn] ...)
                      (for/list ([<exn> (in-list (syntax->list #'(subexn ...)))])
                        (list (format-id <exn> "make-~a" (syntax-e <exn>))
                              (format-id <exn> "make+~a" (syntax-e <exn>))
                              (format-id <exn> "throw-~a" (syntax-e <exn>))))])
         #'(begin (define-type Syntax-Error exn:css)
                  (struct exn:css exn:fail:syntax ())
                  (struct subexn parent ()) ...

                  (define make-exn : (-> (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error)
                    (lambda [v]
                      (css-make-syntax-error subexn v)))
                  ...

                  (define make+exn : (->* ((U CSS-Syntax-Any (Listof CSS-Token))) ((Option CSS:Ident) Log-Level) CSS-Syntax-Error)
                    (lambda [v [property #false] [level 'warning]]
                      (define errobj : CSS-Syntax-Error (css-make-syntax-error subexn v))
                      (css-log-syntax-error errobj property level)
                      errobj))
                  ...

                  (define throw-exn : (->* ((U CSS-Syntax-Any (Listof CSS-Token))) ((Option CSS:Ident) Log-Level) Nothing)
                    (lambda [v [property #false] [level 'warning]]
                      (raise (make+exn v property level))))
                  ...))]))
  
  (define-syntax-error exn:css #:as CSS-Syntax-Error
    [exn:css:resource           #:-> exn:css]
    [exn:css:deprecated         #:-> exn:css]
    [exn:css:cyclic             #:-> exn:css]
    [exn:css:namespace          #:-> exn:css]
    [exn:css:racket             #:-> exn:css]
    [exn:css:contract           #:-> exn:css:racket]
    [exn:css:unrecognized       #:-> exn:css]
    [exn:css:misplaced          #:-> exn:css:unrecognized]
    [exn:css:type               #:-> exn:css:unrecognized]
    [exn:css:range              #:-> exn:css:unrecognized]
    [exn:css:unit               #:-> exn:css:range]
    [exn:css:overconsumption    #:-> exn:css:unrecognized]
    [exn:css:enclosed           #:-> exn:css:overconsumption]
    [exn:css:malformed          #:-> exn:css]
    [exn:css:empty              #:-> exn:css:malformed]
    [exn:css:arity              #:-> exn:css:malformed]
    [exn:css:missing-identifier #:-> exn:css:malformed]
    [exn:css:missing-variable   #:-> exn:css:missing-identifier]
    [exn:css:missing-block      #:-> exn:css:malformed]
    [exn:css:missing-value      #:-> exn:css:malformed]
    [exn:css:missing-feature    #:-> exn:css:malformed]
    [exn:css:missing-delimiter  #:-> exn:css:malformed]
    [exn:css:missing-colon      #:-> exn:css:missing-delimiter]
    [exn:css:missing-comma      #:-> exn:css:missing-delimiter]
    [exn:css:missing-slash      #:-> exn:css:missing-delimiter])

  (struct: css-@rule : CSS-@Rule ([name : CSS:@Keyword] [prelude : (Listof CSS-Token)] [block : (Option CSS:Block)]))
  (struct: css-qualified-rule : CSS-Qualified-Rule ([prelude : (Listof+ CSS-Token)] [block : CSS:Block]))
  (struct: css-declaration : CSS-Declaration ([name : CSS:Ident] [values : (Listof+ CSS-Token)] [important? : Boolean] [lazy? : Boolean]))

  ;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
  (define-type (CSS-Option css) (U css CSS-Syntax-Error False))
  (define-type (CSS:Filter css) (-> (U CSS-Token EOF) (CSS-Option css)))
  (define-type (CSS-Parser css) (-> css (Listof CSS-Token) (Values (CSS-Option css) (Listof CSS-Token))))

  (define-syntax (define-css-filter stx)
    (syntax-case stx [:]
      [(_ atom-filter #:-> RangeType #:with [[dom : DomType defval ...] ...] atom-filters ...)
       #'(define atom-filter : (-> DomType ... (CSS:Filter RangeType)) (λ [dom ...] (CSS:<+> atom-filters ...)))]
      [(_ atom-filter #:-> RangeType atom-filters ...)
       #'(define atom-filter : (-> (CSS:Filter RangeType)) (λ [] (CSS:<+> atom-filters ...)))]))

  (define-syntax (define-css-longhand-defaulting stx)
    (syntax-parse stx #:literals [False]
      [(_ id (~optional (~seq #:with datum #:as Type)) [property sexp ...] ...)
       (with-syntax ([?-Syntax-Error (if (attribute Type) #'(U Type CSS-Syntax-Error) #'(Option CSS-Syntax-Error))]
                     [?-datum (if (attribute datum) #'datum #'?-exn)])
         #'(define id : (->* (?-Syntax-Error) (CSS-Longhand-Values) CSS-Longhand-Values)
             (lambda [?-datum [longhand ((inst make-hasheq Symbol (U CSS-Datum CSS-Syntax-Error)))]]
               (hash-ref! longhand 'property (thunk (cond [(exn:css? ?-datum) ?-datum] [else sexp ...]))) ...
               longhand)))]))

  (define-syntax (css-make-datum->size stx)
    (syntax-parse stx
      [(_ #:100% fl% #:= defval (~optional (~seq #:as NanType)) (~optional (~seq (~and #:no-direction +))))
       (with-syntax ([SizeType (if (attribute NanType) #'NanType #'defval)]
                     [(fl? sfl? length? FLType direction)
                      (cond [(not (attribute +)) (list #'flonum? #'single-flonum? #'css:length? #'Flonum #''#:direction)]
                            [else (list #'nonnegative-flonum? #'nonnegative-single-flonum? #'css+length?
                                        #'Nonnegative-Flonum #'#false)])])
         #'(lambda [[_ : Symbol] [size : CSS-Datum]] : (U FLType SizeType)
             (cond [(fl? size) size]
                   [(sfl? size) (fl* (real->double-flonum size) fl%)]
                   [(length? size) (css:length->scalar size direction)]
                   [else defval])))]))

  (define-syntax (CSS:<+> stx)
    (syntax-case stx []
      [(_ css-filter) #'css-filter]
      [(_ css-filter css-filters ...) #'(css:disjoin css-filter (CSS:<+> css-filters ...))]))
  
  (define-syntax (CSS:<•> stx)
    (syntax-case stx []
      [(_ css-filter f) #'(css:compose css-filter f)]
      [(_ css-filter f g) #'(css:compose (css:compose css-filter g) f)]
      [(_ css-filter f g h ...) #'(css:compose css-filter (compose f g h ...))]))
  
  (define CSS:<=> : (All (a b) (-> (CSS:Filter a) b (CSS:Filter b)))
    (lambda [css-filter const]
      (λ [[desc-value : (U CSS-Token EOF)]] : (CSS-Option b)
        (define datum : (CSS-Option a) (css-filter desc-value))
        (if (exn:css? datum) datum (and datum const)))))

  (define CSS:<?> : (All (a) (->* ((CSS:Filter a)) ((Option a)) (CSS:Filter a)))
    (lambda [css-filter [default-value #false]]
      (λ [[token : (U CSS-Token EOF)]] : (CSS-Option a)
        (define datum : (CSS-Option a) (css-filter token))
        (if (not (false? datum)) datum default-value))))
  
  (define CSS<^> : (-> (CSS:Filter CSS-Datum) (CSS-Parser (Listof CSS-Datum)))
    (lambda [atom-filter]
      (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
        (define-values (head tail) (css-car/cdr tokens))
        (define datum : (CSS-Option CSS-Datum) (atom-filter head))
        (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
              [else (values (cons datum data) tail)]))))
  
  (define CSS<#> : (-> (CSS:Filter CSS-Datum) Symbol (CSS-Parser CSS-Longhand-Values))
    (lambda [atom-filter tag]
      (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
        (define-values (head tail) (css-car/cdr tokens))
        (define datum : (CSS-Option CSS-Datum) (atom-filter head))
        (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
              [else (values (hash-set data tag datum) tail)]))))
  
  (define CSS<+> : (All (a) (-> (CSS-Parser a) (CSS-Parser a) * (CSS-Parser a)))
    (lambda [head-branch . tail-branches]
      (λ [[datum : a] [tokens : (Listof CSS-Token)]] : (Values (CSS-Option a) (Listof CSS-Token))
        (let switch ([head-parser : (CSS-Parser a) head-branch]
                     [tail-parsers : (Listof (CSS-Parser a)) tail-branches])
          (define-values (++datum --tokens) (head-parser datum tokens))
          (cond [(nor (false? ++datum) (exn:css? ++datum)) (values ++datum --tokens)]
                [(pair? tail-parsers) (switch (car tail-parsers) (cdr tail-parsers))]
                [else (values ++datum --tokens)])))))

  (define CSS<~> : (All (a) (-> (CSS-Parser a) (CSS-Parser a) (CSS-Parser a)))
    (lambda [cond-parser value-parser]
      (λ [[datum : a] [tokens : (Listof CSS-Token)]] : (Values (CSS-Option a) (Listof CSS-Token))
        (define-values (++datum --tokens) (cond-parser datum tokens))
        (cond [(nor (false? ++datum) (exn:css? ++datum)) (value-parser datum --tokens)]
              [else (values ++datum tokens)]))))
  
  (define CSS<*> : (All (a) (->* ((CSS-Parser a)) (Index (U Index +inf.0)) (CSS-Parser a)))
    (lambda [css-parser [least 0] [maybe-most +inf.0]]
      (define most : (U Index +inf.0) (if (< maybe-most least) least maybe-most))
      (cond [(zero? most) (λ [[datum : a] [tokens : (Listof CSS-Token)]] (values datum tokens))]
            [else (λ [[datum : a] [tokens : (Listof CSS-Token)]] : (Values (CSS-Option a) (Listof CSS-Token))
                    (let seq ([datum++ : a datum]
                              [tokens-- : (Listof CSS-Token) tokens]
                              [count : Natural 1])
                      (define-values (++datum --tokens) (css-parser datum++ tokens--))
                      (cond [(= count most) (values ++datum --tokens)]
                            [(nor (false? ++datum) (exn:css? ++datum)) (seq ++datum --tokens (add1 count))]
                            [(< least count) (values datum++ tokens--)]
                            [else (values ++datum --tokens)])))])))
  
  (define CSS<?> : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
    (lambda [css-parser]
      (CSS<*> css-parser 0 1)))
  
  (define CSS<@> : (All (a) (-> (-> a a) (CSS-Parser a) * (CSS-Parser a)))
    (lambda [final . css-parsers]
      (λ [[datum : a] [tokens : (Listof CSS-Token)]] : (Values (CSS-Option a) (Listof CSS-Token))
        (let fold ([parsers : (Listof (CSS-Parser a)) css-parsers]
                   [datum++ : a datum]
                   [tokens-- : (Listof CSS-Token) tokens])
          (cond [(null? parsers) (values (final datum++) tokens--)]
                [else (let ([css-parser (car parsers)])
                        (define-values (++datum --tokens) (css-parser datum++ tokens--))
                        (cond [(or (false? ++datum) (exn:css? ++datum)) (values ++datum --tokens)]
                              [else (fold (cdr parsers) ++datum --tokens)]))])))))
  
  (define css:disjoin : (All (a b) (-> (CSS:Filter a) (CSS:Filter b) (CSS:Filter (U a b))))
    (lambda [css-filter1 css-filter2]
      (λ [[desc-value : (U CSS-Token EOF)]] : (CSS-Option (U a b))
        (define datum : (CSS-Option a) (css-filter1 desc-value))
        (cond [(false? datum) (css-filter2 desc-value)]
              [else datum #|TODO: to check if `exn:css:range` is also okay|#]))))
  
  (define css:compose : (All (a b) (-> (CSS:Filter a) (-> a b) (CSS:Filter b)))
    (lambda [css-filter css->racket]
      (λ [[desc-value : (U CSS-Token EOF)]] : (CSS-Option b)
        (define datum : (CSS-Option a) (css-filter desc-value))
        (if (exn:css? datum) datum (and datum (css->racket datum))))))
  
  (define-css-filter css-keyword-filter #:-> Symbol
    #:with [[options : (U (Listof Symbol) Symbol)]]
    (css:ident-norm-filter options))
  
  (define-css-filter css-boolean-filter #:-> (U Zero One)
    (CSS:<=> (css:integer-filter = 0) 0)
    (CSS:<=> (css:integer-filter = 1) 1))
  
  (define-css-filter css-natural-filter #:-> Natural
    (css:integer-filter exact-nonnegative-integer?))
  
  (define-css-filter css+real-filter #:-> (U Natural Nonnegative-Flonum)
    (css:flonum-filter nonnegative-flonum?)
    (css:integer-filter exact-nonnegative-integer?))

  (define-css-filter css+%real-filter #:-> (U Natural Nonnegative-Inexact-Real)
    (css:percentage-filter nonnegative-single-flonum?)
    (css+real-filter))

  (define-css-filter css-flunit-filter #:-> Nonnegative-Flonum
    (CSS:<•> (css:flonum-filter 0.0 fl<= 1.0) flabs)
    (CSS:<=> (css:integer-filter = 0) 0.0)
    (CSS:<=> (css:integer-filter = 1) 1.0))

  (define-css-filter css-%flunit-filter #:-> Nonnegative-Flonum
    (CSS:<•> (css:percentage-filter 0f0 <= 1f0) flabs real->double-flonum)
    (css-flunit-filter))

  (define css-keywords-parser : (->* ((Listof Symbol)) (Symbol) (CSS-Parser (Listof CSS-Datum)))
    (lambda [options [none 'none]]
      (CSS<+> (CSS<^> (CSS:<=> (css-keyword-filter none) null))
              (CSS<*> (CSS<^> (css-keyword-filter options)) 1 +inf.0))))

  ;; https://drafts.csswg.org/selectors/#grammar
  ;; https://drafts.csswg.org/selectors/#structure
  ;; https://drafts.csswg.org/selectors/#data-model
  (define-syntax (define-selectors stx)
    (syntax-case stx []
      [(_ [s-id #:+ S-ID rest ...] ...)
       #'(begin (struct: s-id : S-ID rest ...) ...)]))

  (define-type CSS-Namespace (HashTable Symbol String))
  (define-type CSS-Namespace-Hint (U CSS-Namespace (Listof Symbol) False))
  (define-type CSS-Selector-Combinator (U '>> '> '+ '~ '||))
  (define-type CSS-Attribute-Datum (U String Symbol (Listof (U String Symbol))))
  (define-type CSS-Attribute-Value (U CSS-Attribute-Datum (Vector Symbol CSS-Attribute-Datum)))

  (define css-root-element-type : (Parameterof Symbol) (make-parameter 'root))
  (define css-root-element-id : (Parameterof (U Keyword (Listof+ Keyword))) (make-parameter '#:root))
  
  (define-selectors
    [css-attribute-selector      #:+ CSS-Attribute-Selector ([name : Symbol] [quirk : Symbol] [namespace : (U Symbol Boolean)])]
    [css-attribute~selector      #:+ CSS-Attribute~Selector css-attribute-selector
                                 ([operator : Char]
                                  [value : (U Symbol String)]
                                  [i? : Boolean])]
    
    [css-pseudo-class-selector   #:+ CSS-Pseudo-Class-Selector ([name : Symbol] [arguments : (Option (Listof CSS-Token))])]
    [css-pseudo-element-selector #:+ CSS-Pseudo-Element-Selector
                                 ([name : Symbol]
                                  [arguments : (Option (Listof CSS-Token))]
                                  [pseudo-classes : (Listof CSS-Pseudo-Class-Selector)])]

    [css-compound-selector       #:+ CSS-Compound-Selector
                                 ([combinator : (Option CSS-Selector-Combinator)]
                                  [type : (U Symbol True)]
                                  [quirk : (U Symbol True)]
                                  [namespace : (U Symbol Boolean)]
                                  [pseudo-classes : (Listof CSS-Pseudo-Class-Selector)]
                                  [classes : (Listof Symbol)]
                                  [ids : (Listof Keyword)]
                                  [attributes : (Listof CSS-Attribute-Selector)]
                                  [pseudo-element : (Option CSS-Pseudo-Element-Selector)])]
    
    [css-complex-selector        #:+ CSS-Complex-Selector
                                 ([specificity : Nonnegative-Fixnum]
                                  [list : (Listof+ CSS-Compound-Selector)]
                                  [A : Nonnegative-Fixnum]
                                  [B : Nonnegative-Fixnum]
                                  [C : Nonnegative-Fixnum])])

  (define-preference css-subject #:as CSS-Subject
    ([type : Symbol                                       #:= (css-root-element-type)]
     [id : (U Keyword (Listof+ Keyword))                  #:= (css-root-element-id)]
     [namespace : (U Symbol Boolean)                      #:= #true]
     [classes : (Listof Symbol)                           #:= null]
     [attributes : (HashTable Symbol CSS-Attribute-Value) #:= (make-hasheq)])
    #:prefab)

  (define css-selector-match : (->* (CSS-Complex-Selector CSS-Subject) (Boolean) (Option Nonnegative-Fixnum))
    ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
    ;;; https://drafts.csswg.org/selectors/#case-sensitive
    (lambda [selector element [quirk? #false]] ; WARNING: `quirk?` only affects type name and attribute names
      (define s : CSS-Compound-Selector (last (css-complex-selector-list selector)))
      (define match? : Boolean
        (and (let ([s:type (if quirk? (css-compound-selector-quirk s) (css-compound-selector-type s))])
               (and (css-attribute-namespace-match? (css-compound-selector-namespace s) (css-subject-namespace element))
                    (or (eq? s:type #true) (eq? s:type (css-subject-type element)))))
             (let ([s:ids : (Listof Keyword) (css-compound-selector-ids s)]
                   [id : (U Keyword (Listof+ Keyword)) (css-subject-id element)])
               (cond [(null? s:ids) #true]
                     [(keyword? id) (and (null? (cdr s:ids)) (eq? (car s:ids) id))]
                     [else (set=? (list->set s:ids) (list->set id))]))
             (let ([s:classes : (Listof Symbol) (css-compound-selector-classes s)]
                   [classes : (Listof Symbol) (css-subject-classes element)])
               (for/and : Boolean ([s:c (in-list s:classes)]) (and (memq s:c classes) #true)))
             (let ([s:attrs : (Listof CSS-Attribute-Selector) (css-compound-selector-attributes s)]
                   [attrs : (HashTable Symbol CSS-Attribute-Value) (css-subject-attributes element)])
               (for/and : Boolean ([attr : CSS-Attribute-Selector (in-list s:attrs)])
                 (and (hash-has-key? attrs (if quirk? (css-attribute-selector-quirk attr) (css-attribute-selector-name attr)))
                      (let*-values ([(ns.val) (hash-ref attrs (css-attribute-selector-name attr))]
                                    [(ns datum) (cond [(not (vector? ns.val)) (values #false ns.val)]
                                                      [else (values (vector-ref ns.val 0) (vector-ref ns.val 1))])])
                        (and (css-attribute-namespace-match? (css-attribute-selector-namespace attr) ns)
                             (or (not (css-attribute~selector? attr)) ; [attr]
                                 (let* ([px:val : String (regexp-quote (~a (css-attribute~selector-value attr)))]
                                        [mode : String (if (or quirk? (css-attribute~selector-i? attr)) "i" "-i")]
                                        [val : String (if (list? datum) (string-join ((inst map String Any) ~a datum)) (~a datum))])
                                   (and (non-empty-string? px:val)
                                        (case (css-attribute~selector-operator attr)
                                          [(#\=) (regexp-match? (pregexp (format "(?~a:^~a$)" mode px:val)) val)]
                                          [(#\~) (regexp-match? (pregexp (format "(?~a:\\b~a\\b)" mode px:val)) val)]
                                          [(#\|) (regexp-match? (pregexp (format "(?~a:^~a(-|$))" mode px:val)) val)]
                                          [(#\^) (regexp-match? (pregexp (format "(?~a:^~a)" mode px:val)) val)]
                                          [(#\$) (regexp-match? (pregexp (format "(?~a:~a$)" mode px:val)) val)]
                                          [(#\*) (regexp-match? (pregexp (format "(?~a:~a)" mode px:val)) val)]
                                          [else #false])))))))))))
      (and match? (css-complex-selector-specificity selector))))
  
  (define css-selector-specificity : (-> (Listof CSS-Compound-Selector)
                                         (values Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum))
    ;;; https://drafts.csswg.org/selectors/#specificity-rules
    (lambda [complex-selector]
      (define-values (A B C)
        (for/fold ([A : Nonnegative-Fixnum 0] [B : Nonnegative-Fixnum 0] [C : Nonnegative-Fixnum 0])
                  ([static-unit (in-list complex-selector)])
          (values (fx+ A (length (css-compound-selector-ids static-unit)))
                  (fx+ B (fx+ (length (css-compound-selector-classes static-unit))
                              (fx+ (length (css-compound-selector-pseudo-classes static-unit))
                                   (length (css-compound-selector-attributes static-unit)))))
                  (fx+ C (fx+ (if (css-compound-selector-pseudo-element static-unit) 1 0)
                              (if (symbol? (css-compound-selector-type static-unit)) 1 0))))))
      (values (fxior (fxlshift A 16) (fxior (fxlshift B 8) C))
              A B C)))
  
  (define css-make-complex-selector : (-> (Listof+ CSS-Compound-Selector) CSS-Complex-Selector)
    (lambda [complex-selector]
      (define-values (specificity A B C) (css-selector-specificity complex-selector))
      (make-css-complex-selector specificity complex-selector A B C)))

  (define css-declared-namespace : (-> CSS-Namespace-Hint (U CSS:Ident CSS:Delim Symbol) (U Symbol Boolean))
    (lambda [namespaces namespace]
      (or (css:delim? namespace)        ; *
          (let ([ns (if (css:ident? namespace) (css:ident-datum namespace) namespace)])
            (if (or (false? namespaces) ; application does not care namespaces
                    (and (hash? namespaces) (hash-has-key? namespaces ns))
                    (and (list? namespaces) (memq ns namespaces) #true))
                ns #false)))))

  (define css-attribute-namespace-match? : (-> (U Symbol Boolean) (U Symbol Boolean) Boolean)
    (lambda [src ns]
      (cond [(eq? src #true) #true]
            [(false? src) (false? ns)]
            [else (eq? src ns)])))
  
  ;; https://drafts.csswg.org/css-conditional/#at-supports
  ;; https://drafts.csswg.org/mediaqueries/#media-types
  ;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  ;; https://drafts.csswg.org/mediaqueries/#mq-features
  (define-type CSS-Media-Query (U CSS-Media-Type CSS-Feature-Query (Pairof CSS-Media-Type CSS-Feature-Query)))
  (define-type CSS-Feature-Query (U CSS-Not CSS-And CSS-Or CSS-Media-Feature CSS-Declaration Symbol CSS-Syntax-Error))
  (define-type CSS-Media-Value (U CSS-Numeric CSS:Ident CSS:Ratio))
  (define-type CSS-Media-Datum (U Symbol Integer Flonum))

  (struct: css-media-type : CSS-Media-Type ([name : Symbol] [only? : Boolean]))
  (struct: css-media-feature : CSS-Media-Feature ([name : Symbol] [value : CSS-Media-Datum] [operator : Char]))
  (struct: css-not : CSS-Not ([condition : CSS-Feature-Query]))
  (struct: css-and : CSS-And ([conditions : (Listof CSS-Feature-Query)]))
  (struct: css-or : CSS-Or ([conditions : (Listof CSS-Feature-Query)]))

  ;; https://drafts.csswg.org/mediaqueries/#media-descriptor-table
  ;; https://drafts.csswg.org/mediaqueries/#mf-deprecated
  (define-type CSS-Feature-Support? (-> Symbol (Listof+ CSS-Token) Boolean))
  (define-type CSS-Media-Preferences (HashTable Symbol CSS-Media-Datum))
  (define-type CSS-Media-Feature-Filter (-> Symbol Boolean (-> Void) (U Void (CSS:Filter CSS-Media-Datum))))

  (define css-media-feature-filter : CSS-Media-Feature-Filter
    (lambda [downcased-name min/max? deprecated!]
      (case downcased-name
        [(width height device-width device-height resolution)
         (when (or (eq? downcased-name 'device-width) (eq? downcased-name 'device-height)) (deprecated!))
         (css+length-filter #true)]
        [(aspect-ratio device-aspect-ratio)
         (when (eq? downcased-name 'device-aspect-ratio) (deprecated!))
         (CSS:<•> (css:ratio-filter) real->double-flonum)]
        [(resolution) (CSS:<+> (CSS:<=> (css-keyword-filter 'infinite) +inf.0) (css+resolution-filter))]
        [(color color-index monochrome) (css:integer-filter exact-nonnegative-integer?)]
        [(grid) #|legacy descriptor|# (when (false? min/max?) (css-boolean-filter))]
        [(orientation) (css-keyword-filter '(portrait landscape))]
        [(scan) (css-keyword-filter '(interlace progressive))]
        [(update) (css-keyword-filter '(none slow fast))]
        [(overflow-block) (css-keyword-filter '(none scroll optional-paged paged))]
        [(overflow-inline) (css-keyword-filter '(none scroll))]
        [(color-gamut) (css-keyword-filter '(srgb p3 rec2020))]
        [(pointer any-pointer) (css-keyword-filter '(none coarse fine))]
        [(havor any-havor) (css-keyword-filter '(none havor))]
        [(scripting) (css-keyword-filter '(none initial-only enabled))])))

  (define css-deprecate-media-type : (Parameterof Boolean) (make-parameter #false))
  (define current-css-media-type : (Parameterof Symbol) (make-parameter 'all))
  
  (define-values (current-css-media-preferences current-css-media-feature-filter current-css-feature-support?)
    (values (make-parameter ((inst make-hasheq Symbol CSS-Media-Datum)))
            (make-parameter css-media-feature-filter)
            (make-parameter (const #false))))

  ;; https://drafts.csswg.org/css-cascade/#shorthand
  ;; https://drafts.csswg.org/css-cascade/#filtering
  ;; https://drafts.csswg.org/css-cascade/#cascading
  (define-type CSS-Datum
    (Rec css (U CSS-Token-Datum Bytes FlVector FxVector CSS-Token Null --datum (Object)
                (Pairof css css) (Vectorof css) (Boxof css))))
  
  (define-type CSS-Longhand-Values (HashTable Symbol CSS-Datum))
  (define-type CSS-Variable-Values (HashTable Symbol (U CSS-Declaration Null)))
  (define-type (CSS-Cascaded-Value-Filter Preference) (-> CSS-Values Preference (Option CSS-Values) Preference))
  (define-type CSS-Declaration-Filter (-> Symbol (-> Void)
                                          (U (CSS-Parser (Listof CSS-Datum)) Void False
                                             (Pairof (CSS-Parser CSS-Longhand-Values) (Listof Symbol)))))

  (struct --datum () #:transparent)
  (define initial-longhand : CSS-Longhand-Values (make-immutable-hasheq))

  (define-syntax (define-css-value stx)
    (syntax-case stx [:]
      [(_ datum #:as Datum (fields ...) options ...)
       #'(define-css-value datum #:as Datum #:=> --datum (fields ...) options ...)]
      [(_ datum #:as Datum #:=> parent (fields ...) options ...)
       #'(begin (define-type Datum datum)
                (struct datum parent (fields ...) #:transparent options ...))]))

  (define-syntax (define-prefab-keyword stx)
    (syntax-case stx [:]
      [(_ css-wide-keyword #:as CSS-Wide-Keyword [keyword ...])
       (with-syntax ([(keywords-ormap keywords-filter-map css:keyword ...)
                      (list* (format-id #'css-wide-keyword "~as-ormap" (syntax-e #'css-wide-keyword))
                             (format-id #'css-wide-keyword "~as-filter-map" (syntax-e #'css-wide-keyword))
                             (for/list ([kwd (in-list (syntax->list #'(keyword ...)))])
                               (format-id kwd "css:~a" (syntax-e kwd))))])
         #'(begin (define-css-value css-wide-keyword #:as CSS-Wide-Keyword ([value : Symbol]))
                  (define css:keyword : CSS-Wide-Keyword (css-wide-keyword 'keyword)) ...

                  (define keywords-ormap : (-> (U Symbol CSS-Token) (Option CSS-Wide-Keyword))
                    (lambda [key]
                      (cond [(css-token? key) (and (css:ident? key) (keywords-ormap (css:ident-norm key)))]
                            [(eq? key 'keyword) css:keyword] ...
                            [else #false])))

                  (define keywords-filter-map : (-> (U Symbol CSS-Syntax-Error) (U Symbol CSS-Wide-Keyword CSS-Syntax-Error))
                    (lambda [key]
                      (cond [(not (symbol? key)) key]
                            [(eq? key 'keyword) css:keyword] ...
                            [else key])))))]))

  ; NOTE: `css-lazy` must hold at least one token(even it's just a <whitespace>),
  ;        since `null` indicates an invalid variable when substituting.
  (define-css-value css-lazy #:as CSS-Lazy ([property : CSS:Ident] [force : (Listof+ CSS-Token)] [recursive? : Boolean]))
  (define-css-value css-@λ #:as CSS-@λ ([sexp : (Pairof Symbol (Listof CSS-Datum))]))

  ; https://drafts.csswg.org/css-cascade/#all-shorthand
  ; https://drafts.csswg.org/css-values/#common-keywords
  (define current-css-all-exceptions : (Parameterof (Listof Symbol)) (make-parameter (list 'direction 'unicode-bidi)))
  (define-prefab-keyword css-wide-keyword #:as CSS-Wide-Keyword [initial inherit unset revert])
  
  (define-syntax (call-with-css-media stx)
    (syntax-parse stx
      [(_ (~optional (~seq #:preferences ?-preferences)) sexp ...)
       (with-syntax ([preferences (or (attribute ?-preferences) #'(current-css-media-preferences))])
         #'(let ([w (hash-ref preferences 'width (thunk #false))]
                 [h (hash-ref preferences 'height (thunk #false))])
             (when (and (real? w) (positive? w)) (set-flcss%-vw! length% (real->double-flonum w)))
             (when (and (real? h) (positive? h)) (set-flcss%-vh! length% (real->double-flonum h)))
             sexp ...))]))

  (define-syntax (css-tee-computed-value stx)
    (syntax-case stx []
      [(_ properties desc-name cascaded-value computed-value-sexp)
       #'(let([computed-value computed-value-sexp])
           (when (and (not (eq? cascaded-value computed-value))
                      (implies (object? computed-value) (css-cache-computed-object-value)))
             (hash-set! properties desc-name (thunk computed-value)))
           computed-value)]))

  (define-preference flcss% #:as FlCSS%
    ([vw : Nonnegative-Flonum  #:= 1440.0]
     [vh : Nonnegative-Flonum  #:= 820.0]
     [rem : Nonnegative-Flonum #:= 12.0]
     [em : Nonnegative-Flonum  #:= 12.0]
     [ex : Nonnegative-Flonum  #:= 8.0]
     [ch : Nonnegative-Flonum  #:= 8.0]
     [ic : Nonnegative-Flonum  #:= 12.0])
    #:transparent
    #:mutable)

  (define-preference css-values #:as CSS-Values
    ([descriptors : (HashTable Symbol (-> CSS-Datum)) #:= (make-hasheq)]
     [variables : CSS-Variable-Values                 #:= (make-hasheq)]
     [importants : (HashTable Symbol Boolean)         #:= (make-hasheq)])
    #:transparent)

  (define length% : FlCSS% (make-flcss%))
  (define css-cache-computed-object-value : (Parameterof Boolean) (make-parameter #true))
  
  (define css-ref : (All (a b) (case-> [CSS-Values (Option CSS-Values) Symbol -> CSS-Datum]
                                       [CSS-Values (Option CSS-Values) Symbol (-> Symbol CSS-Datum (∩ a CSS-Datum)) -> a]
                                       [CSS-Values (Option CSS-Values) Symbol (-> Any Boolean : #:+ (∩ a CSS-Datum)) (∩ b CSS-Datum)
                                                   -> (U a b)]))
    (case-lambda
      [(declared-values inherited-values desc-name)
       (define properties : (HashTable Symbol (-> CSS-Datum)) (css-values-descriptors declared-values))
       (define-values (cascaded-value specified-value) (css-ref-raw properties inherited-values desc-name))
       specified-value]
      [(declared-values inherited-values desc-name datum->value)
       (define properties : (HashTable Symbol (-> CSS-Datum)) (css-values-descriptors declared-values))
       (define-values (cascaded-value specified-value) (css-ref-raw properties inherited-values desc-name))
       (css-tee-computed-value properties desc-name cascaded-value (datum->value desc-name specified-value))]
      [(declared-values inherited-values desc-name datum? default-value)
       (define properties : (HashTable Symbol (-> CSS-Datum)) (css-values-descriptors declared-values))
       (define-values (cascaded-value specified-value) (css-ref-raw properties inherited-values desc-name))
       (css-tee-computed-value properties desc-name cascaded-value
                               (cond [(datum? specified-value) specified-value]
                                     [else default-value]))]))

  (define css-ref-raw : (-> (HashTable Symbol (-> CSS-Datum)) (Option CSS-Values) Symbol (Values CSS-Datum CSS-Datum))
    (lambda [properties inherited-values desc-name]
      (define declared-value : (-> CSS-Datum)
        (hash-ref properties desc-name
                  (thunk (cond [(memq desc-name (current-css-all-exceptions)) (thunk css:unset)]
                               [else (hash-ref properties 'all (thunk (thunk css:unset)))]))))
      (define cascaded-value : CSS-Datum (declared-value))
      (define specified-value : CSS-Datum
        (cond [(or (eq? cascaded-value css:revert) (not (css-wide-keyword? cascaded-value))) cascaded-value]
              [(or (eq? cascaded-value css:initial) (false? inherited-values)) css:initial]
              [else (let-values ([(_ sv) (css-ref-raw (css-values-descriptors inherited-values) #false desc-name)]) sv)]))
      (unless (eq? cascaded-value specified-value)
        (hash-set! properties desc-name (thunk specified-value)))
      (values cascaded-value specified-value)))

  ;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
  (define css-viewport-descriptor-filter : CSS-Declaration-Filter
    (lambda [suitcased-name !]
      (define-css-filter viewport-length-filter #:-> (U Symbol Nonnegative-Inexact-Real)
        (css-keyword-filter 'auto)
        (css:percentage-filter nonnegative-single-flonum?)
        (css+length-filter #true))
      (case suitcased-name
        [(width)
         (cons (CSS<@> (λ [[longhand : (HashTable Symbol CSS-Datum)]]
                         (cond [(hash-has-key? longhand 'max-width) longhand]
                               [else (hash-set longhand 'max-width (hash-ref longhand 'min-width))]))
                       (CSS<#> (viewport-length-filter) 'min-width)
                       (CSS<?> (CSS<#> (viewport-length-filter) 'max-width)))
               (list 'min-width 'max-width))]
        [(height)
         (cons (CSS<@> (λ [[longhand : (HashTable Symbol CSS-Datum)]]
                         (cond [(hash-has-key? longhand 'max-height) longhand]
                               [else (hash-set longhand 'max-height (hash-ref longhand 'min-height))]))
                       (CSS<#> (viewport-length-filter) 'min-height)
                       (CSS<?> (CSS<#> (viewport-length-filter) 'max-height)))
               (list 'min-height 'max-height))]
        [(zoom min-zoom max-zoom)
         (CSS<^> (CSS:<+> (CSS:<•> (css+%real-filter) real->double-flonum)
                          (css-keyword-filter 'auto)))]
        [(min-width max-width min-height max-height) (CSS<^> (viewport-length-filter))]
        [(orientation) (CSS<^> (css-keyword-filter '(auto portrait landscape)))]
        [(user-zoom) (CSS<^> (css-keyword-filter '(zoom fixed)))]
        [else #false])))

  (define css-viewport-filter : (CSS-Cascaded-Value-Filter (HashTable Symbol CSS-Media-Datum))
    ;;; https://drafts.csswg.org/css-device-adapt/#constraining
    ;;; https://drafts.csswg.org/css-device-adapt/#handling-auto-zoom
    ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
    ;;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
    (lambda [cascaded-values initial-viewport inherited-viewport]
      ; Notes: We do not check the `initial-viewport` to specific the `specified values` since
      ;          @viewport is a controversial @rule which is easy to be used incorrectly,
      ;          @viewport is rarely used in desktop applications, and
      ;          this behavior is indeed specified if I understand the specification correctly.
      (define-values (initial-width initial-height)
        (let ([w (hash-ref initial-viewport 'width (const #false))]
              [h (hash-ref initial-viewport 'height (const #false))])
          (values (if (real? w) (flmax (real->double-flonum w) 1.0) 1.0)
                  (if (real? h) (flmax (real->double-flonum h) 1.0) 1.0))))
      (define defzoom : Nonnegative-Flonum (current-css-viewport-auto-zoom))
      (define (smart [maix : (-> Flonum Flonum Flonum)] [v1 : (U Flonum Symbol)] [v2 : (U Flonum Symbol)]) : (U Flonum Symbol)
        (cond [(and (symbol? v1) (symbol? v2)) 'auto]
              [(symbol? v1) v2]
              [(symbol? v2) v1]
              [else (maix v1 v2)]))
      (define datum->width (css-make-datum->size #:100% initial-width #:= 'auto))
      (define datum->height (css-make-datum->size #:100% initial-height #:= 'auto))
      (define min-zoom : Flonum (css-ref cascaded-values #false 'min-zoom nonnegative-flonum? 0.0))
      (define max-zoom : Flonum (flmax min-zoom (css-ref cascaded-values #false 'max-zoom nonnegative-flonum? +inf.0)))
      (define min-width : (U Flonum Symbol) (css-ref cascaded-values #false 'min-width datum->width))
      (define max-width : (U Flonum Symbol) (css-ref cascaded-values #false 'max-width datum->width))
      (define min-height : (U Flonum Symbol) (css-ref cascaded-values #false 'min-height datum->height))
      (define max-height : (U Flonum Symbol) (css-ref cascaded-values #false 'max-height datum->height))
      (define-values (width height)
        (let* ([width (smart flmax min-width (smart flmin max-width initial-width))]
               [height (smart flmax min-height (smart flmin max-height initial-height))]
               [width (cond [(and (symbol? width) (symbol? height)) initial-width]
                            [(symbol? width) (if (zero? initial-height) initial-width (fl* height (fl/ initial-width initial-height)))]
                            [else width])])
          (values width (cond [(flonum? height) height]
                              [(zero? initial-width) initial-height]
                              [else (fl* width (fl/ initial-height initial-width))]))))
      (define actual-viewport (hash-copy initial-viewport))
      (for ([name (in-list      '(min-zoom max-zoom width height))]
            [value (in-list (list min-zoom max-zoom width height))])
        (hash-set! actual-viewport name value))
      (hash-set! actual-viewport 'orientation (css-ref cascaded-values #false 'orientation symbol? 'auto))
      (hash-set! actual-viewport 'user-zoom (css-ref cascaded-values #false 'user-zoom symbol? 'zoom))
      (hash-set! actual-viewport 'zoom (max min-zoom (min max-zoom (css-ref cascaded-values #false 'zoom nonnegative-flonum? defzoom))))
      actual-viewport))

  (define-values (current-css-viewport-descriptor-filter current-css-viewport-filter current-css-viewport-auto-zoom)
    (values (make-parameter css-viewport-descriptor-filter)
            (make-parameter css-viewport-filter)
            (ann (make-parameter 1.0) (Parameterof Nonnegative-Flonum))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-car/cdr : (All (a b) (case-> [(Pairof a b) -> (Values a b)]
                                           [(Listof a) -> (Values (U a EOF) (Listof a))]))
    (lambda [pretent-no-whitespace]
      (cond [(null? pretent-no-whitespace) (values eof null)]
            [else (values (car pretent-no-whitespace) (cdr pretent-no-whitespace))])))

  (define css-car/cadr : (All (a) (case-> [(Pairof a (Listof a)) -> (Values a (Listof a) (U a EOF) (Listof a))]
                                          [(Listof a) -> (Values (U a EOF) (Listof a) (U a EOF) (Listof a))]))
    (lambda [pretent-no-whitespace]
      (cond [(null? pretent-no-whitespace) (values eof null eof null)]
            [else (let ([1st (car pretent-no-whitespace)]
                        [2nd (cdr pretent-no-whitespace)])
                    (cond [(null? 2nd) (values 1st null eof null)]
                          [else (values 1st 2nd (car 2nd) (cdr 2nd))]))])))

  (define css-car : (-> (Listof CSS-Token) (Values (U CSS-Token EOF) (Listof CSS-Token)))
    (lambda [dirty]
      (let skip-whitespace ([rest dirty])
        (cond [(null? rest) (values eof null)]
              [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                      (cond [(css:whitespace? head) (skip-whitespace tail)]
                            [else (values head tail)]))]))))
  
  (define css-null? : (-> (Listof Any) Boolean)
    (lambda [dirty]
      (let skip-whitespace : Boolean ([rest dirty])
        (or (null? rest)
            (and (css:whitespace? (car rest))
                 (skip-whitespace (cdr rest)))))))

  (define css-pair? : (-> (Listof CSS-Token) Boolean)
    (lambda [dirty]
      (let skip-whitespace : Boolean ([rest dirty])
        (cond [(null? rest) #false]
              [else (implies (css:whitespace? (car rest))
                             (skip-whitespace (cdr rest)))]))))

  (define css-cons : (All (css) (-> (U CSS-Syntax-Error css) (Listof css) (Listof css)))
    (lambda [item items]
      (cond [(exn? item) items]
            [else (cons item items)]))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module tokenizer typed/racket ;;; https://drafts.csswg.org/css-syntax/#tokenization
  (provide (all-defined-out))

  (require (submod ".." digitama))

  (struct css-srcloc ([in : Input-Port] [source : (U String Symbol)] [line : Natural] [col : Natural] [pos : Natural])
    #:type-name CSS-Srcloc)

  (define-syntax (css-make-token stx)
    (syntax-case stx []
      [(_ src make-css:token datum ...)
       #'(let-values ([(start-position) (css-srcloc-pos src)]
                      [(line column position) (port-next-location (css-srcloc-in src))])
           (make-css:token (css-srcloc-source src) (css-srcloc-line src) (css-srcloc-col src)
                           start-position (max (- (or position 0) start-position) 0)
                           datum ...))]))
  
  (define-syntax (css-make-bad-token stx)
    (syntax-case stx []
      [(_ src css:bad:sub token datum)
       #'(let ([bad (css-make-token src css:bad:sub (cons (assert (object-name token) symbol?) datum))])
           (log-message (current-logger) 'debug 'exn:css:read (css-token->string bad) bad)
           bad)]))
  
  (define css-consume-token : (-> Input-Port (U String Symbol) (U EOF CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#error-handling
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-token
    (lambda [/dev/cssin source]
      (define-values (line column position) (port-next-location /dev/cssin))
      (define srcloc (css-srcloc /dev/cssin source (or line 0) (or column 0) (or position 0)))
      (define ch (read-char /dev/cssin))
      (cond [(eof-object? ch) eof]
            [(char-whitespace? ch) (css-consume-whitespace-token srcloc)]
            [(char-numeric? ch) (css-consume-numeric-token srcloc ch)]
            [(css-char-name-prefix? ch) (css-consume-ident-token srcloc ch)]
            [else (case ch
                    [(#\) #\] #\}) (css-make-token srcloc css:close ch)]
                    [(#\' #\") (css-consume-string-token srcloc ch)]
                    [(#\+ #\.) (css-consume-numeric-token srcloc ch)]
                    [(#\^ #\$ #\| #\~ #\*) (css-consume-match-token srcloc ch)]
                    [(#\#) (css-consume-hash-token srcloc)]
                    [(#\@) (css-consume-@keyword-token srcloc)]
                    [(#\/) (css-consume-comment-token srcloc)]
                    [(#\< #\-) (css-consume-cd-token srcloc ch)]
                    [(#\null) (css-make-token srcloc css:delim #\uFFFD)]
                    [(#\;) (css-make-token srcloc css:semicolon #\;)]
                    [(#\,) (css-make-token srcloc css:comma #\,)]
                    [(#\:) (css-make-token srcloc css:colon #\:)]
                    [else (css-make-token srcloc css:delim ch)])])))

  (define css-consume-cd-token : (-> CSS-Srcloc Char CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#CDO-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#CDC-token-diagram
    (lambda [srcloc open/close]
      (define css : Input-Port (css-srcloc-in srcloc))
      (if (char=? open/close #\<)
          (let ([cdo : (U EOF String) (peek-string 3 0 css)])
            (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 css) (css-make-token srcloc css:cdo '<!--)]
                  [else (css-make-token srcloc css:delim #\<)]))
          (let ([cdc : (U EOF String) (peek-string 2 0 css)])
            (cond [(eof-object? cdc) (css-make-token srcloc css:delim #\-)]
                  [(string=? cdc "->") (read-string 2 css) (css-make-token srcloc css:cdc '-->)]
                  [(css-identifier-prefix? #\- (string-ref cdc 0) (string-ref cdc 1)) (css-consume-ident-token srcloc #\-)]
                  [else (css-consume-numeric-token srcloc #\-)])))))

  (define css-consume-comment-token : (-> CSS-Srcloc (U CSS:WhiteSpace CSS:Delim CSS:Bad))
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (css-make-token srcloc css:slash #\/)]
            [(regexp-match #px".*?\\*/" css) => (λ [**/] (css-make-token srcloc css:whitespace (format "/~a" (car **/))))]
            [else (css-make-bad-token srcloc css:bad:eof struct:css:whitespace "/*")])))

  (define css-consume-whitespace-token : (-> CSS-Srcloc CSS:WhiteSpace)
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (css-make-token srcloc css:whitespace #\space)))
  
  (define css-consume-ident-token : (-> CSS-Srcloc Char (U CSS:Ident CSS:Function CSS:URL CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-ident-like-token
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
    ;;; https://drafts.csswg.org/css-values/#urls
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    (lambda [srcloc id0]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                  (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
             (read-char css) (css-consume-unicode-range-token srcloc)]
            [else (let ([name (css-consume-name css id0)])
                    (define ch : (U EOF Char) (peek-char css))
                    (cond [(or (eof-object? ch) (not (char=? ch #\()))
                           (if (and (char=? id0 #\-) (eqv? id0 ch1))
                               (let ([--id (string->unreadable-symbol name)]) (css-make-token srcloc css:ident --id --id))
                               (css-make-token srcloc css:ident (string->symbol name) (string->symbol (string-downcase name))))]
                          [(and (or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" css)) (read-char css))
                           (define fnorm : Symbol (string->symbol (string-downcase name)))
                           (css-make-token srcloc css:function (string->unreadable-symbol name) fnorm null #false)]
                          [else (read-char css) (css-consume-url-token srcloc)]))])))
      
  (define css-consume-string-token : (-> CSS-Srcloc Char (U CSS:String CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-string-token
    (lambda [srcloc quotation]
      (define css : Input-Port (css-srcloc-in srcloc))
      (let consume-string-token : (U CSS:String CSS:Bad) ([chars : (Listof Char) null])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch quotation))
               (when (eof-object? ch) (css-make-bad-token srcloc css:bad:eof struct:css:string (list->string (reverse chars))))
               (css-make-token srcloc css:string (list->string (reverse chars)))]
              [(char=? ch #\newline) (css-make-bad-token srcloc css:bad:eol struct:css:string (list->string (reverse chars)))]
              [(not (char=? ch #\\)) (consume-string-token (cons ch chars))]
              [else (let ([next (peek-char css)])
                      (cond [(eof-object? next) (consume-string-token chars)]
                            [(and (char=? next #\newline) (read-char css)) (consume-string-token (cons ch chars))]
                            [else (consume-string-token (cons (css-consume-escaped-char css) chars))]))]))))

  (define css-consume-numeric-token : (-> CSS-Srcloc Char (U CSS-Numeric CSS:Delim CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    ;;; https://drafts.csswg.org/css-values/#numeric-types
    (lambda [srcloc sign/digit]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (cond [(not (css-number-prefix? sign/digit ch1 ch2)) (css-make-token srcloc css:delim sign/digit)]
            [else (let-values ([(n representation) (css-consume-number css sign/digit)])
                    (let ([ch1 : (U EOF Char) (peek-char css 0)]
                          [ch2 : (U EOF Char) (peek-char css 1)]
                          [ch3 : (U EOF Char) (peek-char css 2)])
                      (cond [(css-identifier-prefix? ch1 ch2 ch3)
                             (define unit : Symbol (string->symbol (string-downcase (css-consume-name css #false))))
                             (define value : Flonum (real->double-flonum n))
                             (case unit
                               [(em ex ch ic rem)       (css-make-token srcloc css:length:font     representation value unit)]
                               [(vw vh vi vb vmin vmax) (css-make-token srcloc css:length:viewport representation value unit)]
                               [(px cm mm q in pc pt)   (css-make-token srcloc css:length          representation value unit)]
                               [(deg grad rad turn)     (css-make-token srcloc css:angle           representation value unit)]
                               [(s ms min h)            (css-make-token srcloc css:time            representation value unit)]
                               [(hz khz)                (css-make-token srcloc css:frequency       representation value unit)]
                               [(dpi dpcm dppx)         (css-make-token srcloc css:resolution      representation value unit)]
                               [else                    (css-make-token srcloc css:dimension       representation value unit)])]
                            [(and (char? ch1) (char=? ch1 #\%) (read-char css))
                             (define n% : Single-Flonum (real->single-flonum (* n 0.01)))
                             (css-make-token srcloc css:percentage representation n%)]
                            [(flonum? n)
                             (cond [(zero? n) (css-make-token srcloc css:flzero representation n)]
                                   [(fl= n 1.0) (css-make-token srcloc css:flone representation n)]
                                   [else (css-make-token srcloc css:flonum representation n)])]
                            [(zero? n) (css-make-token srcloc css:zero representation n)]
                            [(= n 1) (css-make-token srcloc css:one representation n)]
                            [else (css-make-token srcloc css:integer representation n)])))])))

  (define css-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-url-token
    ;;; https://drafts.csswg.org/css-values/#urls
    ;;; https://drafts.csswg.org/css-values/#url-empty
    ;;; https://drafts.csswg.org/css-values/#about-invalid
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (css-consume-whitespace css)
      (let consume-url-token ([srahc : (Listof Char) null])
        (define ch : (U EOF Char) (read-char css))
        (cond [(or (eof-object? ch) (char=? ch #\)))
               (define uri : (U String 'about:invalid) (if (null? srahc) 'about:invalid (list->string (reverse srahc))))
               (when (eof-object? ch) (css-make-bad-token srcloc css:bad:eof struct:css:url uri))
               (css-make-token srcloc css:url uri null #false)]
              [(and (char-whitespace? ch) (css-consume-whitespace css))
               (define end : (U EOF Char) (read-char css))
               (define uri : (U String 'about:invalid) (if (null? srahc) 'about:invalid (list->string (reverse srahc))))
               (cond [(or (eof-object? end) (char=? end #\))) (css-make-token srcloc css:url uri null #false)]
                     [else (css-consume-bad-url-remnants css (css-make-bad-token srcloc css:bad:blank struct:css:url uri))])]
              [(css-valid-escape? ch (peek-char css)) (consume-url-token (cons (css-consume-escaped-char css) srahc))]
              [(or (memq ch '(#\\ #\" #\' #\()) (css-char-non-printable? ch))
               (css-consume-bad-url-remnants css (css-make-bad-token srcloc css:bad:char struct:css:url ch))]
              [else (consume-url-token (cons ch srahc))]))))

  (define css-consume-unicode-range-token : (-> CSS-Srcloc (U CSS:URange CSS:Bad))
    ;;; https://drafts.csswg.org/css-syntax/#urange-syntax
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define-values (n rest) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
      (define-values (start0 end0)
        (let consume-? : (Values Fixnum Fixnum) ([s : Fixnum n] [e : Fixnum n] [? : Fixnum rest])
          (cond [(zero? ?) (values s e)]
                [else (let ([ch : (U EOF Char) (peek-char css)])
                        (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                              [else (read-char css) (consume-? (fxlshift s 4)
                                                               (fxior (fxlshift e 4) #xF)
                                                               (fx- ? 1))]))])))
      (define-values (start end)
        (cond [(not (fx= start0 end0)) (values start0 end0)]
              [else (let ([ch1 (peek-char css 0)]
                          [ch2 (peek-char css 1)])
                      (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2) (read-char css))
                             (define-values (end _) (css-consume-hexadecimal (css-srcloc-in srcloc) 6))
                             (values start0 end)]
                            [else (values start0 start0)]))]))
      (cond [(and (index? start) (index? end) (<= start end #x10FFFF)) (css-make-token srcloc css:urange (cons start end))]
            [(> end #x10FFFF) (css-make-bad-token srcloc css:bad:range struct:css:urange end)]
            [else (css-make-bad-token srcloc css:bad:range struct:css:urange (cons start end))])))

  (define css-consume-hash-token : (-> CSS-Srcloc (U CSS:Hash CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#hash-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (or (css-char-name? ch1) (css-valid-escape? ch1 ch2))
          (let ([name (css-consume-name (css-srcloc-in srcloc) #false)])
            (css-make-token srcloc css:hash (string->keyword name) #|(string->keyword (string-downcase name))|#))
          (css-make-token srcloc css:delim #\#))))

  (define css-consume-@keyword-token : (-> CSS-Srcloc (U CSS:@Keyword CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#at-keyword-token-diagram
    (lambda [srcloc]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch1 : (U EOF Char) (peek-char css 0))
      (define ch2 : (U EOF Char) (peek-char css 1))
      (define ch3 : (U EOF Char) (peek-char css 2))
      (if (css-identifier-prefix? ch1 ch2 ch3)
          (let ([name (css-consume-name (css-srcloc-in srcloc) #\@)])
            (css-make-token srcloc css:@keyword (string->keyword name) (string->keyword (string-downcase name))))
          (css-make-token srcloc css:delim #\@))))

  (define css-consume-match-token : (-> CSS-Srcloc Char (U CSS:Match CSS:Delim))
    ;;; https://drafts.csswg.org/css-syntax/#include-match-token-diagram
    ;;; https://drafts.csswg.org/css-syntax/#column-token-diagram
    (lambda [srcloc prefix]
      (define css : Input-Port (css-srcloc-in srcloc))
      (define ch : (U EOF Char) (peek-char css))
      (cond [(and (eq? prefix #\|) (eq? ch #\|) (read-char css)) (css-make-token srcloc css:delim #\tab)]
            [(and (eq? ch #\=) (read-char css)) (css-make-token srcloc css:match prefix)]
            [(eq? prefix #\|) (css-make-token srcloc css:vbar prefix)]
            [else (css-make-token srcloc css:delim prefix)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-whitespace : (-> Input-Port Void)
    (lambda [css]
      (regexp-match #px"\\s*" css)
      (void)))
  
  (define css-consume-name : (-> Input-Port (Option Char) String)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-name
    (lambda [css ?-head]
      (let consume-name ([srahc : (Listof Char) (if ?-head (list ?-head) null)])
        (define ch : (U EOF Char) (peek-char css))
        (cond [(and (css-char-name? ch) (read-char css)) (consume-name (cons ch srahc))]
              [(and (css-valid-escape? ch (peek-char css 1)) (read-char css)) (consume-name (cons (css-consume-escaped-char css) srahc))]
              [else (list->string (reverse srahc))]))))

  (define css-consume-number : (-> Input-Port Char (Values (U Flonum Integer) String))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [css sign/digit]
      (let consume-number ([chars (list sign/digit)])
        (define ch : (U EOF Char) (peek-char css))
        (cond [(and (char? ch)
                    (or (char-numeric? ch)
                        (char=? ch #\+) (char=? ch #\-)
                        (css-decimal-point? ch (peek-char css 1))
                        (css-scientific-notation? ch (peek-char css 1) (peek-char css 2)))
                    (read-char css))
               (consume-number (cons ch chars))]
              [else (let* ([representation : String (list->string (reverse chars))]
                           [?-number : (Option Complex) (string->number representation)])
                      (cond [(exact-integer? ?-number) (values ?-number representation)]
                            [(flonum? ?-number) (values ?-number representation)]
                            [else (values +nan.0 representation)]))]))))

  (define css-consume-hexadecimal : (->* (Input-Port Byte) (Fixnum #:\s?$? Boolean) (Values Fixnum Byte))
    (lambda [css --count [result 0] #:\s?$? [eat-last-whitespace? #false]]
      (define hex : (U EOF Char) (peek-char css))
      (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? --count))
             (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char css))
             (values result --count)]
            [else (read-char css) (css-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                                           css (fx- --count 1)
                                                           (fx+ (fxlshift result 4)
                                                                (char->hexadecimal hex)))])))

  (define css-consume-escaped-char : (-> Input-Port Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-escaped-code-point
    (lambda [css]
      (define esc : (U EOF Char) (read-char css))
      (cond [(eof-object? esc) #\uFFFD]
            [(not (char-hexdigit? esc)) esc]
            [else (let-values ([(hex _) (css-consume-hexadecimal css (sub1 6) (char->hexadecimal esc) #:\s?$? #true)])
                    (cond [(or (fx<= hex 0) (fx> hex #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                          [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                          [else (integer->char hex)]))])))

  (define css-consume-bad-url-remnants : (-> Input-Port CSS:Bad CSS:Bad)
    ;;; https://drafts.csswg.org/css-syntax/#consume-the-remnants-of-a-bad-url
    (lambda [css bad-url-token]
      (define ch : (U EOF Char) (read-char css))
      (cond [(or (eof-object? ch) (char=? ch #\))) bad-url-token]
            [(and (char=? ch #\\) (read-char css)) (css-consume-bad-url-remnants css bad-url-token)]
            [else (css-consume-bad-url-remnants css bad-url-token)])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define char-hexdigit? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char-numeric? ch)
               (char-ci<=? #\a ch #\f)))))

  (define char->hexadecimal : (-> Char Fixnum)
    (lambda [hexch]
      (cond [(char<=? #\a hexch) (fx- (char->integer hexch) #x57)]
            [(char<=? #\A hexch) (fx- (char->integer hexch) #x37)]
            [else (fx- (char->integer hexch) #x30)])))

  (define css-char-non-printable? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char<=? #\null ch #\backspace)
               (char=? ch #\vtab)
               (char<=? #\u000E ch #\u001F)
               (char=? ch #\rubout)))))

  (define css-char-name-prefix? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (char-lower-case? ch)
               (char-upper-case? ch)
               (char=? #\_  ch)
               (char>=? ch #\u0080)))))

  (define css-char-name? : (-> (U EOF Char) Boolean : #:+ Char)
    (lambda [ch]
      (and (char? ch)
           (or (css-char-name-prefix? ch)
               (char-numeric? ch)
               (char=? #\- ch)))))
  
  (define css-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#escaping
    ;;; https://drafts.csswg.org/css-syntax/#starts-with-a-valid-escape
    (lambda [ch1 ch2]
      (and (char? ch1)
           (char=? ch1 #\\)
           (or (eof-object? ch2)
               (not (char=? ch2 #\newline))))))

  (define css-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#would-start-an-identifier
    (lambda [ch1 ch2 ch3]
      (or (css-char-name-prefix? ch1)
          (css-valid-escape? ch1 ch2)
          (and (char? ch1) (char=? ch1 #\-)
               (or (css-char-name-prefix? ch2)
                   (and (char? ch2) (char=? ch2 #\-))
                   (css-valid-escape? ch2 ch3))))))

  (define css-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#starts-with-a-number
    (lambda [ch1 ch2 ch3]
      (or (and (char? ch1) (char<=? #\0 ch1 #\9))
          (and (char? ch1) (char? ch2)
               (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
          (and (char? ch1) (char? ch2)
               (or (char=? ch1 #\+) (char=? ch1 #\-))
               (or (char<=? #\0 ch2 #\9)
                   (and (char=? ch2 #\.)
                        (char? ch3) (char<=? #\0 ch3 #\9)))))))

  (define css-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [ch1 ch2 ch3]
      (and (char? ch1) (char? ch2)
           (char-ci=? ch1 #\e)
           (or (char<=? #\0 ch2 #\9)
               (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                    (char? ch3)
                    (char<=? #\0 ch3 #\9))))))

  (define css-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-number
    (lambda [ch1 ch2]
      (and (char? ch1) (char? ch2)
           (char=? ch1 #\.)
           (char<=? #\0 ch2 #\9))))

  (define css-fallback-charset : (-> String String)
    (lambda [from]
      (define CHARSET : String (string-upcase from))
      (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
            [else CHARSET])))
  
  (define css-fallback-encode-input-port : (-> Input-Port Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#input-byte-stream
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    (lambda [/dev/rawin]
      (define magic : (U EOF String) (peek-string 1024 0 /dev/rawin))
      (cond [(eof-object? magic) /dev/rawin]
            [(let ([charset? (regexp-match #px"^@charset \"(.*?)\";" magic)])
               (and charset? (let ([name (cdr charset?)]) (and (pair? name) (car name)))))
             => (λ [v] (let ([charset (css-fallback-charset v)])
                         (cond [(string-ci=? charset "UTF-8") /dev/rawin]
                               [else (with-handlers ([exn? (const /dev/rawin)])
                                       (reencode-input-port /dev/rawin charset
                                                            (string->bytes/utf-8 (format "@charset \u22~a\u22;" v))
                                                            #false (object-name /dev/rawin) #true))])))]
            [else /dev/rawin]))))

(module parser typed/racket ;;; https://drafts.csswg.org/css-syntax/#parsing
  (provide (all-defined-out))

  (require (submod ".." digitama))
  (require (submod ".." tokenizer))
  
  (define-syntax (define-css-parser-entry stx)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (syntax-case stx [: :-> lambda]
      [(_ id :-> ->T (lambda [cssin [args : T defval ...] ...] body ...))
       #'(define (id [/dev/stdin : CSS-StdIn (current-input-port)] [args : T defval ...] ...) : ->T
           (define /dev/cssin : Input-Port (css-open-input-port /dev/stdin))
           (dynamic-wind (thunk (port-count-lines! /dev/cssin))
                         (thunk ((λ [[cssin : Input-Port] [args : T defval ...] ...] : ->T body ...) /dev/cssin args ...))
                         (thunk (close-input-port /dev/cssin))))]))

  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (define-css-parser-entry css-parse-stylesheet :-> (Listof CSS-Syntax-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
    (lambda [/dev/cssin]
      (css-consume-stylesheet /dev/cssin)))

  (define-css-parser-entry css-parse-rules :-> (Listof CSS-Syntax-Rule)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-rules
    ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
    (lambda [/dev/cssin]
      (css-consume-rules /dev/cssin #false)))

  (define-css-parser-entry css-parse-rule :-> (U CSS-Syntax-Rule CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-rule
    (lambda [/dev/cssin]
      (define stx (css-read-syntax/skip-whitespace /dev/cssin))
      (define retval : (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error)
        (cond [(eof-object? stx) (make+exn:css:empty stx)]
              [(css:@keyword? stx) (css-consume-@rule /dev/cssin stx)]
              [else (css-consume-qualified-rule /dev/cssin stx)]))
      (define end (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(or (eof-object? end) (exn? retval)) retval]
            [else (make+exn:css:overconsumption end)])))

  (define-css-parser-entry css-parse-declaration :-> (U CSS-Declaration CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#declaration
    ;;; https://drafts.csswg.org/css-syntax/#parse-declaration
    ;;; https://drafts.csswg.org/css-conditional/#at-ruledef-supports
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(not (css:ident? token)) (make+exn:css:missing-identifier token)]
            [else (let-values ([(components _) (css-consume-components /dev/cssin)])
                    (css-components->declaration token components))])))

  (define-css-parser-entry css-parse-declarations :-> (Listof (U CSS-Declaration CSS-@Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-declarations
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-list-of-declarations
    (lambda [/dev/cssin]
      (let consume-declaration+@rule ([mixed-list : (Listof (U CSS-Declaration CSS-@Rule)) null])
        (define token (css-read-syntax /dev/cssin))
        (cond [(eof-object? token) (reverse mixed-list)]
              [(or (css:whitespace? token) (css:semicolon? token)) (consume-declaration+@rule mixed-list)]
              [(css:@keyword? token) (consume-declaration+@rule (cons (css-consume-@rule /dev/cssin token) mixed-list))]
              [else (let-values ([(components _) (css-consume-components /dev/cssin #\;)])
                      (define ?-declaration : (U CSS-Declaration CSS-Syntax-Error)
                        (cond [(css:ident? token) (css-components->declaration token components)]
                              [else (make+exn:css:missing-identifier token)]))
                      (consume-declaration+@rule (css-cons ?-declaration mixed-list)))]))))
  
  (define-css-parser-entry css-parse-component-value :-> (U CSS-Token CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/css-syntax/#parse-component-value
    (lambda [/dev/cssin]
      (define token (css-read-syntax/skip-whitespace /dev/cssin))
      (cond [(eof-object? token) (make+exn:css:empty token)]
            [else (let ([retval (css-consume-component-value /dev/cssin token)])
                    (define end (css-read-syntax/skip-whitespace /dev/cssin))
                    (cond [(eof-object? end) retval]
                          [else (make+exn:css:overconsumption end)]))])))

  (define-css-parser-entry css-parse-component-values :-> (Listof CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [/dev/cssin]
      (define-values (components _) (css-consume-components /dev/cssin))
      components))

  (define-css-parser-entry css-parse-component-valueses :-> (Listof (Listof CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    (lambda [/dev/cssin]
      (css-consume-componentses /dev/cssin #:omit-comma? #false)))

  (define-css-parser-entry css-parse-media-queries :-> (Listof CSS-Media-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/mediaqueries/#mq-list
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query-list
    ;;; https://drafts.csswg.org/mediaqueries/#error-handling
    (lambda [/dev/cssin [rulename : CSS-Syntax-Any eof]]
      (for/list : (Listof CSS-Media-Query) ([entry (in-list (css-consume-componentses /dev/cssin #:omit-comma? #true))])
        (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
          (define-values (token tokens) (css-car entry))
          (define-values (next rest) (css-car tokens))
          (cond [(css:ident-norm=:=? token 'not)
                 (cond [(css:ident? next) (css-components->media-type+query next #false rest)]
                       [else (css-components->negation token tokens #true)])]
                [(css:ident? token)
                 (define-values (?-type ?-<and>)
                   (cond [(css:ident-norm=:=? token 'only) (values next rest)]
                         [else (values token tokens)]))
                 (cond [(eof-object? ?-type) (make+exn:css:missing-identifier ?-type)]
                       [(css:ident? ?-type) (css-components->media-type+query ?-type #true ?-<and>)]
                       [else (make+exn:css:unrecognized ?-type)])]
                [else (css-components->feature-query entry #true rulename)])))))

  (define-css-parser-entry css-parse-feature-query :-> CSS-Feature-Query
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    (lambda [/dev/cssin [rulename : CSS-Syntax-Any eof]]
      (define-values (conditions _) (css-consume-components /dev/cssin))
      (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
        (css-components->feature-query conditions #false rulename))))
  
  (define-css-parser-entry css-parse-selectors :-> (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (define-values (components _) (css-consume-components /dev/cssin))
      (css-components->selectors components #false)))

  (define-css-parser-entry css-parse-relative-selectors :-> (U (Listof CSS-Complex-Selector) CSS-Syntax-Error)
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-relative-selector
    ;;; https://drafts.csswg.org/selectors/#the-scope-pseudo
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [/dev/cssin]
      (make+exn:css:unrecognized eof)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-stylesheet : (-> Input-Port (Listof CSS-Syntax-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#parse-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#declaration-rule-list
    (lambda [css]
      (define rules : (Listof CSS-Syntax-Rule) (css-consume-rules css #true))
      (define rule : (Option CSS-Syntax-Rule) (and (pair? rules) (car rules)))
      (if (and (css-@rule? rule) (css:@keyword-norm=:=? (css-@rule-name rule) '#:@charset))
          (cdr rules)
          rules)))
  
  (define css-consume-rules : (-> Input-Port Boolean (Listof CSS-Syntax-Rule))
    ;;; https://drafts.csswg.org/css-syntax/#consume-list-of-rules
    (lambda [css toplevel?]
      (let consume-rules ([rules : (Listof CSS-Syntax-Rule) null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (reverse rules)]
              [(css:whitespace? token) (consume-rules rules)]
              [(css:@keyword? token) (consume-rules (css-cons (css-consume-@rule css token) rules))]
              [(css:cd? token) (consume-rules (if toplevel? rules (css-cons (css-consume-qualified-rule css token) rules)))]
              [else (consume-rules (css-cons (css-consume-qualified-rule css token) rules))]))))

  (define css-consume-@rule : (-> Input-Port CSS:@Keyword CSS-@Rule)
    ;;; https://drafts.csswg.org/css-syntax/#at-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-an-at-rule
    (lambda [css reconsumed-at-token]
      (define-values (prelude ?-block) (css-consume-rule-item css #:@rule? #true))
      (make-css-@rule reconsumed-at-token prelude ?-block)))

  (define css-consume-qualified-rule : (-> Input-Port CSS-Token (U CSS-Qualified-Rule CSS-@Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    (lambda [css reconsumed]
      (define head (css-consume-component-value css reconsumed))
      (define-values (prelude ?-block) (css-consume-rule-item css #:@rule? #false))
      (cond [(css:block? ?-block) (make-css-qualified-rule (cons head prelude) ?-block)]
            [else (make+exn:css:missing-block (cons head prelude))])))

  (define css-consume-component-value : (-> Input-Port CSS-Token CSS-Token)
    ;;; https://drafts.csswg.org/css-syntax/#component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-component-value
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css reconsumed]
      (cond [(css:delim? reconsumed)
             (case (css:delim-datum reconsumed)
               [(#\{) (css-consume-simple-block css reconsumed #\})]
               [(#\[) (css-consume-simple-block css reconsumed #\])]
               [(#\() (css-consume-simple-block css reconsumed #\))]
               [else reconsumed])]
            [(css:function? reconsumed) (css-consume-function css reconsumed)]
            [else reconsumed])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-consume-rule-item : (-> Input-Port #:@rule? Boolean (Values (Listof CSS-Token) (Option CSS:Block)))
    ;;; https://drafts.csswg.org/css-syntax/#qualified-rule
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-qualified-rule
    (lambda [css #:@rule? at-rule?]
      (let consume-item ([prelude : (Listof CSS-Token) null]
                         [simple-block : (Option CSS:Block) #false])
        (define token (css-read-syntax css))
        (cond [(or (eof-object? token) (and at-rule? (css:semicolon? token)))
               (when (eof-object? token) (make+exn:css:missing-delimiter prelude))
               (values (reverse prelude) simple-block)]
              [(css:delim=:=? token #\{) (values (reverse prelude) (css-consume-simple-block css token #\}))]
              [(css:block=:=? token #\{) (values (reverse prelude) token)]
              [else (consume-item (cons (css-consume-component-value css token) prelude) simple-block)]))))

  (define css-consume-simple-block : (-> Input-Port CSS:Delim Char CSS:Block)
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    (lambda [css open close-char]
      (define-values (components close end-token) (css-consume-block-body css open close-char))
      (css-remake-token [open end-token] css:block (css:delim-datum open) components #false)))

  (define css-consume-function : (-> Input-Port CSS:Function (U CSS:Function CSS:URL))
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    ;;; https://drafts.csswg.org/css-values/#functional-notations
    ;;; https://drafts.csswg.org/css-values/#urls
    (lambda [css func]
      (define fname : Symbol (css:function-datum func))
      (cond [(not (symbol-unreadable? fname)) func]
            [else (let-values ([(components close end-token) (css-consume-block-body css func #\))]
                               [(fnorm) (css:function-norm func)])
                    (if (eq? fnorm 'url)
                        (let-values ([(href modifiers) (css-car components)])
                          (define datum : (U String 'about:invalid)
                            (cond [(not (css:string? href)) 'about:invalid]
                                  [else (let ([datum (css:string-datum href)])
                                          (if (string=? datum "") 'about:invalid datum))]))
                          (css-remake-token func css:url datum (css-url-modifiers-filter func modifiers) #false))
                        (let ([freadable (string->symbol (symbol->string fname))])
                          (css-remake-token [func end-token] css:function freadable fnorm
                                            ; var()s are only meaningful as property values,
                                            ; that's the reason whitespaces are keeped at this point.
                                            (cond [(eq? fnorm 'var) components] 
                                                  [else (filter-not css:whitespace? components)])
                                            #false))))])))

  (define css-consume-block-body : (-> Input-Port CSS-Token Char (Values (Listof CSS-Token) CSS-Syntax-Terminal CSS-Token))
    ;;; https://drafts.csswg.org/css-syntax/#consume-simple-block
    ;;; https://drafts.csswg.org/css-syntax/#consume-a-function
    (lambda [css start-token close-char]
      (let consume-body ([components : (Listof CSS-Token) null])
        (define token (css-read-syntax css))
        (cond [(css:close=:=? token close-char) (values (reverse components) token token)]
              [(not (eof-object? token)) (consume-body (cons (css-consume-component-value css token) components))]
              [else (let ([end-token (if (null? components) start-token (car components))])
                      (make+exn:css:missing-delimiter token)
                      (values (reverse components) token end-token))]))))
  
  (define css-consume-components : (->* (Input-Port) ((Option Char) Boolean) (Values (Listof CSS-Token) CSS-Syntax-Terminal))
    ;;; https://drafts.csswg.org/css-syntax/#parse-list-of-component-values
    (lambda [css [terminating-char #false] [omit-terminate? #false]]
      (let consume-component ([stnenopmoc : (Listof CSS-Token) null])
        (define token (css-read-syntax css))
        (cond [(eof-object? token) (values (reverse stnenopmoc) token)]
              [(and terminating-char (css:delim=:=? token terminating-char))
               (define next (css-peek-syntax/skip-whitespace css))
               (cond [(and omit-terminate? (css-null? stnenopmoc))
                      (cond [(and (eof-object? next) (css-read-syntax/skip-whitespace css))
                             (make+exn:css:overconsumption token)
                             (values (reverse stnenopmoc) eof)]
                            [else (make+exn:css:empty token)
                                  (css-consume-components css terminating-char omit-terminate?)])]
                     [(eof-object? next)
                      (css-read-syntax/skip-whitespace css)
                      (values (reverse stnenopmoc) next)]
                     [else (values (reverse stnenopmoc) token)])]
              [else (consume-component (cons (css-consume-component-value css token) stnenopmoc))]))))

  (define css-consume-componentses : (-> Input-Port [#:omit-comma? Boolean] (Listof (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/css-syntax/#parse-comma-separated-list-of-component-values
    ;;; https://drafts.csswg.org/css-values/#comb-comma
    (lambda [css #:omit-comma? [omit-comma? #true]]
      (let consume-components ([componentses : (Listof (Listof CSS-Token)) null])
        (define-values (components terminating-token) (css-consume-components css #\, omit-comma?))
        (cond [(not (eof-object? terminating-token)) (consume-components (cons components componentses))]
              [(not omit-comma?) (reverse (cons components componentses))]
              [else (filter css-pair? (reverse (cons components componentses)))]))))

  (define css-components->declaration : (-> CSS:Ident (Listof CSS-Token) (U CSS-Declaration CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#consume-declaration
    ;;; https://drafts.csswg.org/css-cascade/#importance
    ;;; https://drafts.csswg.org/css-values/#component-whitespace
    ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    (lambda [id-token components]
      (define-values (?-: value-list) (css-car components))
      (cond [(not (css:colon? ?-:)) (make+exn:css:missing-colon id-token)]
            [else (let ([var? (and (css:ident=<-? id-token symbol-unreadable?) #true)])
                    (define-values (?-values important? lazy?) (css-any->declaration-value id-token value-list var?))
                    (if (exn? ?-values) ?-values (make-css-declaration id-token ?-values important? lazy?)))])))

  (define css-any->declaration-value : (-> CSS-Token (Listof CSS-Token) Boolean
                                           (Values (U (Listof+ CSS-Token) CSS-Syntax-Error) Boolean Boolean))
    ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
    ;;; https://drafts.csswg.org/css-variables/#defining-variables
    (lambda [hint-token components var?]
      (let fold ([lla : (Listof CSS-Token) null]
                 [! : (Option CSS:Delim) #false]
                 [lazy? : Boolean #false]
                 [rest : (Listof CSS-Token) components])
        (define-values (head tail) (if ! (css-car/cdr rest) (css-car rest)))
        (if (eof-object? head)
            (let ([all (reverse lla)]) ; (reverse) does not know non-null list.
              ; whitespace-only <declaration-value>s are also valid, but they are only meaningful for variables.
              (cond [(pair? all) (values all (and (false? var?) ! #true) lazy?)]
                    [(and var? (pair? components)) (values components #false #false)]
                    [else (values (make+exn:css:missing-value hint-token) #false lazy?)]))
            (cond [(and ! (css:ident-norm=:=? head 'important)) (fold (if var? (cons head lla) lla) ! lazy? tail)]
                  [(and !) (values (make+exn:css:unrecognized !) #false lazy?)]
                  [(css:delim=:=? head #\!) (fold lla head lazy? tail)]
                  [(css-special-token? head) (values (make+exn:css:malformed head) #false lazy?)]
                  [else (let-values ([(shadow shadow-lazy? real-tail) (css-declaration-value-filter head tail)])
                          (cond [(exn? shadow) (values shadow #false lazy?)]
                                [else (fold (cons shadow lla) ! (or lazy? shadow-lazy?) real-tail)]))])))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->media-type+query : (-> CSS:Ident Boolean (Listof CSS-Token) CSS-Media-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-query
    (lambda [media only? conditions]
      (define downcased-type : Symbol (css:ident-norm media))
      (define-values (?-and ?-conditions) (css-car conditions))
      (when (css-deprecate-media-type) (make+exn:css:deprecated media))
      (cond [(memq downcased-type '(only not and or)) (make+exn:css:misplaced media)]
            [(eof-object? ?-and) (make-css-media-type downcased-type only?)]
            [(not (css:ident-norm=:=? ?-and 'and)) (make+exn:css:unrecognized ?-and)]
            [(css-null? ?-conditions) (make+exn:css:missing-feature ?-and)]
            [else (cons (make-css-media-type downcased-type only?)
                        (css-components->junction ?-conditions 'and #false #true))])))
  
  (define css-components->feature-query : (-> (Listof CSS-Token) Boolean CSS-Syntax-Any CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-only
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [conditions media? alt]
      (define-values (token rest) (css-car conditions))
      (define-values (op chain) (css-car rest))
      (cond [(eof-object? token) (throw-exn:css:missing-feature alt)]
            [(css:ident-norm=:=? token 'not) (css-components->negation token rest media?)]
            [(eof-object? op) (css-component->feature-query media? token)]
            [(css:ident-norm=<-? op '(and or)) (css-components->junction chain (css:ident-norm op) token media?)]
            [else (throw-exn:css:unrecognized op)])))

  (define css-component->feature-query : (-> Boolean CSS-Token CSS-Feature-Query)
    ;;; https://drafts.csswg.org/css-syntax/#preserved-tokens
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    ;;; https://drafts.csswg.org/mediaqueries/#mq-boolean-context
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [media? condition]
      (cond [(css:block=:=? condition #\()
             (define subany : (Listof CSS-Token) (css:block-components condition))
             (define-values (name any-values) (css-car subany))
             (define-values (op value-list) (css-car any-values))
             (cond [(css:block=:=? name #\() (css-components->feature-query subany media? condition)]
                   [(css:ident-norm=:=? name 'not) (css-components->negation name any-values media?)]
                   [(and (css:ident? name) (css:colon? op))
                    (define descriptor (css-components->declaration name any-values))
                    (cond [(exn? descriptor) (if media? (throw-exn:css:enclosed condition) (raise descriptor))]
                          [(and media?) (css-declaration->media-query descriptor condition)]
                          [else descriptor])]
                   [(and media?)
                    (cond [(and (css:ident? name) (eof-object? op)) (css-make-media-feature name #false #\? #false)]
                          [else (css-components->media-range-query subany condition)])]
                   [(eof-object? name) (throw-exn:css:empty condition)]
                   [(css:ident? name) (throw-exn:css:missing-colon condition)]
                   [(css:function? condition) (throw-exn:css:enclosed condition)]
                   [else (throw-exn:css:missing-identifier condition)])]
            [else (throw-exn:css:missing-feature condition)])))

  (define css-components->negation : (-> CSS:Ident (Listof CSS-Token) Boolean CSS-Not)
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-not
    (lambda [<not> tokens media?]
      (define-values (token rest) (css-car tokens))
      (cond [(eof-object? token) (throw-exn:css:missing-feature <not>)]
            [(css:ident-norm=:=? token 'not) (throw-exn:css:misplaced token)]
            [(css-null? rest) (make-css-not (css-component->feature-query media? token))]
            [else (throw-exn:css:overconsumption rest)])))

  (define css-components->junction : (-> (Listof CSS-Token) Symbol (Option CSS-Token) Boolean (U CSS-And CSS-Or))
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-and
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-media-or
    (lambda [conditions op ?-head media?]
      (define make-junction (if (eq? op 'and) make-css-and make-css-or))
      (let components->junction ([junctions : (Listof CSS-Token) (if (false? ?-head) null (list ?-head))]
                                 [--conditions : (Listof CSS-Token) conditions])
        (define-values (condition rest) (css-car --conditions))
        (define-values (token others) (css-car rest))
        (cond [(eof-object? condition) (make-junction (map (curry css-component->feature-query media?) (reverse junctions)))]
              [(css:ident-norm=:=? condition 'not) (throw-exn:css:misplaced condition)]
              [(or (eof-object? token) (css:ident-norm=:=? token op)) (components->junction (cons condition junctions) others)]
              [(css:ident-norm=<-? token '(and or)) (throw-exn:css:misplaced token)]
              [else (throw-exn:css:overconsumption token)]))))

  (define css-query-support? : (-> CSS-Media-Query (U CSS-Media-Preferences CSS-Feature-Support?) Boolean)
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    ;;; https://drafts.csswg.org/mediaqueries/#evaluating
    ;;; https://drafts.csswg.org/mediaqueries/#media-types
    ;;; https://drafts.csswg.org/mediaqueries/#boolean-context
    (lambda [query support?]
      (cond [(css-not? query) (not (css-query-support? (css-not-condition query) support?))]
            [(css-and? query) (andmap (λ [[q : CSS-Feature-Query]] (css-query-support? q support?)) (css-and-conditions query))]
            [(css-or? query) (ormap (λ [[q : CSS-Feature-Query]] (css-query-support? q support?)) (css-or-conditions query))]
            [(hash? support?)
             (cond [(css-media-feature? query)
                    (define downcased-name : Symbol (css-media-feature-name query))
                    (define datum : CSS-Media-Datum (css-media-feature-value query))
                    (define metadata : (U CSS-Media-Datum EOF) (hash-ref support? downcased-name (λ _ eof)))
                    (cond [(symbol? datum) (and (symbol? metadata) (eq? datum metadata))]
                          [(real? metadata) (case (css-media-feature-operator query)
                                              [(#\>) (> metadata datum)] [(#\≥) (>= metadata datum)]
                                              [(#\<) (< metadata datum)] [(#\≤) (<= metadata datum)]
                                              [else (= metadata datum)])]
                          [else #false])]
                   [(symbol? query)
                    (define metadata : CSS-Media-Datum (hash-ref support? query (λ _ 'none)))
                    (not (if (symbol? metadata) (eq? metadata 'none) (zero? metadata)))]
                   [(css-media-type? query)
                    (define result (memq (css-media-type-name query) (list (current-css-media-type) 'all)))
                    (if (css-media-type-only? query) (and result #true) (not result))]
                   [else (and (pair? query)
                              (css-query-support? (car query) support?)
                              (css-query-support? (cdr query) support?))])]
            [else (and (procedure? support?)
                       (css-declaration? query)
                       (support? (css:ident-norm (css-declaration-name query))
                                 (css-declaration-values query)))])))

  (define css-components->media-range-query : (-> (Listof CSS-Token) CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [components broken-condition]
      (define-values (value0 rest0) (css-car-media-value components))
      (define-values (d0 op0 po0 rest1) (css-car-comparison-operator rest0))
      (define-values (value1 rest2) (css-car-media-value rest1))
      (define-values (d1 op1 po1 rest3) (css-car-comparison-operator rest2))
      (define-values (value2 terminal) (css-car-media-value rest3))
      (cond [(eof-object? value0) (throw-exn:css:empty broken-condition)]
            [(eof-object? d0) (throw-exn:css:missing-delimiter components)]
            [(eof-object? value1) (throw-exn:css:missing-value rest0)]
            [(and (css:ident? value0) (css:delim? d1)) (throw-exn:css:enclosed broken-condition)]
            [(and (eq? op0 #\=) (css:delim? d1)) (throw-exn:css:overconsumption broken-condition)]
            [(css:ident? value0) (css-make-media-feature value0 value1 op0 d0)]
            [(and (eof-object? d1) (css:ident? value1)) (css-make-media-feature value1 value0 po0 d0)]
            [(not (css:ident? value1)) (throw-exn:css:missing-identifier value1)]
            [(or (eof-object? value2) (css:ident? value2)) (throw-exn:css:missing-value rest2)]
            [(css-pair? terminal) (throw-exn:css:overconsumption terminal)]
            [(not (eq? (css:delim-datum d0) (css:delim-datum d1))) (throw-exn:css:malformed (list d0 value1 d1))]
            [else (make-css-and (list (css-make-media-feature value1 value0 po0 d0)
                                      (css-make-media-feature value1 value2 op1 d1)))])))
  
  (define css-declaration->media-query : (-> CSS-Declaration CSS:Block CSS-Feature-Query)
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    (lambda [property broken-condition]
      (define-values (media-value rest) (css-car-media-value (css-declaration-values property)))
      (cond [(eof-object? media-value) (throw-exn:css:enclosed broken-condition)]
            [(css-pair? rest) (throw-exn:css:enclosed broken-condition)]
            [else (css-make-media-feature (css-declaration-name property) media-value #\: #false)])))

  (define css-car-comparison-operator : (-> (Listof CSS-Token) (Values (U CSS:Delim EOF) Char Char (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/mediaqueries/#mq-range-context
    (lambda [components]
      (define-values (d rest) (css-car components))
      (define-values (?-= terminal) (css-car/cdr rest))
      (cond [(css:delim=:=? d #\=) (values d #\= #\= rest)]
            [(css:delim=:=? d #\>) (if (css:delim=:=? ?-= #\=) (values d #\≥ #\≤ terminal) (values d #\> #\< rest))]
            [(css:delim=:=? d #\<) (if (css:delim=:=? ?-= #\=) (values d #\≤ #\≥ terminal) (values d #\< #\> rest))]
            [(eof-object? d) (values eof #\≠ #\≠ rest)]
            [else (throw-exn:css:unrecognized d)])))
  
  (define css-car-media-value : (-> (Listof CSS-Token) (Values (U CSS-Media-Value EOF) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-mf-value
    ;;; https://drafts.csswg.org/mediaqueries/#typedef-ratio
    (lambda [components]
      (define-values (value rest) (css-car components))
      (define-values (?-/ ?-rest) (css-car rest))
      (define-values (?-int terminal) (css-car ?-rest))
      (cond [(eof-object? value) (values eof rest)]
            [(css:slash? ?-/)
             (define width : (Option Positive-Integer) (css:integer=<-? value exact-positive-integer?))
             (define height : (Option Positive-Integer) (css:integer=<-? ?-int exact-positive-integer?))
             (values (cond [(and width height (css-token? ?-int)) (css-remake-token [value ?-int] css:ratio (/ width height))]
                           [(css-number? value) (throw-exn:css:range value)]
                           [(css-number? ?-int) (throw-exn:css:range ?-int)]
                           [else (throw-exn:css:type (filter css-token? (list value ?-/ ?-int)))])
                     terminal)]
            [(or (css:ident? value) (css-numeric? value)) (values value rest)]
            [else (values (throw-exn:css:unrecognized value) rest)])))

  (define css-make-media-feature : (-> CSS:Ident (Option CSS-Media-Value) Char (Option CSS:Delim) (U Symbol CSS-Media-Feature))
    ;;; https://drafts.csswg.org/mediaqueries/#mq-features
    ;;; https://drafts.csswg.org/mediaqueries/#mq-min-max
    (lambda [desc-name ?-value ophint ?-op]
      (define errobj : (Listof CSS-Token) (filter css-token? (list desc-name ?-op ?-value)))
      (define name : String (symbol->string (css:ident-norm desc-name)))
      (define-values (downcased-name op min/max?)
        (cond [(string-prefix? name "min-") (values (string->symbol (substring name 4)) #\≥ #true)]
              [(string-prefix? name "max-") (values (string->symbol (substring name 4)) #\≤ #true)]
              [else (values (string->symbol name) ophint #false)]))
      (when (and min/max?)
        (cond [(or (not ?-value) (css:delim? ?-op)) (throw-exn:css:misplaced errobj)]
              [(not (css-numeric? ?-value)) (throw-exn:css:type errobj)]))
      (define feature-filter : (U Void (CSS:Filter CSS-Media-Datum))
        ((current-css-media-feature-filter) downcased-name min/max? (thunk (void (make+exn:css:deprecated desc-name)))))
      (cond [(void? feature-filter) (throw-exn:css:unrecognized errobj)]
            [(false? ?-value) downcased-name]
            [else (let ([datum (feature-filter ?-value)])
                    (cond [(false? datum) (throw-exn:css:type ?-value desc-name)]
                          [(exn:css? datum) (css-log-syntax-error datum desc-name) (raise datum)]
                          [else (make-css-media-feature downcased-name datum op)]))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->selectors : (-> (Listof CSS-Token) CSS-Namespace-Hint (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#parse-selector
    ;;; https://drafts.csswg.org/selectors/#selector-list
    ;;; https://drafts.csswg.org/selectors/#grouping
    (lambda [components namespaces]
      (with-handlers ([exn:css? (λ [[errcss : exn:css]] errcss)])
        (define-values (head-complex-selector ?-eof ?-rest) (css-car-complex-selector components namespaces))
        (let extract-complex-selector ([srotceles : (Listof CSS-Complex-Selector) null]
                                       [terminal : (U EOF CSS:Delim) ?-eof]
                                       [rest : (Listof CSS-Token) ?-rest])
          (if (css-null? rest)
              (cond [(eof-object? terminal) (cons head-complex-selector (reverse srotceles))]
                    [else (throw-exn:css:overconsumption terminal)])
              (let-values ([(complex-selector ?-terminal ?-rest) (css-car-complex-selector rest namespaces)])
                (extract-complex-selector (cons complex-selector srotceles) ?-terminal ?-rest)))))))
  
  (define css-car-complex-selector : (-> (Listof CSS-Token) CSS-Namespace-Hint
                                         (Values CSS-Complex-Selector (U EOF CSS:Delim) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#combinators
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [components namespaces]
      (define-values (head-compound-selector rest) (css-car-compound-selector components #false namespaces))
      (let extract-relative-selector ([srotceles : (Listof CSS-Compound-Selector) null]
                                      [tokens : (Listof CSS-Token) rest])
        (define-values (?-terminal rest) (css-car tokens))
        (define-values (token ?-selectors) (css-car/cdr tokens))
        (cond [(or (eof-object? ?-terminal) (css:comma? ?-terminal))
               (values (css-make-complex-selector (cons head-compound-selector (reverse srotceles))) ?-terminal rest)]
              [(not (css-selector-combinator? token)) (throw-exn:css:unrecognized ?-terminal)]
              [else (let*-values ([(combinator ?-selectors) (css-car-combinator token ?-selectors)]
                                  [(?-selector ?-rest) (css-car ?-selectors)])
                      (if (or (eof-object? ?-selector) (css:comma? ?-selector))
                          (throw-exn:css:overconsumption ?-selectors)
                          (let-values ([(selector rest) (css-car-compound-selector ?-selectors combinator namespaces)])
                            (extract-relative-selector (cons selector srotceles) rest))))]))))

  (define css-car-compound-selector : (-> (Listof CSS-Token) (Option CSS-Selector-Combinator) CSS-Namespace-Hint
                                          (Values CSS-Compound-Selector (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    ;;; https://github.com/w3c/csswg-drafts/issues/202
    (lambda [components combinator namespaces]
      (define-values (head heads) (css-car components))
      (define-values (typename quirkname namespace simple-selector-components)
        (cond [(css:ident? head) (css-car-elemental-selector head heads namespaces)]
              [(css:delim=<-? head '(#\| #\*)) (css-car-elemental-selector head heads namespaces)]
              [(or (eof-object? head) (css:comma? head)) (throw-exn:css:empty head)]
              [else (values #true #true (or (css-declared-namespace namespaces '||) #true) (cons head heads))]))
      (define-values (pseudo-classes selector-components) (css-car-pseudo-class-selectors simple-selector-components))
      (let extract-simple-selector ([sessalc : (Listof Symbol) null]
                                    [sdi : (Listof Keyword) null]
                                    [setubirtta : (Listof CSS-Attribute-Selector) null]
                                    [pseudo-element : (Option CSS-Pseudo-Element-Selector) #false]
                                    [selector-tokens : (Listof CSS-Token) selector-components])
        (define-values (token tokens) (css-car/cdr selector-tokens))
        (cond [(or (eof-object? token) (css:comma? token) (css-selector-combinator? token))
               (values (make-css-compound-selector combinator typename quirkname namespace pseudo-classes
                                                   (reverse sessalc) (reverse sdi) (reverse setubirtta) pseudo-element)
                       selector-tokens)]
              [(and pseudo-element) (throw-exn:css:overconsumption token)]
              [(css:delim=:=? token #\.)
               (define-values (next rest) (css-car/cdr tokens))
               (cond [(not (css:ident? next)) (throw-exn:css:missing-identifier next)]
                     [else (extract-simple-selector (cons (css:ident-datum next) sessalc) sdi setubirtta pseudo-element rest)])]
              [(css:colon? token)
               (define-values (?-pseudo-classes ?-rest) (css-car-pseudo-class-selectors tokens))
               (define-values (next rest) (css-car/cdr ?-rest))
               (cond [(null? ?-pseudo-classes) (throw-exn:css:misplaced (list token (car tokens)))]
                     [else (let ([pclass (car ?-pseudo-classes)])
                             (define pelement : CSS-Pseudo-Element-Selector
                               (make-css-pseudo-element-selector (css-pseudo-class-selector-name pclass)
                                                                 (css-pseudo-class-selector-arguments pclass)
                                                                 (cdr ?-pseudo-classes)))
                             (extract-simple-selector sessalc sdi setubirtta pelement ?-rest))])]
              [(css:block=:=? token #\[)
               (define attribute-selector : CSS-Attribute-Selector (css-simple-block->attribute-selector token namespaces))
               (extract-simple-selector sessalc sdi (cons attribute-selector setubirtta) pseudo-element tokens)]
              [(css:hash? token) (extract-simple-selector sessalc (cons (css:hash-datum token) sdi) setubirtta pseudo-element tokens)]
              [else (throw-exn:css:unrecognized token)]))))

  (define css-car-combinator : (-> (U CSS:WhiteSpace CSS:Delim) (Listof CSS-Token) (Values CSS-Selector-Combinator (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#grammar
    (lambda [token tokens]
      (case (cond [(css:whitespace? token) #\space] [(css:delim? token) (css:delim-datum token)] [else #\null])
        [(#\space)
         (define-values (next tail) (css-car tokens))
         (cond [(css-selector-combinator? next) (css-car-combinator next tail)]
               [else (values '>> tokens)])]
        [(#\>)
         (define-values (next tail) (css-car/cdr tokens))
         (define-values (next2 tail2) (css-car tail))
         (cond [(css:delim=:=? next #\>) (values '>> tail2)]
               [else (values '> tail)])]
        [(#\+)   (values '+ tokens)]
        [(#\~)   (values '~ tokens)]
        [(#\tab) (values '|| tokens)]
        [else (throw-exn:css:unrecognized token)])))
  
  (define css-car-elemental-selector : (-> (U CSS:Ident CSS:Delim) (Listof CSS-Token) CSS-Namespace-Hint
                                           (Values (U Symbol True) (U Symbol True) (U Symbol Boolean) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [token tokens namespaces]
      (define-values (next rest next2 rest2) (css-car/cadr tokens))
      (cond [(css:vbar? token)
             (cond [(css:ident? next) (values (css:ident-datum next) (css:ident-norm next) #false rest)]
                   [(css:delim=:=? next #\*) (values #true #true #false rest)]
                   [else (throw-exn:css:missing-identifier next)])]
            [(css:vbar? next)
             (define ns : (U Symbol Boolean) (css-declared-namespace namespaces token))
             (cond [(false? ns) (throw-exn:css:namespace token)]
                   [(css:ident? next2) (values (css:ident-datum next2) (css:ident-norm next2) ns rest2)]
                   [(css:delim=:=? next2 #\*) (values #true #true ns rest2)]
                   [else (throw-exn:css:missing-identifier (list token next))])]
            [else (let ([ns (or (css-declared-namespace namespaces '||) #true)])
                    (cond [(css:delim? token) (values #true #true ns tokens)]
                          [else (values (css:ident-datum token) (css:ident-norm token) ns tokens)]))])))

  (define css-car-pseudo-class-selectors : (-> (Listof CSS-Token) (Values (Listof CSS-Pseudo-Class-Selector) (Listof CSS-Token)))
    ;;; https://drafts.csswg.org/selectors/#structure
    ;;; https://drafts.csswg.org/selectors/#elemental-selectors
    ;;; https://drafts.csswg.org/selectors/#pseudo-classes
    (lambda [components]
      (let extract-pseudo-class-selector ([srotceles : (Listof CSS-Pseudo-Class-Selector) null]
                                          [tokens : (Listof CSS-Token) components])
        (define-values (maybe: rest ?-id rest2) (css-car/cadr tokens))
        (cond [(or (not (css:colon? maybe:)) (css:colon? ?-id)) (values (reverse srotceles) tokens)]
              [(css:ident? ?-id)
               (let ([selector (make-css-pseudo-class-selector (css:ident-datum ?-id) #false)])
                 (extract-pseudo-class-selector (cons selector srotceles) rest2))]
              [(css:function? ?-id)
               (let ([selector (make-css-pseudo-class-selector (css:function-norm ?-id) (css:function-arguments ?-id))])
                 (extract-pseudo-class-selector (cons selector srotceles) rest2))]
              [else (throw-exn:css:missing-identifier maybe:)]))))
  
  (define css-simple-block->attribute-selector : (-> CSS:Block CSS-Namespace-Hint CSS-Attribute-Selector)
    ;;; https://drafts.csswg.org/selectors/#attribute-selectors
    ;;; https://drafts.csswg.org/selectors/#attrnmsp
    ;;; https://drafts.csswg.org/selectors/#attribute-case
    ;;; https://drafts.csswg.org/css-namespaces/#css-qnames
    (lambda [block namespaces]
      (define-values (1st rest1) (css-car (css:block-components block)))
      (define-values (2nd rest2 3rd rest3) (css-car/cadr rest1))
      (define-values (attrname quirkname namespace op-part)
        (cond [(eof-object? 1st) (throw-exn:css:empty block)]
              [(or (css:match? 1st) (css:delim=:=? 1st #\=))
               (throw-exn:css:missing-identifier block)]
              [(or (eof-object? 2nd) (css:match? 2nd) (css:delim=:=? 2nd #\=) (css:whitespace? 2nd))
               ; WARNING: the namespace behavior for attributes is different from that for elements 
               (cond [(css:ident? 1st) (values (css:ident-datum 1st) (css:ident-norm 1st) #false rest1)]
                     [else (throw-exn:css:missing-identifier 1st)])]
              [(or (eof-object? 3rd) (css:match? 3rd) (css:delim=:=? 3rd #\=) (css:whitespace? 3rd))
               (cond [(and (css:vbar? 1st) (css:ident? 2nd)) (values (css:ident-datum 2nd) (css:ident-norm 2nd) #false rest2)]
                     [(css:vbar? 2nd) (throw-exn:css:missing-identifier 2nd)]
                     [else (throw-exn:css:unrecognized 1st)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:vbar? 2nd) (css:ident? 3rd))
               (define ns (css-declared-namespace namespaces 1st))
               (cond [(false? ns) (throw-exn:css:namespace 1st)]
                     [else (values (css:ident-datum 3rd) (css:ident-norm 3rd) ns rest3)])]
              [(and (or (css:ident? 1st) (css:delim=:=? 1st #\*)) (css:vbar? 2nd))
               (throw-exn:css:missing-identifier 3rd)]
              [(or (css:ident? 1st) (css:delim=:=? 1st #\*))
               (throw-exn:css:unrecognized 2nd)]
              [else (throw-exn:css:unrecognized 1st)]))
      (define-values (op value-part value ci-part) (css-car/cadr op-part))
      (define-values (i terminal) (css-car ci-part))
      (unless (eof-object? op)
        (cond [(eof-object? value) (throw-exn:css:missing-value op)]
              [(nor (eof-object? i) (css:ident-norm=:=? i 'i)) (throw-exn:css:overconsumption i)]
              [(css-pair? terminal) (throw-exn:css:overconsumption terminal)]))
      (define val : (U String Symbol)
        (cond [(css:string? value) (css:string-datum value)]
              [(css:ident? value) (css:ident-datum value)]
              [(or (css:whitespace? value) (eof-object? value)) ""]
              [else (throw-exn:css:type value)]))
      (cond [(or (css:whitespace? op) (eof-object? op)) (make-css-attribute-selector attrname quirkname namespace)]
            [(css:delim=:=? op #\=) (make-css-attribute~selector attrname quirkname namespace #\= val (css:ident? i))]
            [(css:match? op) (make-css-attribute~selector attrname quirkname namespace (css:match-datum op) val (css:ident? i))]
            [else (throw-exn:css:unrecognized op)])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-open-input-port : (-> CSS-StdIn Input-Port)
    ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
    (lambda [/dev/stdin]
      (if (list? /dev/stdin)
          (let ([total : Index (length /dev/stdin)]
                [cursor : Integer 0])
            (make-input-port (if (pair? /dev/stdin) (css-token-source (car /dev/stdin)) '/dev/cssin/null)
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(>= cursor total) eof]
                                          [(set! cursor (add1 cursor))
                                           => (λ _ (list-ref /dev/stdin (sub1 cursor)))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               (λ _ (cond [(>= (+ skip cursor) total) eof]
                                          [else (list-ref /dev/stdin (+ skip cursor))])))
                             void))
          (let* ([/dev/rawin (cond [(port? /dev/stdin) /dev/stdin]
                                   [(path? /dev/stdin) (open-input-file /dev/stdin)]
                                   [(regexp-match? #px"\\.css$" /dev/stdin) (open-input-file (~a /dev/stdin))]
                                   [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin/string)]
                                   [(bytes? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin/bytes)]
                                   [else (open-input-string (~s /dev/stdin) '/dev/cssin/error)])]
                 [/dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin)]
                 [peek-pool : (Listof CSS-Syntax-Any) null])
            (define portname : (U String Symbol)
              (let ([src (object-name /dev/cssin)])
                (cond [(path? src) (path->string (simple-form-path src))]
                      [else (string->symbol (~a src))])))
            (make-input-port portname
                             (λ [[buf : Bytes]]
                               (λ _ (cond [(null? peek-pool) (css-consume-token /dev/cssin portname)]
                                          [else (let-values ([(rest peeked) (split-at-right peek-pool 1)])
                                                  (set! peek-pool rest)
                                                  (car peeked))])))
                             (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                               ; NOTE: It seems that optimize this code to always throw the last peeked token
                               ;        does not improve the performance.
                               (λ _ (and (for ([idx (in-range (length peek-pool) (add1 skip))])
                                           (set! peek-pool (cons (css-consume-token /dev/cssin portname) peek-pool)))
                                         (list-ref peek-pool (- (length peek-pool) skip 1)))))
                             (thunk (unless (eq? /dev/rawin /dev/cssin) (close-input-port /dev/cssin))
                                    (unless (eq? /dev/rawin /dev/stdin) (close-input-port /dev/rawin)))
                             #false #false
                             (thunk (port-next-location /dev/cssin))
                             (thunk (port-count-lines! /dev/cssin)))))))
  
  (define css-read-syntax : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define stx (read-char-or-special css))
      (cond [(or (css-token? stx) (eof-object? stx)) stx]
            [else (css-make-bad-token (css-srcloc css '/dev/cssin/error 0 0 0)
                                      css:bad:stdin struct:css-token (~s stx))])))

  (define css-peek-syntax : (->* (Input-Port) (Natural) CSS-Syntax-Any)
    (lambda [css [skip 0]]
      (define stx (peek-char-or-special css skip))
      (cond [(or (css-token? stx) (eof-object? stx)) stx]
            [else (css-make-bad-token (css-srcloc css '/dev/cssin/error 0 0 0)
                                      css:bad:stdin struct:css-token (~s stx))])))
  
  (define css-read-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (define token (css-read-syntax css))
      (cond [(not (css:whitespace? token)) token]
            [else (css-read-syntax/skip-whitespace css)])))

  (define css-peek-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
    (lambda [css]
      (let peek/skip-whitespace : CSS-Syntax-Any ([skip : Nonnegative-Fixnum 0])
        (define token (css-peek-syntax css skip))
        (cond [(not (css:whitespace? token)) token]
              [else (peek/skip-whitespace (fx+ skip 1))]))))

  (define css-selector-combinator? : (-> CSS-Syntax-Any Boolean : #:+ (U CSS:WhiteSpace CSS:Delim))
    (lambda [token]
      (or (css:whitespace? token)
          (and (css:delim=<-? token '(#\~ #\+ #\> #\tab))
               #true))))

  (define css-function->var : (-> CSS:Function (U CSS:Var CSS-Syntax-Error))
    (lambda [var]
      (define-values (--var ?-fallback-list) (css-car (css:function-arguments var)))
      (define-values (?-comma fallback) (css-car ?-fallback-list))
      (cond [(not (css:ident=<-? --var symbol-unreadable?)) (make+exn:css:missing-variable --var)]
            [(eof-object? ?-comma) (css-remake-token var css:var (css:ident-datum --var) null #false)]
            [(not (css:comma? ?-comma)) (make+exn:css:missing-comma ?-comma)]
            [else (let-values ([(?-fallback _ lazy?) (css-any->declaration-value ?-comma fallback #true)])
                    (cond [(exn? ?-fallback) ?-fallback]
                          [else (css-remake-token var css:var (css:ident-datum --var) ?-fallback lazy?)]))])))

  (define css-declaration-value-filter : (-> CSS-Token (Listof CSS-Token)
                                             (Values (U CSS-Token CSS-Syntax-Error) Boolean (Listof CSS-Token)))
    (lambda [token candidates]
      (cond [(css:url? token)
             (define-values (submodifiers lazy?) (css-lazy-subtokens-map (css:url-modifiers token)))
             (define url : (U CSS:URL CSS-Syntax-Error)
               (cond [(exn:css? submodifiers) submodifiers]
                     [(false? lazy?) token]
                     [else (css:url-copy token (css-url-modifiers-filter token submodifiers) lazy?)]))
             (values url (not (eq? token url)) candidates)]
            [(css:function? token)
             (define func : (U CSS:Function CSS:Var CSS-Syntax-Error)
               (cond [(eq? (css:function-norm token) 'var) (css-function->var token)]
                     [else (let-values ([(subarguments lazy?) (css-lazy-subtokens-map (css:function-arguments token))])
                             (cond [(exn:css? subarguments) subarguments]
                                   [(false? lazy?) token]
                                   [else (css:function-copy token subarguments lazy?)]))]))
             (values func (not (eq? token func)) candidates)]
            [(css:@keyword? token)
             (define-values (next rest) (css-car/cdr candidates))
             (define binding : Symbol (string->symbol (substring (keyword->string (css:@keyword-datum token)) 1)))
             (if (or (eof-object? next) (not (css:block? next)))
                 (values (css-remake-token token css:racket binding) #false candidates)
                 (let-values ([(argl lazy?) (css-lazy-subtokens-map (filter-not css:whitespace? (css:block-components next)))])
                   (cond [(exn:css? argl) (values argl #false candidates)]
                         [else (values (css-remake-token [token next] css:λracket binding argl lazy?) lazy? rest)])))]
            [(css:block? token)
             (define-values (subcomponents lazy?) (css-lazy-subtokens-map (css:block-components token)))
             (define block : (U CSS:Block CSS-Syntax-Error)
               (cond [(exn:css? subcomponents) subcomponents]
                     [(false? lazy?) token]
                     [else (css:block-copy token subcomponents lazy?)]))
             (values block (not (eq? token block)) candidates)]
            [else (values token #false candidates)])))

  (define css-lazy-subtokens-map : (-> (Listof CSS-Token) (Values (U (Listof CSS-Token) CSS-Syntax-Error) Boolean))
    (lambda [subtokens]
      (let lazy-fold ([tokens : (Listof CSS-Token) null]
                      [lazy? : Boolean #false]
                      [subrest : (Listof CSS-Token) subtokens])
        (cond [(null? subrest) (values (reverse tokens) lazy?)]
              [else (let ([head (car subrest)])
                      (define-values (shadow shadow-lazy? tail) (css-declaration-value-filter head (cdr subrest)))
                      (cond [(exn:css? shadow) (values shadow #false)]
                            [else (lazy-fold (cons shadow tokens)
                                             (or lazy? shadow-lazy?)
                                             tail)]))]))))

  (define css-url-modifiers-filter : (-> CSS-Token (Listof CSS-Token) (Listof CSS-URL-Modifier))
    (lambda [url modifiers]
      (let modifiers-filter ([sreifidom : (Listof CSS-URL-Modifier) null]
                             [tail : (Listof CSS-Token) modifiers])
        (define-values (head rest) (css-car tail))
        (cond [(eof-object? head) (reverse sreifidom)]
              [(or (css:ident? head) (css-lazy-token? head)) (modifiers-filter (cons head sreifidom) rest)]
              [else (make+exn:css:type (list url head)) (modifiers-filter sreifidom rest)]))))
  
  (define css-media-queries-support? : (-> (Listof CSS-Media-Query) CSS-Media-Preferences Boolean)
    (lambda [queries preferences]
      (or (null? queries)
          (for/or ([query (in-list queries)])
                  (css-query-support? query preferences))))))

(module grammar typed/racket ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
  (provide (all-defined-out))
  
  (require (submod ".." digitama))
  (require (submod ".." tokenizer))
  (require (submod ".." parser))

  (require (for-syntax syntax/parse))

  ;; https://drafts.csswg.org/css-syntax/#css-stylesheets
  (define-type CSS-Media-Rule (Pairof (Listof CSS-Grammar-Rule) CSS-Media-Preferences))
  (define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-Media-Rule))
  (define-type CSS-StyleSheet-Pool (HashTable Natural CSS-StyleSheet))

  (struct: css-style-rule : CSS-Style-Rule ([selectors : (Listof+ CSS-Complex-Selector)] [properties : CSS-Declarations]))
  
  (struct: css-stylesheet : CSS-StyleSheet
    ([pool : CSS-StyleSheet-Pool]
     [location : (U String Symbol)]
     [timestamp : Integer]
     [preferences : CSS-Media-Preferences]
     [imports : (Listof Positive-Integer)]
     [namespaces : CSS-Namespace]
     [rules : (Listof CSS-Grammar-Rule)]))

  (define css-stylesheet-placeholder : CSS-StyleSheet
    (make-css-stylesheet (make-hasheq) '/dev/null 0 (make-hasheq) null (make-hasheq) null))
  
  (define-css-parser-entry read-css-stylesheet :-> CSS-StyleSheet
    ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
    ;;; https://drafts.csswg.org/css-syntax/#charset-rule
    ;;; https://drafts.csswg.org/css-namespaces
    ;;; https://drafts.csswg.org/css-cascade/#at-import
    ;;; https://drafts.csswg.org/css-conditional/#processing
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
    (lambda [/dev/cssin [pool : CSS-StyleSheet-Pool ((inst make-hasheq Natural CSS-StyleSheet))]]
      (define location : (U String Symbol) (let ([p (object-name /dev/cssin)]) (if (symbol? p) p (~a p))))
      (define identity : Natural (if (string? location) (css-stylesheet-path->identity location) 0))
      (define init-viewport : CSS-Media-Preferences (current-css-media-preferences))
      (or (and (string? location) (hash-has-key? pool identity)
               (let ([stylesheet (hash-ref pool identity)])
                 (and (not (css-stylesheet-outdated? stylesheet))
                      (css-update-imported-stylesheets stylesheet)
                      stylesheet)))
          (let ([rules (css-consume-stylesheet /dev/cssin)])
            (when (positive? identity) (hash-set! pool identity css-stylesheet-placeholder))
            (define namespaces : CSS-Namespace (make-hasheq))
            (define-values (viewport imports grammars)
              (css-syntax-rules->grammar-rules location rules namespaces #true #true init-viewport pool))
            (define timestamp : Integer (if (string? location) (file-or-directory-modify-seconds location) (current-seconds)))
            (define stylesheet : CSS-StyleSheet (make-css-stylesheet pool location timestamp viewport imports namespaces grammars))
            (when (positive? identity) (hash-set! pool identity stylesheet))
            stylesheet))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-syntax-rules->grammar-rules : (->* ((U String Symbol) (Listof CSS-Syntax-Rule) CSS-Namespace Boolean Boolean)
                                                 (CSS-Media-Preferences CSS-StyleSheet-Pool)
                                                 (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule)))
    (lambda [src syntaxes0 namespaces can-import0? allow-namespace0?
                 [init-viewport (current-css-media-preferences)]
                 [pool ((inst make-hasheq Natural CSS-StyleSheet))]]
      (define-values (viewport-srotpircsed !viewport-sexatnys)
        (for/fold ([viewport-srotpircsed : (Listof CSS-Declarations) null]
                   [normal-sexatnys : (Listof CSS-Syntax-Rule) null])
                  ([stx : CSS-Syntax-Rule (in-list syntaxes0)])
          (define ?-descriptor : (U CSS-Declarations CSS-Syntax-Error Void)
            (when (and (css-@rule? stx) (css:@keyword-norm=:=? (css-@rule-name stx) '#:@viewport))
              (define prelude : (Listof CSS-Token) (css-@rule-prelude stx))
              (define ?-block : (Option CSS:Block) (css-@rule-block stx))
              (cond [(css-pair? prelude) (make+exn:css:overconsumption prelude)]
                    [?-block (css-components->declarations (css:block-components ?-block))]
                    [else (make+exn:css:missing-block (css-@rule-name stx))])))
          (cond [(void? ?-descriptor) (values viewport-srotpircsed (cons stx normal-sexatnys))]
                [(exn? ?-descriptor) (values viewport-srotpircsed normal-sexatnys)]
                [else (values (cons ?-descriptor viewport-srotpircsed) normal-sexatnys)])))
      (define viewport : CSS-Media-Preferences
        (cond [(null? viewport-srotpircsed) init-viewport]
              [else (css-cascade-viewport init-viewport
                                          (reverse viewport-srotpircsed)
                                          (current-css-viewport-descriptor-filter)
                                          (current-css-viewport-filter))]))
      (let syntax->grammar : (Values CSS-Media-Preferences (Listof Positive-Integer) (Listof CSS-Grammar-Rule))
        ([seititnedi : (Listof Positive-Integer) null]
         [srammarg : (Listof CSS-Grammar-Rule) null]
         [!viewport-syntaxes : (Listof CSS-Syntax-Rule) (reverse !viewport-sexatnys)]
         [can-import? : Boolean can-import0?]
         [allow-namespace? : Boolean allow-namespace0?])
        (cond [(null? !viewport-syntaxes) (values viewport (reverse seititnedi) (reverse srammarg))]
              [else (let-values ([(stx rest) (values (car !viewport-syntaxes) (cdr !viewport-syntaxes))])
                      (if (css-qualified-rule? stx)
                          (let ([?-rule : (U CSS-Style-Rule CSS-Syntax-Error) (css-qualified-rule->style-rule stx namespaces)])
                            (define ++srammarg : (Listof CSS-Grammar-Rule)
                              (cond [(or (exn? ?-rule) (null? (css-style-rule-properties ?-rule))) srammarg]
                                    [else (cons ?-rule srammarg)]))
                            (syntax->grammar seititnedi ++srammarg rest #false #false))
                          (case (css:@keyword-norm (css-@rule-name stx))
                            [(#:@charset)
                             (make+exn:css:misplaced (css-@rule-name stx))
                             (syntax->grammar seititnedi srammarg rest can-import? allow-namespace?)]
                            [(#:@import)
                             (define css-id : (U Positive-Integer False CSS-Syntax-Error)
                               (cond [(false? can-import?) (make+exn:css:misplaced (css-@rule-name stx))]
                                     [else (css-@import->stylesheet-identity stx src viewport pool)]))
                             (syntax->grammar (if (integer? css-id) (cons css-id seititnedi) seititnedi)
                                              srammarg rest can-import? allow-namespace?)]
                            [(#:@namespace)
                             (define css-ns : (U (Pairof Symbol String) CSS-Syntax-Error)
                               (cond [allow-namespace? (css-@namespace->namespace stx)]
                                     [else (make+exn:css:misplaced (css-@rule-name stx))]))
                             (when (pair? css-ns) (hash-set! namespaces (car css-ns) (cdr css-ns)))
                             (syntax->grammar seititnedi srammarg rest #false allow-namespace?)]
                            [(#:@media)
                             (define media-rules (css-@media->media-rule stx viewport namespaces pool))
                             (define ++srammarg : (Listof CSS-Grammar-Rule)
                               (cond [(list? media-rules) (append (reverse media-rules) srammarg)]
                                     [(pair? media-rules) (cons media-rules srammarg)]
                                     [else srammarg]))
                             (syntax->grammar seititnedi ++srammarg rest #false #false)]
                            [(#:@support)
                             (define expand-stxes (css-@support->syntax-rules stx))
                             (define delta : (Listof CSS-Syntax-Rule) (if (pair? expand-stxes) (append expand-stxes rest) rest))
                             (syntax->grammar seititnedi srammarg delta #false #false)]
                            [else (syntax->grammar seititnedi (cons stx srammarg) rest #false #false)])))]))))
  
  (define css-@import->stylesheet-identity : (-> CSS-@Rule Any CSS-Media-Preferences CSS-StyleSheet-Pool
                                                 (U Positive-Integer False CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-cascade/#at-import
    (lambda [import parent-href preferences pool]
      (define-values (uri ?-condition) (css-car (css-@rule-prelude import)))
      (define name : CSS:@Keyword (css-@rule-name import))
      (define ?-block : (Option CSS:Block) (css-@rule-block import))
      (define ?-target.css : (U Path CSS-Syntax-Error)
        (cond [(eof-object? uri) (make+exn:css:empty (css-@rule-name import))]
              [(css:string=<-? uri non-empty-string?) => (λ [url] (css-url-string->path parent-href url))]
              [(css:url=<-? uri string?) => (λ [url] (css-url-string->path parent-href url))]
              [(or (css:string? uri) (css:url? uri)) (make+exn:css:empty uri)]
              [else (make+exn:css:unrecognized uri)]))
      (cond [(exn? ?-target.css) ?-target.css]
            [(css:block? ?-block) (make+exn:css:overconsumption ?-block)]
            [(false? (regexp-match? #px"\\.css$" ?-target.css)) (make+exn:css:resource uri)]
            [(false? (file-exists? ?-target.css)) (make+exn:css:resource uri)]
            [else (let-values ([(?-support ?-media-list) (css-car ?-condition)])
                    (define-values (support? media-list)
                      (if (css:function-norm=:=? ?-support 'supports)
                          (let* ([components (css:function-arguments ?-support)]
                                 [supports (list (css-remake-token ?-support css:block #\( components #false))]
                                 [query (css-parse-feature-query supports name)])
                            (values (css-query-support? query (current-css-feature-support?)) ?-media-list))
                          (values #true ?-condition)))
                    (and support? (css-media-queries-support? (css-parse-media-queries media-list name) preferences)
                         (parameterize ([current-css-media-preferences preferences])
                           (read-css-stylesheet ?-target.css pool))
                         (css-stylesheet-path->identity ?-target.css)))])))
    
  (define css-@namespace->namespace : (-> CSS-@Rule (U (Pairof Symbol String) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-namespaces/#syntax
    (lambda [ns]
      (define-values (1st rest) (css-car (css-@rule-prelude ns)))
      (define-values (2nd terminal) (css-car rest))
      (define ?-block : (Option CSS:Block) (css-@rule-block ns))
      (define namespace : (U String CSS-Syntax-Error)
        (let ([uri (if (eof-object? 2nd) 1st 2nd)])
          (cond [(css:string? uri) (css:string-datum uri)]
                [(css:url? uri) (~a (css:url-datum uri))]
                [(eof-object? 1st) (make+exn:css:empty (css-@rule-name ns))]
                [else (make+exn:css:unrecognized uri)])))
      (cond [(exn? namespace) namespace]
            [(css:block? ?-block) (make+exn:css:overconsumption ?-block)]
            [(css-pair? terminal) (make+exn:css:overconsumption terminal)]
            [(css:ident? 1st) (cons (css:ident-datum 1st) namespace)]
            [(eof-object? 2nd) (cons '|| namespace)]
            [else (make+exn:css:unrecognized 1st)])))

  (define css-@media->media-rule : (-> CSS-@Rule CSS-Media-Preferences CSS-Namespace CSS-StyleSheet-Pool
                                       (U (Listof CSS-Grammar-Rule) CSS-Media-Rule CSS-Syntax-Error Void))
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
    (lambda [media preferences namespaces pool]
      (define name : CSS:@Keyword (css-@rule-name media))
      (define ?-block : (Option CSS:Block) (css-@rule-block media))
      (cond [(false? ?-block) (make+exn:css:missing-block name)]
            [(css-null? (css:block-components ?-block)) (void)]
            [else (when (css-media-queries-support? (css-parse-media-queries (css-@rule-prelude media) name) preferences)
                    (define stxes : (Listof CSS-Syntax-Rule) (css-parse-rules (css:block-components ?-block)))
                    (when (pair? stxes)
                      (define-values (viewport _ grammars)
                        (css-syntax-rules->grammar-rules 'src stxes namespaces #false #false preferences pool))
                      (when (pair? grammars)
                        (cond [(eq? viewport preferences) grammars]
                              [else (cons grammars viewport)]))))])))

  (define css-@support->syntax-rules : (-> CSS-@Rule (U (Listof CSS-Syntax-Rule) CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-conditional/#contents-of
    ;;; https://drafts.csswg.org/css-conditional/#at-supports
    (lambda [support]
      (define name : CSS:@Keyword (css-@rule-name support))
      (define ?-block : (Option CSS:Block) (css-@rule-block support))
      (cond [(false? ?-block) (make+exn:css:missing-block name)]
            [(css-null? (css:block-components ?-block)) null]
            [(not (css-query-support? (css-parse-feature-query (css-@rule-prelude support) name) (current-css-feature-support?))) null]
            [else (css-parse-rules (css:block-components ?-block))])))
  
  (define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (Option CSS-Namespace) (U CSS-Style-Rule CSS-Syntax-Error))
    ;;; https://drafts.csswg.org/css-syntax/#style-rules
    ;;; https://drafts.csswg.org/selectors/#invalid
    (lambda [qr namespaces]
      (define prelude : (Listof+ CSS-Token) (css-qualified-rule-prelude qr))
      (define components : (Listof CSS-Token) (css:block-components (css-qualified-rule-block qr)))
      (define ?-selectors : (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error) (css-components->selectors prelude namespaces))
      (cond [(exn? ?-selectors) ?-selectors]
            [else (make-css-style-rule ?-selectors (css-components->declarations components))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-cascade-viewport : (->* (CSS-Media-Preferences (Listof CSS-Declarations))
                                      (CSS-Declaration-Filter (CSS-Cascaded-Value-Filter (HashTable Symbol CSS-Media-Datum)))
                                      CSS-Media-Preferences)
    ;;; https://drafts.csswg.org/css-device-adapt/#atviewport-rule
    (lambda [viewport-preferences viewport-descriptors
                                  [viewport-descriptor-filter (current-css-viewport-descriptor-filter)]
                                  [viewport-filter (current-css-viewport-filter)]]
      (call-with-css-media #:preferences viewport-preferences
        (viewport-filter (css-cascade-declarations viewport-descriptor-filter viewport-descriptors)
                         viewport-preferences
                         #false))))

  (define css-cascade : (All (Preference) (-> (Listof CSS-StyleSheet) CSS-Subject
                                              CSS-Declaration-Filter (CSS-Cascaded-Value-Filter Preference)
                                              Preference (Option CSS-Values) [#:quirk? Boolean]
                                              (Values Preference CSS-Values)))
    ;;; https://drafts.csswg.org/css-cascade/#filtering
    ;;; https://drafts.csswg.org/css-cascade/#cascading
    ;;; https://drafts.csswg.org/css-variables/#cycles
    (lambda [stylesheets subject desc-filter value-filter initial-values inherited-values #:quirk? [quirk? #false]]
      (define declared-values : CSS-Values (make-css-values))
      (let cascade-stylesheets ([batch : (Listof CSS-StyleSheet) stylesheets])
        (for ([stylesheet (in-list batch)])
          (define child-identities : (Listof Positive-Integer) (css-stylesheet-imports stylesheet))
          (cascade-stylesheets (for/list : (Listof CSS-StyleSheet) ([import (in-list child-identities)])
                                 (hash-ref (css-stylesheet-pool stylesheet) import)))
          (css-cascade-rules (css-stylesheet-rules stylesheet) subject desc-filter quirk?
                             (css-stylesheet-preferences stylesheet) declared-values)))
      (css-resolve-variables declared-values inherited-values)
      (values (value-filter declared-values initial-values inherited-values) declared-values)))

  (define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) CSS-Subject CSS-Declaration-Filter)
                                   (Boolean CSS-Media-Preferences CSS-Values) CSS-Values)
    ;;; https://drafts.csswg.org/css-cascade/#filtering
    ;;; https://drafts.csswg.org/css-cascade/#cascading
    ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
    ;;; https://drafts.csswg.org/selectors/#data-model
    (lambda [rules subject desc-filter [quirk? #false] [top-preferences (current-css-media-preferences)] [descbase (make-css-values)]]
      ; NOTE: defined the `Style-Metadata` as `List` will slow down the parsing,
      ;       even though this code is not reached at that stage.
      (define-type Style-Metadata (Vector Nonnegative-Fixnum CSS-Declarations CSS-Media-Preferences))
      (define-values (selected-rules-style single-preference?)
        (let cascade-rule : (Values (Listof Style-Metadata) Boolean) ([preferences : CSS-Media-Preferences top-preferences]
                                                                      [grammars : (Listof CSS-Grammar-Rule) rules]
                                                                      [stylebase : (Listof Style-Metadata) null]
                                                                      [single? : Boolean #true])
          (for/fold ([styles stylebase]
                     [single-preference? single?])
                    ([style (in-list grammars)])
            (cond [(css-@rule? style) (values styles single-preference?)]
                  [(pair? style) (cascade-rule (cdr style) (car style) styles #false)]
                  [else (let ([selectors : (Listof+ CSS-Complex-Selector) (css-style-rule-selectors style)])
                          (define specificity : Nonnegative-Fixnum
                            (for/fold ([max-specificity : Nonnegative-Fixnum 0]) ([selector (in-list selectors)])
                              (define matched-specificity : Nonnegative-Fixnum (or (css-selector-match selector subject quirk?) 0))
                              (fxmax matched-specificity max-specificity)))
                          (cond [(zero? specificity) (values styles single-preference?)]
                                [else (let ([sm : Style-Metadata (vector specificity (css-style-rule-properties style) preferences)])
                                        (values (cons sm styles) single-preference?))]))]))))
      (unless (null? selected-rules-style)
        (define ordered-sources : (Listof Style-Metadata)
          (sort (reverse selected-rules-style)
                (λ [[sm1 : Style-Metadata] [sm2 : Style-Metadata]]
                  (fx< (vector-ref sm1 0) (vector-ref sm2 0)))))
        (call-with-css-media #:preferences top-preferences
          (if (and single-preference?)
              (let ([source-ref (λ [[src : Style-Metadata]] : CSS-Declarations (vector-ref src 1))])
                (css-cascade-declarations desc-filter (map source-ref ordered-sources) descbase))
              (for ([src (in-list ordered-sources)])
                (define alter-preferences : CSS-Media-Preferences (vector-ref src 2))
                (if (eq? alter-preferences top-preferences)
                    (css-cascade-declarations desc-filter (vector-ref src 1) descbase)
                    (call-with-css-media #:preferences alter-preferences
                      (css-cascade-declarations desc-filter (vector-ref src 1) descbase)))))))
      descbase))

  (define css-cascade-declarations : (->* (CSS-Declaration-Filter (U CSS-Declarations (Listof CSS-Declarations))) (CSS-Values) CSS-Values)
    ;;; https://drafts.csswg.org/css-cascade/#shorthand
    ;;; https://drafts.csswg.org/css-cascade/#importance
    ;;; https://drafts.csswg.org/css-variables/#syntax
    ;;; https://drafts.csswg.org/css-variables/#using-variables
    (lambda [desc-filter properties [valuebase (make-css-values)]]
      (define varbase : CSS-Variable-Values (css-values-variables valuebase))
      (define descbase : (HashTable Symbol (-> CSS-Datum)) (css-values-descriptors valuebase))
      (define importants : (HashTable Symbol Boolean) (css-values-importants valuebase))
      (define (desc-more-important? [desc-name : Symbol] [important? : Boolean]) : Boolean
        (or important? (not (hash-has-key? importants desc-name))))
      (define (desc-set! [desc-name : Symbol] [important? : Boolean] [declared-value : (-> CSS-Datum)]) : Void
        (when important? (hash-set! importants desc-name #true))
        (when (eq? desc-name 'all)
          (for ([desc-key (in-list (remq* (current-css-all-exceptions) (hash-keys descbase)))])
            (when (desc-more-important? desc-key important?)
              (hash-set! descbase desc-key declared-value))))
        (hash-set! descbase desc-name declared-value))
      (define #:forall (a) (do-parse [parse : (CSS-Parser a)] [initial : a]
                                     [declared-values : (Listof CSS-Token)] [<desc-name> : CSS:Ident]) : (Option a)
        (define-values (desc-value tail) (parse initial declared-values))
        (cond [(pair? tail) (make+exn:css:overconsumption tail <desc-name>) #false]
              [(false? desc-value) (make+exn:css:type tail <desc-name>) #false]
              [(exn:css? desc-value) (css-log-syntax-error desc-value <desc-name>) #false]
              [else desc-value]))
      (define (parse-long [info : (Pairof (CSS-Parser CSS-Longhand-Values) (Listof Symbol))]
                          [<desc-name> : CSS:Ident] [declared-values : (Listof+ CSS-Token)]
                          [important? : Boolean] [lazy? : Boolean]) : Void
        (cond [(and lazy?)
               (define pending-thunk : (-> (Option CSS-Longhand-Values))
                 (thunk (let ([flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                          (and (css-pair? flat-values) (do-parse (car info) initial-longhand flat-values <desc-name>)))))
               (define &pending-longhand : (Boxof (-> (Option CSS-Longhand-Values))) (box pending-thunk))
               (for ([name (in-list (cdr info))] #:when (desc-more-important? name important?))
                 (desc-set! name important?
                            (thunk (let ([longhand ((unbox &pending-longhand))])
                                     (box-cas! &pending-longhand pending-thunk (thunk longhand))
                                     (define desc-value : CSS-Datum
                                       (cond [(not (hash? longhand)) css:unset]
                                             [else (hash-ref longhand name (thunk css:initial))]))
                                     (hash-set! descbase name (thunk desc-value))
                                     desc-value))))]
              [(do-parse (car info) initial-longhand declared-values <desc-name>)
               => (λ [longhand] (for ([(name desc-value) (in-hash longhand)])
                                  (desc-set! name important? (thunk desc-value))))]))
      (define (parse-desc [info : (CSS-Parser (Listof CSS-Datum))]
                          [<desc-name> : CSS:Ident] [desc-name : Symbol] [declared-values : (Listof+ CSS-Token)]
                          [important? : Boolean] [lazy? : Boolean]) : Void
        (define (normalize [desc-value : (Option (Listof CSS-Datum))]) : (Option CSS-Datum)
          (cond [(pair? desc-value) (if (null? (cdr desc-value)) (car desc-value) (reverse desc-value))]
                [else (and (null? (cdr declared-values)) (css-wide-keywords-ormap (car declared-values)))]))
        (cond [(and lazy?)
               (desc-set! desc-name important?
                          (thunk (let ([flat-values (css-variable-substitute <desc-name> declared-values varbase null)])
                                   (define desc-value : (Option CSS-Datum)
                                     (and (css-pair? flat-values)
                                          (normalize (do-parse info null flat-values <desc-name>))))
                                   (unless (false? desc-value) (hash-set! descbase desc-name (thunk desc-value)))
                                   (or desc-value css:unset))))]
              [(normalize (do-parse info null declared-values <desc-name>))
               => (λ [desc-value] (desc-set! desc-name important? (thunk desc-value)))]))
      (let cascade ([subproperties : (U CSS-Declarations (Listof CSS-Declarations)) properties])
        (for ([property (in-list subproperties)])
          (cond [(list? property) (cascade property)]
                [else (let* ([<desc-name> : CSS:Ident (css-declaration-name property)]
                             [desc-name : Symbol (css:ident-norm <desc-name>)])
                        (cond [(symbol-unreadable? desc-name) (hash-set! varbase desc-name property)]
                              [else (let ([important? : Boolean (css-declaration-important? property)])
                                      (when (desc-more-important? desc-name important?)
                                        (define declared-values : (Listof+ CSS-Token) (css-declaration-values property))
                                        (define lazy? : Boolean (css-declaration-lazy? property))
                                        (define info : (U (Pairof (CSS-Parser CSS-Longhand-Values) (Listof Symbol))
                                                          (CSS-Parser (Listof CSS-Datum)) False Void)
                                          (desc-filter desc-name (thunk (void (make+exn:css:deprecated <desc-name>)))))
                                        (cond [(false? info) (make+exn:css:unrecognized <desc-name>)]
                                              [(void? info) (void '(ignore this property))]
                                              [(pair? info) (parse-long info <desc-name> declared-values important? lazy?)]
                                              [else (parse-desc info <desc-name> desc-name declared-values important? lazy?)])))]))])))
      valuebase))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define css-components->declarations : (-> (Listof CSS-Token) CSS-Declarations)
    (lambda [components]
      (let make-style-rule : CSS-Declarations ([seitreporp : CSS-Declarations null] [tokens : (Listof CSS-Token) components])
        (define-values (id any-values) (css-car tokens))
        (define-values (:values rest)
          (let collect : (Values (Listof CSS-Token) (Listof CSS-Token)) ([seulav : (Listof CSS-Token) null]
                                                                         [rest : (Listof CSS-Token) any-values])
            (define-values (head tail) (css-car/cdr rest))
            (cond [(or (eof-object? head) (css:semicolon? head)) (values (reverse seulav) tail)]
                  [(and (css:block=:=? head #\{) (css:@keyword? id)) (values (reverse (cons head seulav)) tail)]
                  [else (collect (cons head seulav) tail)])))
        (cond [(eof-object? id) (reverse seitreporp)]
              [(css:ident? id) (make-style-rule (css-cons (css-components->declaration id :values) seitreporp) rest)]
              [else (make-style-rule (css-cons (make+exn:css:unrecognized (cons id :values)) seitreporp) rest)]))))

  (define css-stylesheet-path->identity : (-> Path-String Positive-Integer)
    (lambda [uri.css]
      (file-or-directory-identity uri.css)))

  (define css-stylesheet-outdated? : (-> CSS-StyleSheet Boolean)
    (lambda [stylesheet]
      (define location : (U String Symbol) (css-stylesheet-location stylesheet))
      (and (string? location)
           (file-exists? location)
           (< (css-stylesheet-timestamp stylesheet)
              (file-or-directory-modify-seconds location)))))

  (define css-update-imported-stylesheets : (-> CSS-StyleSheet Void)
    (lambda [stylesheet]
      (define pool (css-stylesheet-pool stylesheet))
      (for ([id (in-list (css-stylesheet-imports stylesheet))])
        (define child : (Option CSS-StyleSheet) (hash-ref pool id (const #false)))
        (when (css-stylesheet? child)
          (define child.css : (U Symbol String) (css-stylesheet-location child))
          (when (string? child.css)
            (read-css-stylesheet (string->path child.css) pool))))))
  
  (define css-url-string->path : (-> Any String Path)
    (lambda [parent-location uri]
      (define uri.css : Path-String
        (cond [(absolute-path? (string->path uri)) uri]
              [else (let ([pwd (or (and (string? parent-location) (path-only parent-location)) (current-directory))])
                      (build-path pwd uri))]))
      (simple-form-path uri.css)))

  (define css-resolve-variables : (-> CSS-Values (Option CSS-Values) CSS-Values)
    ;;; https://drafts.csswg.org/css-variables/#cycles
    (lambda [declared-values inherited-values]
      (define declared-vars : CSS-Variable-Values (css-values-variables declared-values))
      (for ([(--var --value) (in-hash declared-vars)])
        (when (and (css-declaration? --value) (css-declaration-lazy? --value))
          (define property : CSS:Ident (css-declaration-name --value))
          (define --values : (Listof+ CSS-Token) (css-declaration-values --value))
          (define flat-values : (Listof CSS-Token) (css-variable-substitute property --values declared-vars (list --var)))
          (hash-set! declared-vars --var
                     (cond [(pair? flat-values) (struct-copy css-declaration --value [values flat-values] [lazy? #false])]
                           [else null]))))
      (unless (false? inherited-values)
        (for ([(--var --value) (in-hash (css-values-variables inherited-values))])
          (hash-ref! declared-vars --var (thunk --value))))
      declared-values))

  (define css-variable-substitute : (-> CSS:Ident (Listof CSS-Token) CSS-Variable-Values (Listof Symbol) (Listof CSS-Token))
    ;;; https://drafts.csswg.org/css-variables/#invalid-variables
    (lambda [property var-values varbase refpath]
      (let var-fold ([seulav : (Listof CSS-Token) null]
                     [--values : (Listof CSS-Token) var-values])
        (define-values (head tail) (css-car/cdr --values))
        (cond [(eof-object? head) (if (css-pair? seulav) (reverse (filter-not css:whitespace? seulav)) seulav)]
              [(css:function? head)
               (cond [(not (css:function-lazy? head)) (var-fold (cons head seulav) tail)] 
                     [else (let ([args (css-variable-substitute property (css:function-arguments head) varbase refpath)])
                             (if (null? args) null (var-fold (cons (css:function-copy head args #false) seulav) tail)))])]
              [(css:λracket? head)
               (cond [(not (css:λracket-lazy? head)) (var-fold (cons head seulav) tail)]
                     [else (let ([args (css-variable-substitute property (css:λracket-arguments head) varbase refpath)])
                             (if (null? args) null (var-fold (cons (css:λracket-copy head args #false) seulav) tail)))])]
              [(css:url? head)
               (cond [(css:url-lazy? head) (var-fold (cons head seulav) tail)]
                     [else (let ([mods (css-variable-substitute property (css:url-modifiers head) varbase refpath)])
                             (cond [(null? mods) null]
                                   [else (var-fold (cons (css:url-copy head (css-url-modifiers-filter head mods) #false) seulav)
                                                   tail)]))])]
              [(css:block? head)
               (cond [(not (css:block-lazy? head)) (var-fold (cons head seulav) tail)]
                     [else (let ([coms (css-variable-substitute property (css:block-components head) varbase refpath)])
                             (if (null? coms) null (var-fold (cons (css:block-copy head coms #false) seulav) tail)))])]
              [(not (css:var? head)) (var-fold (cons head seulav) tail)]
              [(memq (css:var-datum head) refpath) (make+exn:css:cyclic head property 'debug) null]
              [else (let ([--var (css:var-datum head)])
                      (define --value : (U CSS-Declaration Null) (hash-ref varbase --var (thunk null)))
                      (define-values (--vs lazy?)
                        (cond [(null? --value) (values (css:var-fallback head) (css:var-lazy? head))]
                              [else (values (css-declaration-values --value) (css-declaration-lazy? --value))]))
                      (cond [(null? --vs) (make+exn:css:missing-value head property 'debug) null]
                            [(not lazy?) (var-fold (append (reverse --vs) seulav) tail)]
                            [else (let ([vs (css-variable-substitute property --vs varbase (cons --var refpath))])
                                    (if (null? vs) vs (var-fold (append (reverse vs) seulav) tail)))]))])))))

(require (submod "." digitama))
(require (submod "." parser))
(require (submod "." grammar))

;;; Interacting with Racket
(define-type CSS-@λ-Filter (-> Symbol (U Keyword Natural) CSS-Token (U CSS-Datum CSS-Syntax-Error Void)))
(define-type CSS-@λ-Metainfo (Vector (Listof Keyword) (Listof Keyword) (Listof Natural) (U Natural +inf.0)))
(define-type CSS-@λ-Pool (HashTable Symbol CSS-@λ-Metainfo))

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

(define-syntax (define-@λ-pool stx)
  (syntax-case stx []
    [(_ the-@λ-pool #:λnames λnames modpaths ...)
     (with-syntax ([the-pool (parameterize ([current-namespace (make-base-namespace)])
                               (eval `(require ,@(syntax->datum #'(modpaths ...))))
                               (make-pool #'the-@λ-pool (syntax->datum #'λnames)))])
       #'(define the-@λ-pool : CSS-@λ-Pool the-pool))]))

(define-syntax (define-css-declared-racket-value-filter stx)
  (syntax-case stx []
    [(_ value-filter #:with ?-value #:as ValueType asserts ...)
     #'(define value-filter : (->* (CSS:Racket) (Namespace) (U ValueType CSS-Syntax-Error))
         (lambda [racket-id [ns (current-namespace)]]
           (define ?-value (css-eval-value racket-id ns))
           (cond asserts ... [(exn:css? ?-value) ?-value] [else (make-exn:css:contract racket-id)])))]
    [(_ value-filter #:is-a? class% #:as ValueType)
     #'(define-css-declared-racket-value-filter value-filter #:with ?-value #:as ValueType
         [(is-a? ?-value class%) (cast ?-value ValueType)])]
    [(_ value-filter #:? type? #:as ValueType)
     #'(define-css-declared-racket-value-filter value-filter #:with ?-value #:as ValueType
         [(type? ?-value) ?-value])]))

#|(define-css-filter css-@lambda-filter #:-> CSS-@λ
  #:with [desc-value [λfilter : CSS-@λ-Filter] [λpool : CSS-@λ-Pool]]
  [(css:λracket? desc-value)
   (let* ([λid (css:λracket-datum desc-value)]
          [λinfo (hash-ref λpool λid (thunk #false))])
     (cond [(false? λinfo) (make-exn:css:range desc-value)]
           [else (css-@λ-filter desc-value λfilter λinfo)]))])
  
(define css-@λ-filter : (-> CSS:λRacket CSS-@λ-Filter CSS-@λ-Metainfo (U CSS-@λ CSS-Syntax-Error))
  (lambda [<λ> λfilter λinfo]
    (define λname : Symbol (css:λracket-datum <λ>))
    (define λ:all : (Listof Keyword) (vector-ref λinfo 1))
    (define λarities : (Listof Natural) (vector-ref λinfo 2))
    (define λarity-min : (U Natural +inf.0) (vector-ref λinfo 3))
    (let λ-filter ([spxes : (Listof CSS-Datum) null]
                   [args : (Listof CSS-Token) (css:λracket-arguments <λ>)]
                   [position : Natural 0]
                   [λ:kws : (Listof Keyword) (vector-ref λinfo 0)])
      (define-values (head tail) (css-car/cdr args))
      (cond [(css:hash? head)
             (define-values (value rest) (css-car/cdr tail))
             (define λ:kw : Keyword (css:hash-datum head))
             (cond [(eof-object? value) (make-exn:css:arity head)]
                   [(not (memq λ:kw λ:all)) (make-exn:css:unrecognized head)]
                   [else (let ([datum (λfilter λname λ:kw value)])
                           (cond [(exn:css? datum) datum]
                                 [(void? datum) (λ-filter spxes rest position λ:kws)]
                                 [else (λ-filter (list* datum λ:kw spxes) rest position (remq λ:kw λ:kws))]))])]
            [(css-token? head)
             (define datum : (U CSS-Datum CSS-Syntax-Error Void) (λfilter λname position head))
             (cond [(exn:css? datum) datum]
                   [(void? datum) (make-exn:css:arity args)]
                   [else (λ-filter (cons datum spxes) tail (add1 position) λ:kws)])]
            [(nor (null? λ:kws) (or (memq position λarities) (>= position λarity-min))) (make-exn:css:arity <λ>)]
            [else (css-@λ (cons λname (reverse spxes)))]))))|#

(define css-eval-value : (-> CSS:Racket Namespace (U Any CSS-Syntax-Error))
  (lambda [<thing> ns]
    ;;; WARNING: This operation is extremely expensive at first time.
    (with-handlers ([exn? (λ _ (make-exn:css:racket <thing>))])
      (define id : Symbol (css:racket-datum <thing>))
      (define v : Any (call-with-values (thunk (eval id ns)) (λ _ (car _))))
      (if (parameter? v) (v) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require (submod ".." digitama))
  (require (submod ".." parser))
  (require (submod ".." grammar))

  (require "format.rkt")
  (require (only-in typed/racket/gui get-display-size))
  
  (define-syntax (time-run stx)
    (syntax-case stx []
      [(_ sexp ...)
       #'(let ([momery0 : Natural (current-memory-use)])
           (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
           (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n"
                   (~size (- (current-memory-use) momery0) 'Bytes)
                   cpu real gc)
           (car result))]))
  
  (define run-file : Path-String (find-system-path 'run-file))
  (define tamer.css : Path-String
    (simplify-path (cond [(regexp-match? #px"DrRacket$" run-file) (build-path 'up "tamer" "tamer.css")]
                         [else (build-path (find-system-path 'orig-dir) run-file 'up 'up "tamer" "tamer.css")])))
  
  (define-values (width height) (get-display-size))
  (define-values (in out) (make-pipe))
  (define css-logger (make-logger 'css #false))
  (define css (thread (thunk (let forever ([/dev/log (make-log-receiver css-logger 'debug)])
                               (match (sync/enable-break /dev/log)
                                 [(vector level message urgent _)
                                  (cond [(eof-object? urgent) (close-output-port out)]
                                        [else (fprintf out "[~a] ~a~n" level message)
                                              (forever /dev/log)])])))))

  (current-logger css-logger)
  (css-deprecate-media-type #true)
  (current-css-media-type 'screen)
  (current-css-media-preferences
   ((inst make-hash Symbol CSS-Media-Datum)
    (list (cons 'orientation 'landscape)
          (cons 'width (or width 0))
          (cons 'height (or height 0)))))
  
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (define tamer-sheet : CSS-StyleSheet (time-run (read-css-stylesheet tamer.css)))
  (define tamer-root : CSS-Subject (make-css-subject #:type 'root #:id '#:header))
  (define tamer-body : CSS-Subject (make-css-subject #:type 'module #:id '#:root #:classes '(main)))

  (define css-declaration-filter : CSS-Declaration-Filter
    (lambda [suitcased-name !]
      (λ [[initial : (Listof CSS-Datum)] [declared-values : (Listof CSS-Token)]]
        (values (map css-token->datum declared-values) null))))

  (define css-filter : (CSS-Cascaded-Value-Filter (Option (HashTable Symbol Any)))
    (lambda [declared-values default-values inherited-values]
      (for/hash : (HashTable Symbol Any) ([desc-name (in-hash-keys (css-values-descriptors declared-values))])
        (values desc-name (css-ref declared-values inherited-values desc-name)))))

  tamer-sheet
  tamer-root
  (match-define (list preference header-preference)
    (time-run (let-values ([(preference for-children)
                            (css-cascade (list tamer-sheet) tamer-root css-declaration-filter css-filter #false  #false)])
                (list preference for-children))))
  header-preference

  tamer-body
  (time-run (let-values ([(preference for-children)
                          (css-cascade (list tamer-sheet) tamer-body css-declaration-filter css-filter #false header-preference)])
              for-children))

  (map (λ [[in : String]] : (Pairof String Integer)
         (let ([?-complex-selectors (css-parse-selectors in)])
           (cond [(exn:css? ?-complex-selectors) (cons in -1)]
                 [else (let* ([s (car ?-complex-selectors)]
                              [a (css-complex-selector-A s)]
                              [b (css-complex-selector-B s)]
                              [c (css-complex-selector-C s)])
                         (cons in (+ (* a 100) (* b 10) c)))])))
       (list "* + *"
             "li"
             "li::first-line"
             "ul li"
             "ul ol+li"
             "h1 + *[rel=up]"
             "ul ol li.red"
             "li.red.level"
             "#x34y"
             ":not(FOO)#s12"
             ".foo :matches(.bar, #baz)"
             "body #darkside [sith] p"))

  (log-message css-logger 'debug "exit" eof)
  (copy-port in (current-output-port)))
