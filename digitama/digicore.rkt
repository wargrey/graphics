#lang typed/racket

(provide (except-out (all-defined-out) define-tokens define-token define-token-interface
                     define-symbolic-tokens define-numeric-tokens define-dimensional-tokens
                     define-prefab-keyword define-syntax-error css-make-syntax-error
                     css-tee-computed-value css-ref-raw))

(provide (rename-out [exact-nonnegative-integer? natural?]))
(provide (all-from-out racket/flonum racket/fixnum))

(require racket/fixnum)
(require racket/flonum)

(require "misc.rkt")

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-preference stx)
  (syntax-parse stx #:literals [:]
    [(self preference #:as Preference
           (~optional (~seq #:undefined Undef undef? undef)
                      #:defaults ([Undef #'CSS-Wide-Keyword] [undef? #'css-wide-keyword?] [undef #'css:initial]))
           (fields ...)
           options ...)
     #'(self preference #:as Preference #:undefined Undef undef? undef #:with [] (fields ...) options ...)]
    [(self preference #:as Preference
           (~optional (~seq #:undefined Uv uv? uv)
                      #:defaults ([Uv #'CSS-Wide-Keyword] [uv? #'css-wide-keyword?] [uv #'css:initial]))
           #:with [[bindings BindTypes ...] ...]
           ([property : DataType info ...] ...)
           options ...)
     (with-syntax* ([make-preference (format-id #'preference "make-~a" (syntax-e #'preference))]
                    [([property-filter ArgType defval ...] ...)
                     (for/list ([field-info (in-list (syntax->list #'([property DataType info ...] ...)))])
                       (syntax-parse field-info
                         [(p T #:= dv #:~> Super fltr) #'[(let ([v : (U T Uv) fltr]) (if (uv? v) dv v)) (U Super Uv) uv]]
                         [(p T #:= dv #:~> fltr) #'[(let ([v : (U T Uv) fltr]) (if (uv? v) dv v)) Any uv]]
                         [(p T #:~> Super fltr) #'[fltr Super]]
                         [(p T #:~> fltr) #'[fltr Any]]
                         [(p T #:= dv) #'[(if (uv? p) dv p) (U T Uv) uv]]
                         [(p rest ...) (raise-syntax-error (syntax-e #'self) "property requires an algorithm to compute value" #'p)]))]
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
       #'(begin (struct preference ([property : DataType] ...) options ...)
                (define pref-bindings : (Listof Symbol) (list 'properties ...)) ...
                (define (make-preference args ...) : Preference (preference property-filter ...))
                (define-type Preference preference)))]))
  
(define-syntax (define-token-interface stx)
  (syntax-case stx [:]
    [(_ symbolic-prefix : Type id? id-datum #:+ CSS:ID #:eq? type=?)
     (with-syntax ([<id> (format-id #'symbolic-prefix "<~a>" (syntax-e #'symbolic-prefix))]
                   [id=<-? (format-id #'symbolic-prefix "~a=<-?" (syntax-e #'symbolic-prefix))]
                   [id=:=? (format-id #'symbolic-prefix "~a=:=?" (syntax-e #'symbolic-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                  [Any (U (-> Type Boolean) (Listof Type)) -> (Option Type) : #:+ CSS:ID]))
                  (lambda [token range?]
                    (and (id? token)
                         (let ([datum : Type (id-datum token)])
                           (cond [(procedure? range?) (and (range? datum) datum)]
                                 [else (and (member datum range? type=?) datum)])))))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                [(U (-> Type Boolean) (Listof Type) Type) -> (CSS:Filter Type)]
                                                [-> (CSS:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : CSS-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(range?) (cond [(procedure? range?)
                                     (λ [[t : CSS-Syntax-Any]]
                                       (and (id? t)
                                            (or (let ([d : Type (id-datum t)]) (and (range? d) d))
                                                (make-exn:css:range t))))]
                                    [(list? range?)
                                     (λ [[t : CSS-Syntax-Any]]
                                       (and (id? t)
                                            (let ([d : Type (id-datum t)])
                                              (cond [(member d range? type=?) d]
                                                    [else (make-exn:css:range t)]))))]
                                    [else (λ [[t : CSS-Syntax-Any]]
                                            (and (id? t)
                                                 (let ([d : Type (id-datum t)])
                                                   (if (type=? d range?) d (make-exn:css:range t)))))])]))

                (define id=:=? : (-> Any Type (Option Type) : #:+ CSS:ID) #| for performance |#
                  (lambda [t v]
                    (and (id? t)
                         (let ([d : Type (id-datum t)])
                           (and (type=? d v) d)))))))]
    [(_ numeric-prefix : Type id? id-datum #:+ CSS:ID #:= type=?)
     (with-syntax ([<id> (format-id #'numeric-prefix "<~a>" (syntax-e #'numeric-prefix))]
                   [id=<-? (format-id #'numeric-prefix "~a=<-?" (syntax-e #'numeric-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                  [Any (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                  [Any Type (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                  [Any (Listof Type) -> (Option Type) : #:+ CSS:ID]))
                  (case-lambda
                    [(token op n)   (and (id? token) (let ([d : Type (id-datum token)]) (and (op d n) d)))]
                    [(token l op r) (and (id? token) (let ([m : Type (id-datum token)]) (and (op l m) (op m r) m)))]
                    [(token range?) (and (id? token) (let ([d : Type (id-datum token)])
                                                       (cond [(procedure? range?) (and (range? d) d)]
                                                             [else (for/or : (Option Type) ([v (in-list range?)])
                                                                     (and (type=? d v) d))])))]))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                [(-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                [Type (-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                [(Listof Type) -> (CSS:Filter Type)]
                                                [-> (CSS:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : CSS-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(op n) (λ [[t : CSS-Syntax-Any]]
                              (and (id? t)
                                   (let ([d : Type (id-datum t)])
                                     (if (op d n) d (make-exn:css:range t)))))]
                    [(l op r) (λ [[t : CSS-Syntax-Any]]
                                (and (id? t)
                                     (let ([m : Type (id-datum t)])
                                       (if (and (op l m) (op m r)) m (make-exn:css:range t)))))]
                    [(range?) (λ [[t : CSS-Syntax-Any]]
                                (and (id? t)
                                     (let ([d : Type (id-datum t)])
                                       (or (cond [(procedure? range?) (and (range? d) d)]
                                                 [(list? range?) (and (member d range? type=?) d)]
                                                 [else (and (type=? d range?) d)])
                                           (make-exn:css:range t)))))]))))]))

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
    [(_ parent #:+ _ [id #:+ ID #:=> canonical-unit [conversion ...]] ...)
     (with-syntax ([token->datum (format-id #'parent "~a->datum" (syntax-e #'parent))]
                   [token-filter (format-id #'parent "~a-filter" (syntax-e #'parent))]
                   [([id? +id? css:id->scalar css-id->scalar <id> <+id>] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (define varname (symbol->string (syntax-e <id>)))
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a?" (string-replace varname ":" "+"))
                            (format-id <id> "~a->scalar" (syntax-e <id>))
                            (format-id <id> "~a->scalar" (string-replace varname ":" "-"))
                            (format-id <id> "<~a>" (syntax-e <id>))
                            (format-id <id> "<~a>" (string-replace varname "css:" "css+"))))]
                   [([Flonum/Font Flunum/Font !font?] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (if (not (eq? (syntax-e <id>) 'css:length))
                          (list #'Flonum #'Nonnegative-Flonum #'#true)
                          (list #'(U Flonum CSS:Length:Font) #'(U Nonnegative-Flonum CSS:Length:Font)
                                #'(not (css:length:font? token)))))])
       #'(begin (struct: id : ID parent ()) ...

                (define css-id->scalar : (case-> [Nonnegative-Flonum Symbol -> Nonnegative-Flonum]
                                                 [Flonum Symbol -> Flonum])
                  (lambda [canonical-unit unit]
                    (case unit
                      [(canonical-unit) canonical-unit]
                      conversion ...
                      [else +nan.0])))
                ...

                (define css:id->scalar : (case-> [(U ID CSS-Zero) -> Flonum]
                                                 [(U ID CSS-Zero) True -> Flonum]
                                                 [(U ID CSS-Zero) False -> Nonnegative-Flonum])
                  (lambda [token [direction? #false]]
                    (cond [(not (id? token)) 0.0]
                          [(and direction?) (css-id->scalar (css:dimension-datum token) (css:dimension-unit token))]
                          [else (css-id->scalar (flabs (css:dimension-datum token)) (css:dimension-unit token))])))
                ...

                (define +id? : (-> Any Boolean : #:+ ID)
                  (lambda [token]
                    (and (id? token)
                         (fl> (css:dimension-datum token) 0.0))))
                ...

                (define <id> : (case-> [-> (CSS:Filter Flonum/Font)]
                                       [False -> (CSS:Filter Flonum/Font)]
                                       [True -> (CSS:Filter Flonum)])
                  (lambda [[ignore-font? #false]]
                    (λ [[token : CSS-Syntax-Any]]
                      (cond [(id? token) (if (or ignore-font? !font?) (css:id->scalar token) token)]
                            [(css:dimension? token) (make-exn:css:unit token)]
                            [else #false]))))
                ...

                (define <+id> : (case-> [-> (CSS:Filter Flunum/Font)]
                                        [False -> (CSS:Filter Flunum/Font)]
                                        [True -> (CSS:Filter Nonnegative-Flonum)])
                  (lambda [[ignore-font? #false]]
                    (λ [[token : CSS-Syntax-Any]]
                      (cond [(+id? token) (if (or ignore-font? !font?) (css:id->scalar token #false) token)]
                            [(id? token) (make-exn:css:range token)]
                            [(css:dimension? token) (make-exn:css:unit token)]
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

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens css-bad-token #:+ CSS-Bad-Token
    [css:bad            #:+ CSS:Bad             #:as String]
    [css:close          #:+ CSS:Close           #:as Char])
    
  ; TODO: Typed Racket is buggy if there are more than 11 conditions for (token->datum)
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
                        [[(dpcm)  (fl* dppx (fl/ 2.54 96.0))]
                         [(dpi)   (fl* dppx (fl/ 1.0 96.0))]
                         [(x)     dppx]]])

  (define-symbolic-tokens css-unreadable-token #:+ CSS-Unreadable-Token
    ; These tokens are remade by the parser instead of being produced by the tokenizer.
    [css:ratio          #:+ CSS:Ratio           #:as Positive-Exact-Rational]
    [css:racket         #:+ CSS:Racket          #:as Symbol]
    [css:unquote        #:+ CSS:Unquote         #:as String]))
  
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

(define positive-flonum? : (-> Any Boolean : #:+ Positive-Flonum) (λ [v] (and (flonum? v) (fl> v 0.0))))
(define nonnegative-flonum? : (-> Any Boolean : #:+ Nonnegative-Flonum) (λ [v] (and (flonum? v) (fl>= v 0.0))))
  
(define positive-single-flonum? : (-> Any Boolean : #:+ Positive-Single-Flonum) (λ [v] (and (single-flonum? v) (> v 0.0f0))))
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
          [(css-numeric? instance) (css-numeric-representation instance)]
          [(css:@keyword? instance) (keyword->string (css:@keyword-datum instance))]
          [(css:hash? instance) (~a "#" (keyword->string (css:hash-datum instance)))]
          [(css:match? instance) (~a (css:match-datum instance) '=)]
          [(css:delim=:=? instance #\tab) "||"]
          [(css:string? instance) (~s (css:string-datum instance))]
          [else (~a (css-token->datum instance))])))
  
(define css-token->string : (->* (CSS-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (format "~a:~a:~a: ~a: ~a" (css-token-source instance) (css-token-line instance) (add1 (css-token-column instance))
            (or (object-name alt-object) (object-name instance))
            (or alt-datum (css-token-datum->string instance)))))

(define css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error)
                                    (U CSS-Syntax-Any (Listof CSS-Token))
                                    CSS-Syntax-Error)
  (let ([empty-stacks (continuation-marks #false)])
    (lambda [exn:css any]
      (define (token->exn [main : CSS-Token]) : CSS-Syntax-Error
        (exn:css (css-token->string main exn:css) empty-stacks (list (css-token->syntax main))))
      (define (tokens->exn [head : CSS-Token] [others : (Listof CSS-Token)]) : CSS-Syntax-Error
        (exn:css (format "~a ~a" (css-token->string head exn:css) (map css-token-datum->string others))
                 empty-stacks (map css-token->syntax (cons head others))))
      (match any
        [(or (? eof-object?) (list)) (exn:css (~a eof) empty-stacks null)]
        [(list token) (token->exn token)]
        [(list main others ...) (tokens->exn main (filter-not css:whitespace? others))]
        [(? css:function? main) (tokens->exn main (css:function-arguments main))]
        [(? css:λracket? main) (tokens->exn main (css:λracket-arguments main))]
        [(? css:block? main) (tokens->exn main (css:block-components main))]
        [(? css-token?) (token->exn any)]))))

(define css-log-error : (->* ((U exn CSS:Bad)) (Any Log-Level Symbol) Void)
  (lambda [errobj [src #false] [level 'debug] [topic 'exn:css:fail]]
    (define message : String
      (cond [(css:bad? errobj) (css-token->string errobj)]
            [else (format "@~s: ~a: ~a" src (object-name errobj) (read-line (open-input-string (exn-message errobj))))]))
    (log-message (current-logger) level topic message errobj)))
  
(define css-log-read-error : (->* ((U exn CSS:Bad)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (css-log-error errobj src level 'exn:css:read)))

(define css-log-eval-error : (->* ((U exn CSS:Bad)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (css-log-error errobj src level 'exn:css:eval)))
  
(define css-log-syntax-error : (->* (CSS-Syntax-Error) ((Option CSS:Ident) Log-Level) Void)
  (lambda [errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define topic : Symbol 'exn:css:syntax)
    (define msg : String (exn-message errobj))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(false? property) (log-message logger level topic msg errobj)]
          [(not <eof>?) (log-message logger level topic (format "~a @‹~a›" msg (css:ident-datum property)) errobj)]
          [else (let ([eof-msg (css-token->string property errobj eof)])
                  (log-message logger level topic (format "~a @‹~a›" eof-msg (css:ident-datum property)) errobj))])))

;;; https://drafts.csswg.org/css-syntax/#parsing
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
  [exn:css:type:identifier    #:-> exn:css:type]
  [exn:css:type:variable      #:-> exn:css:type:identifier]
  [exn:css:range              #:-> exn:css:unrecognized]
  [exn:css:unit               #:-> exn:css:range]
  [exn:css:overconsumption    #:-> exn:css:unrecognized]
  [exn:css:enclosed           #:-> exn:css:overconsumption]
  [exn:css:malformed          #:-> exn:css]
  [exn:css:arity              #:-> exn:css:malformed]
  [exn:css:empty              #:-> exn:css:malformed]
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
;    WARNING: Notations are not following the CSS Specifications https://drafts.csswg.org/css-values/#component-combinators
(define-type (CSS-Multiplier idx) (U idx (List idx) (Pairof (U idx Symbol) (U idx Symbol))))
(define-type (CSS-Maybe css) (U css CSS-Wide-Keyword))
(define-type (CSS-Option css) (U css CSS-Syntax-Error False))
(define-type (CSS:Filter css) (-> CSS-Syntax-Any (CSS-Option css)))
(define-type (CSS-Parser css) (-> css (Listof CSS-Token) (Values (CSS-Option css) (Listof CSS-Token))))
(define-type CSS-Shorthand-Parser (CSS-Parser CSS-Longhand-Values))
(define-type CSS-Longhand-Update (-> Symbol CSS-Datum CSS-Datum CSS-Datum))
  
(define-syntax (define-css-disjoined-filter stx)
  (syntax-case stx [:]
    [(_ compound-filter #:-> RangeType #:with [[dom : DomType defval ...] ...] atom-filters ...)
     #'(define (compound-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...))]
    [(_ compound-filter #:-> RangeType atom-filters ...)
     #'(define (compound-filter) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...))]))
  
(define-syntax (define-css-atomic-filter stx)
  (syntax-case stx [:]
    [(_ atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where [defaux ...])
     #'(define (atom-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) defaux ...
         (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
           (cond [(css:token? token) atom-body ...]
                 [else #false])))]
    [(defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ...)
     #'(defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where [])]))

(define-syntax (define-css-function-filter stx)
  (define (parse-pattern <constructor> <matches>)
    (define-values (snerttap stnemugra)
      (for/fold ([snrettap null] [stnemugra null])
                ([<pattern> (in-list <matches>)])
        (syntax-case <pattern> [: _]
          [(field ? type?) (values (cons #'(? type? field) snrettap) (cons #'field stnemugra))]
          [(field ? type? ...) (values (cons #'(? (λ [v] (or (type? v) ...)) field) snrettap) (cons #'field stnemugra))]
          [(field : Type) (values (cons #'(? (make-predicate Type) field) snrettap) (cons #'field stnemugra))]
          [(field : Type ...) (values (cons #'(? (make-predicate (U Type ...)) field) snrettap) (cons #'field stnemugra))]
          [,field (values (cons #'field snrettap) (cons #'field stnemugra))]
          [_ (let ([? (datum->syntax #'_ (gensym))]) (values (cons ? snrettap) (cons ? stnemugra)))]
          [field (values snrettap (cons #'field stnemugra))])))
    (list (list* #'list #'_ (reverse snerttap)) (cons <constructor> (reverse stnemugra))))
  (syntax-parse stx
    [(self func-filter #:-> RangeType
           [(fname aliases ...) #:=> [transforms ...] fparser ...] ...
           (~optional (~seq #:where [defaux ...])))
     (with-syntax ([defines (if (attribute defaux) #'(begin defaux ...) #'(void))]
                   [((([pattern ...] [transform ...]) ...) ...)
                    (for/list ([<function> (in-list (syntax->list #'(fname ...)))]
                               [<transforms> (in-list (syntax->list #'([transforms ...] ...)))])
                      (define transforms (syntax-e <transforms>))
                      (when (null? transforms) (raise-syntax-error (syntax-e #'self) "empty value transform" <function>))
                      (for/list ([<transform> (in-list transforms)])
                        (define transform (syntax-e <transform>))
                        (cond [(pair? transform) (parse-pattern (car transform) (cdr transform))]
                              [(null? transform) (raise-syntax-error (syntax-e #'self) "missing value constructor" <transform>)]
                              [else (let ([? (datum->syntax <transform> (gensym))])
                                      (list (list #'? #'list? ?) (list #'values ?)))])))])
       #'(define (func-filter) : (CSS:Filter RangeType) defines
           (define do-parse : (-> Symbol (CSS-Parser (Listof CSS-Datum)) (Listof CSS-Token) (U (Listof CSS-Datum) CSS-Syntax-Error))
             (lambda [func-name func-parse func-argl]
               (define-values (fargs --tokens) (func-parse (list func-name) func-argl))
               (cond [(exn:css? fargs) fargs]
                     [(false? fargs) (make-exn:css:type --tokens)]
                     [(pair? --tokens) (make-exn:css:overconsumption --tokens)]
                     [else (reverse fargs)])))
           (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
             (and (css:function? token)
                  (let ([argl : (Listof CSS-Token) (css:function-arguments token)])
                    (case (css:function-norm token)
                      [(fname aliases ...)
                       (match (do-parse 'fname (CSS<+> fparser ...) argl)
                         [(pattern ...) (transform ...)] ...
                         [(? exn? e) e]
                         [_ (make-exn:css:arity token)])]
                      ...
                      [else (make-exn:css:range token)]))))))]))

(define-syntax (define-css-system-parameters-filter stx)
  (syntax-case stx []
    [(_ <id> #:-> RangeType [css racket] ... [otherwise last-one])
     (with-syntax ([(current ... current-last)
                    (let ([suffix (string-downcase (symbol->string (syntax-e #'RangeType)))])
                      (for/list ([<p> (in-list (syntax->list #'(css ... otherwise)))])
                        (format-id <p> "default-css-~a-~a" (syntax-e <p>) suffix)))])
       #'(begin (define current : (Parameterof RangeType) (make-parameter racket)) ...
                (define current-last : (Parameterof RangeType) (make-parameter last-one))
                (define-css-disjoined-filter <id> #:-> RangeType
                  (CSS:<~> (<css:ident-norm> (list 'css ... 'otherwise))
                           (λ [[id : Symbol]] : RangeType
                             (case id [(css) (current)] ... [else (current-last)]))))))]))

(define-syntax (make-css->size stx)
  (syntax-parse stx
    [(_ #:100% fl% #:= defval (~optional (~seq #:as NanType)) (~optional (~seq (~and #:no-direction +))))
     (with-syntax ([SizeType (if (attribute NanType) #'NanType #'defval)]
                   [(fl? sfl? length? FLType direction)
                    (cond [(not (attribute +)) (list #'flonum? #'single-flonum? #'css:length? #'Flonum #'#true)]
                          [else (list #'nonnegative-flonum? #'nonnegative-single-flonum? #'css+length?
                                      #'Nonnegative-Flonum #'#false)])])
       #'(lambda [[_ : Symbol] [size : CSS-Datum]] : (U FLType SizeType)
           (cond [(fl? size) size]
                 [(sfl? size) (fl* (real->double-flonum size) fl%)]
                 [(length? size) (css:length->scalar size direction)]
                 [else defval])))]))
  
(define-syntax (CSS<?> stx)
  (syntax-case stx [else]
    [(_) #'values]
    [(_ [else <else> ...]) #'(CSS<&> <else> ...)]
    [(_ [<if> <then> ...]) #'(css:if <if> (CSS<&> <then> ...) #false)]
    [(_ [<if> <then> ...] [else <else> ...]) #'(css:if <if> (CSS<&> <then> ...) (CSS<&> <else> ...))]
    [(_ [<if> <then> ...] ... [else <else> ...]) #'(css:if (list (cons <if> (CSS<&> <then> ...)) ...) (CSS<&> <else> ...))]
    [(_ [<if> <then> ...] ...) #'(css:if (list (cons <if> (CSS<&> <then> ...)) ...) #false)]))
  
(define-syntax (CSS:<+> stx)
  (syntax-case stx []
    [(_ css-filter) #'css-filter]
    [(_ css-filter css-filters ...) #'(css:disjoin css-filter (CSS:<+> css-filters ...))]))
  
(define-syntax (CSS:<~> stx)
  (syntax-case stx []
    [(_ css-filter f) #'(css:compose css-filter f)]
    [(_ css-filter f g) #'(css:compose (css:compose css-filter g) f)]
    [(_ css-filter f g h ...) #'(css:compose css-filter (compose f g h ...))]))

(define CSS:<=> : (All (a b) (-> (CSS:Filter a) b (CSS:Filter b)))
  (lambda [css-filter const]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum const)))))

(define CSS:<?> : (All (a b c) (case-> [(CSS:Filter a) (-> (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error) -> (CSS:Filter a)]
                                       [(CSS:Filter a) b (-> CSS-Syntax-Error c) -> (CSS:Filter (U a b c))]))
  (case-lambda
    [(css:filter make-exn)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(or (false? datum) (exn:css? datum)) (make-exn token)]
             [else datum]))]
    [(css:filter false-value fexn-value)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(false? datum) false-value]
             [(exn:css? datum) (or (fexn-value datum) datum)]
             [else datum]))]))
  
(define CSS<^> : (All (a) (case-> [(U (CSS:Filter CSS-Datum) (Listof (CSS:Filter CSS-Datum))) -> (CSS-Parser (Listof CSS-Datum))]
                                  [(CSS:Filter CSS-Datum) Symbol -> CSS-Shorthand-Parser]
                                  [(CSS:Filter CSS-Datum) Symbol CSS-Longhand-Update -> CSS-Shorthand-Parser]
                                  [(CSS-Parser (Listof CSS-Datum)) (List Symbol) -> CSS-Shorthand-Parser]
                                  [(CSS-Parser (Listof CSS-Datum)) (List Symbol) CSS-Longhand-Update -> CSS-Shorthand-Parser]
                                  [(CSS:Filter a) (->* (a) (CSS-Longhand-Values) CSS-Longhand-Values) -> CSS-Shorthand-Parser]))
  (case-lambda
    [(atom-filter)
     (if (list? atom-filter)
         (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
           (let datum-fold ([data++ : (Listof CSS-Datum) data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [filters : (Listof (CSS:Filter CSS-Datum)) atom-filter])
             (cond [(null? filters) (values data++ tokens--)]
                   [else (let ([css-filter (car filters)])
                           (define-values (token --tokens) (css-car/cdr tokens--))
                           (define datum : (CSS-Option CSS-Datum) (css-filter token))
                           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens--)]
                                 [else (datum-fold (cons datum data++) --tokens (cdr filters))]))])))
         (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
           (define-values (head tail) (css-car/cdr tokens))
           (define datum : (CSS-Option CSS-Datum) (atom-filter head))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (cons datum data) tail)])))]
    [(atom-filter tag)
     (cond [(symbol? tag)
            (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (token --tokens) (css-car/cdr tokens))
              (define datum : (CSS-Option CSS-Datum) (atom-filter token))
              (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                    [else (values (hash-set data tag datum) --tokens)]))]
           [(list? tag)
            (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (subdata --tokens) (atom-filter null tokens))
              (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
                    [else (values (hash-set data (car tag) (reverse subdata)) --tokens)]))]
           [(λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
              (define-values (token --tokens) (css-car/cdr tokens))
              (define datum : (CSS-Option a) (atom-filter token))
              (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                    [else (values (tag datum data) --tokens)]))])]
    [(atom-filter tag updater)
     (if (symbol? tag)
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum : (CSS-Option CSS-Datum) (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (hash-update data tag (λ [[v : CSS-Datum]] (updater tag v datum)) (thunk #false)) --tokens)]))
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (define-values (subdata --tokens) (atom-filter null tokens))
           (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
                 [else (values (hash-update data (car tag)
                                            (λ [[v : CSS-Datum]] (updater (car tag) v (reverse subdata)))
                                            (thunk #false))
                               --tokens)])))]))

(define CSS<$> : (case-> [(CSS-Parser (Listof CSS-Datum)) -> (CSS-Parser (Listof CSS-Datum))]
                         [(CSS-Parser (Listof CSS-Datum)) CSS-Datum -> (CSS-Parser (Listof CSS-Datum))]
                         [(CSS:Filter CSS-Datum) Symbol (-> CSS-Longhand-Values CSS-Datum) -> CSS-Shorthand-Parser]
                         [(CSS-Parser (Listof CSS-Datum)) (List Symbol) (-> CSS-Longhand-Values CSS-Datum) -> CSS-Shorthand-Parser])
  (case-lambda
    [(css-parser)
     (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values data null)]))]
    [(css-parser eof-value)
     (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values (cons eof-value data) null)]))]
    [(css:filter tag fdatum)
     (if (symbol? tag)
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
                 [else (values (hash-set data tag (fdatum data)) null)]))
         (λ [[data : CSS-Longhand-Values] [tokens : (Listof CSS-Token)]]
           (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
                 [else (values (hash-set data (car tag) (fdatum data)) null)])))]))

(define CSS<~> : (All (a) (-> (CSS-Parser a) (-> a a) (CSS-Parser a)))
  (lambda [css-parser data=>data]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values (data=>data ++data) --tokens)]))))

(define CSS<_> : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [css-parser]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values data --tokens)]))))

(define CSS<+> : (All (a) (-> (CSS-Parser a) (CSS-Parser a) * (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#comb-one
  (lambda [head-branch . tail-branches]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let switch ([head-parser : (CSS-Parser a) head-branch]
                   [tail-parsers : (Listof (CSS-Parser a)) tail-branches])
        (define-values (++data --tokens) (head-parser data tokens))
        (cond [(nor (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
              [(pair? tail-parsers) (switch (car tail-parsers) (cdr tail-parsers))]
              [else (values ++data --tokens)])))))

(define CSS<&> : (All (a) (-> (CSS-Parser a) * (CSS-Parser a)))
  ;;; TODO: https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(css-parser) css-parser]
    [css-parsers (λ [[data : a] [tokens : (Listof CSS-Token)]]
                   (let datum-fold ([data++ : a data]
                                    [tokens-- : (Listof CSS-Token) tokens]
                                    [parsers : (Listof (CSS-Parser a)) css-parsers])
                     (cond [(null? parsers) (values data++ tokens--)]
                           [else (let ([css-parser (car parsers)])
                                   (define-values (++data --tokens) (css-parser data++ tokens--))
                                   (cond [(or (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
                                         [else (datum-fold ++data --tokens (cdr parsers))]))])))]))
  
(define CSS<*> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  ;;; https://drafts.csswg.org/css-values/#comb-any
  (lambda [css-parser [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : a] [tokens : (Listof CSS-Token)]]
                  (let seq ([data++ : a data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [n : Natural 1])
                    (define-values (++data --tokens) (css-parser data++ tokens--))
                    (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n) (values data++ tokens--) (values ++data --tokens))]
                          [(= n most) (values ++data --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (seq ++data --tokens (add1 n))])))])))

(define CSS<#> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let seq ([data++ : a data]
                [tokens-- : (Listof CSS-Token) tokens]
                [n : Natural 1])
        (define-values (++data tail) (css-parser data++ tokens--))
        (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n) (values data++ tokens--) (values ++data tail))]
              [(= n most) (values ++data tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(eof-object? ?comma) (seq ++data --tokens (add1 n))] ; to check least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) --tokens)]
                            [(null? --tokens) (values (make-exn:css:overconsumption ?comma) --tokens)]
                            [else (seq ++data --tokens (add1 n))]))])))))

(define CSS<!> : (->* ((CSS-Parser (Listof CSS-Datum))) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof CSS-Datum)))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof CSS-Datum)] [tokens : (Listof CSS-Token)]]
      (let seq ([sub++ : (Listof CSS-Datum) null]
                [tokens-- : (Listof CSS-Token) tokens]
                [n : Natural 1])
        (define-values (subdata --tokens) (css-parser sub++ tokens--))
        (cond [(or (false? subdata) (exn:css? subdata))
               (cond [(< least n) (values (cons (reverse sub++) data) tokens--)]
                     [else (values subdata --tokens)])]
              [(= n most) (values (cons (reverse subdata) data) --tokens)]
              [else (seq subdata --tokens (add1 n))])))))

(define css:disjoin : (All (a b) (-> (CSS:Filter a) (CSS:Filter b) (CSS:Filter (U a b))))
  (lambda [css-filter1 css-filter2]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter1 token))
      (cond [(false? datum) (css-filter2 token)]
            [else datum]))))
  
(define css:compose : (All (a b) (-> (CSS:Filter a) (-> a b) (CSS:Filter b)))
  (lambda [css-filter css->racket]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum (css->racket datum))))))

(define css:if : (All (a) (case-> [(Listof+ (Pairof (CSS:Filter CSS-Datum) (CSS-Parser a))) (Option (CSS-Parser a)) -> (CSS-Parser a)]
                                  [(CSS:Filter CSS-Datum) (CSS-Parser a) (Option (CSS-Parser a)) -> (CSS-Parser a)]))
  (case-lambda
    [(cond-parsers else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let else-if ([branch (car cond-parsers)]
                     [branches-- (cdr cond-parsers)])
         (define-values (if:filter then-parser) (css-car/cdr branch))
         (define-values (token --tokens) (css-car/cdr tokens))
         (define if:datum : (CSS-Option CSS-Datum) (if:filter token))
         (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
               [(pair? branches--) (else-if (car branches--) (cdr branches--))]
               [(false? else-parser) (values if:datum tokens)]
               [else (else-parser data tokens)])))]
    [(if:filter then-parser else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (define-values (token --tokens) (css-car/cdr tokens))
       (define if:datum : (CSS-Option CSS-Datum) (if:filter token))
       (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
             [(false? else-parser) (values if:datum tokens)]
             [else (else-parser data tokens)]))]))

(define css:multiplier-range : (-> (U (CSS-Multiplier Index) '+ '? '*) Index (Values Natural (U Natural +inf.0)))
  (lambda [multiplier least]
    (case multiplier
      [(?) (values 0 1)]
      [(*) (values 0 +inf.0)]
      [(+) (values 1 +inf.0)]
      [else (cond [(index? multiplier) (values (max multiplier least) (max multiplier least))]
                  [else (let ([n (let ([n (car multiplier)]) (if (index? n) (max n least) least))]) 
                          (define m : (U Index Symbol Null) (cdr multiplier))
                          (cond [(index? m) (values n (max n m))]
                                [(symbol? m) (values n +inf.0)]
                                [else (values n n)]))])])))
  
(define-css-disjoined-filter <css-boolean> #:-> (U Zero One)
  (CSS:<=> (<css:integer> = 0) 0)
  (CSS:<=> (<css:integer> = 1) 1))

(define-css-disjoined-filter <css-keyword> #:-> Symbol
  #:with [[options : (U (Listof Symbol) Symbol)]]
  (<css:ident-norm> options))
  
(define-css-disjoined-filter <css-natural> #:-> Natural
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (<css:integer> exact-positive-integer?)]
        [else    (<css:integer> exact-nonnegative-integer?)]))

(define-css-disjoined-filter <css+real> #:-> (U Natural Nonnegative-Flonum)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:flonum> positive-flonum?) (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:flonum> nonnegative-flonum?) (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoined-filter <css+%real> #:-> (U Natural Nonnegative-Inexact-Real)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:percentage> positive-single-flonum?)
                          (<css:flonum> positive-flonum?)
                          (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:percentage> nonnegative-single-flonum?)
                          (<css:flonum> nonnegative-flonum?)
                          (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoined-filter <css-flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define-css-disjoined-filter <css-%flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:percentage> 0f0 <= 1f0) flabs real->double-flonum)
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define <:css-keywords:> : (->* ((Listof Symbol)) (Symbol) (CSS-Parser (Listof CSS-Datum)))
  (lambda [options [none 'none]]
    (CSS<+> (CSS<^> (CSS:<=> (<css-keyword> none) null))
            (CSS<*> (CSS<^> (<css-keyword> options)) '+))))

(define (<css-comma>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\,) make-exn:css:missing-comma))
(define (<css-slash>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\/) make-exn:css:missing-slash))

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
       (<css+length> #true)]
      [(aspect-ratio device-aspect-ratio)
       (when (eq? downcased-name 'device-aspect-ratio) (deprecated!))
       (CSS:<~> (<css:ratio>) real->double-flonum)]
      [(resolution) (CSS:<+> (CSS:<=> (<css-keyword> 'infinite) +inf.0) (<css:resolution>))]
      [(color color-index monochrome) (<css:integer> exact-nonnegative-integer?)]
      [(grid) #|legacy descriptor|# (when (false? min/max?) (<css-boolean>))]
      [(orientation) (<css-keyword> '(portrait landscape))]
      [(scan) (<css-keyword> '(interlace progressive))]
      [(update) (<css-keyword> '(none slow fast))]
      [(overflow-block) (<css-keyword> '(none scroll optional-paged paged))]
      [(overflow-inline) (<css-keyword> '(none scroll))]
      [(color-gamut) (<css-keyword> '(srgb p3 rec2020))]
      [(pointer any-pointer) (<css-keyword> '(none coarse fine))]
      [(havor any-havor) (<css-keyword> '(none havor))]
      [(scripting) (<css-keyword> '(none initial-only enabled))])))

(define css-deprecate-media-type : (Parameterof Boolean) (make-parameter #false))
(define default-css-media-type : (Parameterof Symbol) (make-parameter 'all))
  
(define-values (default-css-media-preferences default-css-media-feature-filter default-css-feature-support?)
  (values (make-parameter ((inst make-hasheq Symbol CSS-Media-Datum)))
          (make-parameter css-media-feature-filter)
          (make-parameter (const #false))))

;; https://drafts.csswg.org/css-cascade/#shorthand
;; https://drafts.csswg.org/css-cascade/#filtering
;; https://drafts.csswg.org/css-cascade/#cascading
(define-type CSS-Datum
  (Rec css (U CSS-Token-Datum Boolean Bytes FlVector FxVector CSS-Token --datum (Object)
              ; (Pairof css css) messes up things when (list? datum)ing. 
              (Listof css) (Vectorof css) (Boxof css))))
  
(define-type CSS-Longhand-Values (HashTable Symbol CSS-Datum))
(define-type CSS-Variable-Values (HashTable Symbol (U CSS-Declaration Null)))
(define-type CSS-Cascading-Declarations (U CSS-Declarations (Listof CSS-Declarations)))
(define-type CSS-Declaration-Parser (U (Pairof CSS-Shorthand-Parser (Listof Symbol)) (CSS-Parser (Listof CSS-Datum)) Void False))
(define-type CSS-Declaration-Parsers (-> Symbol (-> Void) CSS-Declaration-Parser))
(define-type (CSS-Cascaded-Value-Filter Preference) (-> CSS-Values Preference (Option CSS-Values) Preference))
  
(struct --datum () #:transparent)

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

; https://drafts.csswg.org/css-cascade/#all-shorthand
; https://drafts.csswg.org/css-values/#common-keywords
(define default-css-all-exceptions : (Parameterof (Listof Symbol)) (make-parameter (list 'direction 'unicode-bidi)))
(define-prefab-keyword css-wide-keyword #:as CSS-Wide-Keyword [initial inherit unset revert])
  
(define-syntax (call-with-css-media stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:preferences ?preferences)) sexp ...)
     (with-syntax ([preferences (or (attribute ?preferences) #'(default-css-media-preferences))])
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
(define css-longhand : CSS-Longhand-Values (make-immutable-hasheq))
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

(define css-set! : (-> CSS-Values Symbol CSS-Datum Void)
  (lambda [declared-values property value]
    (hash-set! (css-values-descriptors declared-values) property (thunk value))))

(define in-css-values : (-> CSS-Values (Sequenceof Symbol (-> CSS-Datum)))
  (lambda [declared-values]
    (in-hash (css-values-descriptors declared-values))))

(define css-ref-raw : (-> (HashTable Symbol (-> CSS-Datum)) (Option CSS-Values) Symbol (Values CSS-Datum CSS-Datum))
  (lambda [properties inherited-values desc-name]
    (define declared-value : (-> CSS-Datum)
      (hash-ref properties desc-name
                (thunk (cond [(memq desc-name (default-css-all-exceptions)) (thunk css:unset)]
                             [else (hash-ref properties 'all (thunk (thunk css:unset)))]))))
    (define cascaded-value : CSS-Datum (declared-value))
    (define specified-value : CSS-Datum
      (cond [(not (css-wide-keyword? cascaded-value)) cascaded-value]
            [(or (eq? cascaded-value css:initial) (false? inherited-values)) css:initial]
            [else (let-values ([(_ sv) (css-ref-raw (css-values-descriptors inherited-values) #false desc-name)]) sv)]))
    (unless (eq? cascaded-value specified-value)
      (hash-set! properties desc-name (thunk specified-value)))
    (values cascaded-value specified-value)))

;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
(define css-viewport-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (define-css-disjoined-filter viewport-length-filter #:-> (U Symbol Nonnegative-Inexact-Real)
      (<css-keyword> 'auto)
      (<css:percentage> nonnegative-single-flonum?)
      (<css+length> #true))
    (define (make-size-parser [min-size : Symbol] [max-size : Symbol]) : (Pairof CSS-Shorthand-Parser (Listof Symbol))
      (cons (CSS<&> (CSS<^> (viewport-length-filter) min-size)
                    (CSS<$> (viewport-length-filter) max-size (λ [longhand] (hash-ref longhand min-size))))
            (list min-size max-size)))
    (case suitcased-name
      [(width) (make-size-parser 'min-width 'max-width)]
      [(height) (make-size-parser 'min-height 'max-height)]
      [(zoom min-zoom max-zoom) (CSS<^> (CSS:<+> (<css-keyword> 'auto) (CSS:<~> (<css+%real>) real->double-flonum)))]
      [(min-width max-width min-height max-height) (CSS<^> (viewport-length-filter))]
      [(orientation) (CSS<^> (<css-keyword> '(auto portrait landscape)))]
      [(user-zoom) (CSS<^> (<css-keyword> '(zoom fixed)))]
      [else #false])))

(define css-viewport-filter : (CSS-Cascaded-Value-Filter (HashTable Symbol CSS-Media-Datum))
  ;;; https://drafts.csswg.org/css-device-adapt/#constraining
  ;;; https://drafts.csswg.org/css-device-adapt/#handling-auto-zoom
  ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
  ;;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
  (lambda [cascaded-values initial-viewport inherited-viewport]
    ; Notes: There is no need to check the `initial-viewport` to specific the `specified values` since
    ;          @viewport is a controversial @rule which is easy to be used incorrectly,
    ;          @viewport is rarely used in desktop applications, and
    ;          this behavior is indeed specified if I understand the specification correctly.
    (define-values (initial-width initial-height)
      (let ([w (hash-ref initial-viewport 'width (const #false))]
            [h (hash-ref initial-viewport 'height (const #false))])
        (values (if (real? w) (flmax (real->double-flonum w) 1.0) 1.0)
                (if (real? h) (flmax (real->double-flonum h) 1.0) 1.0))))
    (define defzoom : Nonnegative-Flonum (default-css-viewport-auto-zoom))
    (define (smart [maix : (-> Flonum Flonum Flonum)] [v1 : (U Flonum Symbol)] [v2 : (U Flonum Symbol)]) : (U Flonum Symbol)
      (cond [(and (symbol? v1) (symbol? v2)) 'auto]
            [(symbol? v1) v2]
            [(symbol? v2) v1]
            [else (maix v1 v2)]))
    (define css->width (make-css->size #:100% initial-width #:= 'auto))
    (define css->height (make-css->size #:100% initial-height #:= 'auto))
    (define min-zoom : Flonum (css-ref cascaded-values #false 'min-zoom nonnegative-flonum? 0.0))
    (define max-zoom : Flonum (flmax min-zoom (css-ref cascaded-values #false 'max-zoom nonnegative-flonum? +inf.0)))
    (define min-width : (U Flonum Symbol) (css-ref cascaded-values #false 'min-width css->width))
    (define max-width : (U Flonum Symbol) (css-ref cascaded-values #false 'max-width css->width))
    (define min-height : (U Flonum Symbol) (css-ref cascaded-values #false 'min-height css->height))
    (define max-height : (U Flonum Symbol) (css-ref cascaded-values #false 'max-height css->height))
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

(define-values (default-css-viewport-parsers default-css-viewport-filter default-css-viewport-auto-zoom)
  (values (make-parameter css-viewport-parsers)
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

(define css-car : (-> (Listof CSS-Token) (Values CSS-Syntax-Any (Listof CSS-Token)))
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
          [else (cons item items)])))
