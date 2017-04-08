#lang typed/racket

(provide (all-defined-out))

;;; https://drafts.csswg.org/css-values/#absolute-lengths
;;; https://drafts.csswg.org/css-values/#relative-lengths

(require "digicore.rkt")
(require "misc.rkt")

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-dimensional-tokens stx)
  (syntax-case stx []
    [(_ parent
        ([id #:+ ID #:=> canonical-unit [conversion ...]] ...)
        ([ctoken #:+ CToken #:-> cparent] ...))
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
                (struct: ctoken : CToken cparent ()) ...

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

                (define +id? : (-> Any Boolean : #:+ (U ID CSS-Zero))
                  (lambda [token]
                    (or (css-zero? token)
                        (and (id? token)
                             (fl>= (css:dimension-datum token) 0.0)))))
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

(define-dimensional-tokens css:dimension
  ([css:length         #:+ CSS:Length          #:=> px
                       [[(cm)    (fl* px (fl/ 96.0 2.54))]
                        [(mm)    (fl* px (fl/ 96.0 25.4))]  ; 1cm/10
                        [(q)     (fl* px (fl/ 96.0 101.6))] ; 1cm/40
                        [(in)    (fl* px 96.0)]
                        [(pc)    (fl* px 16.0)]             ; 1in/6
                        [(pt)    (fl* px (fl/ 96.0 72.0))]  ; 1in/72
                        [(em)    (fl* px (css-em))]
                        [(ex)    (fl* px (css-ex))]
                        [(cap)   (fl* px (css-cap))]
                        [(ch)    (fl* px (css-ch))]
                        [(ic)    (fl* px (css-ic))]
                        [(lh)    (fl* px (css-lh))]
                        [(rem)   (fl* px (css-rem))]
                        [(rlh)   (fl* px (css-rlh))]
                        [(vw vi) (fl* px (fl* 0.01 (css-vw)))]
                        [(vh vb) (fl* px (fl* 0.01 (css-vh)))]
                        [(vmin)  (fl* px (fl* 0.01 (min (css-vw) (css-vh))))]
                        [(vmax)  (fl* px (fl* 0.01 (max (css-vw) (css-vh))))]]]
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
  ([css:length:font     #:+ CSS:Length:Font     #:-> css:length]
   [css:length:viewport #:+ CSS:Length:Viewport #:-> css:length]))
