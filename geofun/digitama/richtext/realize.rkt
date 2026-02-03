#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/string)
(require racket/symbol)

(require "self.rkt")
(require "markup.rkt")

(require "../../font.rkt")

(require "../self.rkt")
(require "../paint/self.rkt")
(require "../dc/text.rkt")
(require "../dc/markup.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rich-numerical-text-realize
  (lambda [#:id [text-id : (Option Symbol) #false]
           #:background [bg-paint : Option-Fill-Paint #false]
           #:alignment [alignment : Geo-Text-Alignment 'center]
           #:precision [precision : (U Integer (List '= Integer)) 3]
           #:radix [base : (U (List 'up Integer) Integer) 10]
           [n : (U Complex Rich-Datum<%>)]
           [font : (Option Font)]
           [color : Option-Fill-Paint]] : Geo
    (define (geo-plain-string [v : String]) : Geo
      (geo-text #:id text-id #:alignment alignment
                #:color color #:background bg-paint
                n font))

    (define (geo-primitive-number [v : Complex]) : Geo
      (cond [(exact? v) (geo-plain-string (number->string v))]
            [(integer? v) (geo-plain-string (number->string (inexact->exact v)))]
            ;[(>= precision 0) (geo-plain-string (~r text #:precision precision))]
            [else (geo-plain-string (number->string v))]))
    
    (cond [(complex? n) (geo-primitive-number n)]
          [else (geo-plain-string (~a n))])))

(define geo-rich-text-realize
  (lambda [#:id [text-id : (Option Symbol) #false]
           #:background [bg-paint : Option-Fill-Paint #false]
           #:alignment [alignment : Geo-Text-Alignment 'center]
           #:precision [precision : (U Integer (List '= Integer)) 3]
           #:max-width [max-width : (Option Nonnegative-Flonum) #false]
           #:max-height [max-height : (Option Nonnegative-Flonum) #false]
           #:radix [base : (U (List 'up Integer) Integer) 10]
           [text : Geo-Rich-Text]
           [font : (Option Font)]
           [color : Option-Fill-Paint]] : Geo
    (cond [(or (string? text) (symbol? text))
           (if (or max-width max-height)
               (geo-paragraph #:id text-id #:alignment alignment
                              #:color color #:background bg-paint
                              #:max-width  (or  max-width +inf.0)
                              #:max-height (or max-height +inf.0) 
                              text font)
               (geo-text #:id text-id #:alignment alignment
                         #:color color #:background bg-paint
                         text font))]
          [(geo-markup-datum? text) ; TODO: deal with size limits
           (geo-markup #:id text-id #:alignment alignment
                       #:color color #:background bg-paint
                       #:error-color 'GhostWhite #:error-background 'Firebrick
                       text font)]
          [(geo? text) #;#:|TODO: deal with size limits| text]
          [else (geo-rich-numerical-text-realize #:id text-id
                                                 #:background bg-paint #:alignment alignment
                                                 #:precision precision #:radix base
                                                 text font color)])))

(define #:forall (T) geo-rich-text-realize*
  (lambda [#:id [text-id : (Option Symbol) #false]
           #:background [bg-paint : Option-Fill-Paint #false]
           #:alignment [alignment : Geo-Text-Alignment 'center]
           #:precision [precision : (U Integer (List '= Integer)) 3]
           #:max-width [max-width : (Option Nonnegative-Flonum) #false]
           #:max-height [max-height : (Option Nonnegative-Flonum) #false]
           #:radix [base : (U (List 'up Integer) Integer) 10]
           [text : (U T Geo-Rich-Text)]
           [font : (Option Font)]
           [color : Option-Fill-Paint]] : (U T Geo)
    (if (geo-rich-text? text)
        (geo-rich-text-realize #:id text-id
                               #:background bg-paint #:alignment alignment
                               #:precision precision #:radix base
                               #:max-width max-width #:max-height max-height
                               text font color)
        text)))
