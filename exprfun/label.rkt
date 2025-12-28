#lang typed/racket/base

(provide (all-defined-out) Geo-Text-Alignment)

(require "../font.rkt")

(require "self.rkt")
(require "color.rkt")
(require "markup.rkt")

(require "dc/text.rkt")
(require "paint/self.rkt")
(require "unsafe/dc/text-layout.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Label-Text DC-Markup-Text)
(define-type Option-Label-Text (Option Label-Text))
(define-type Label-Maybe-Text (U Option-Label-Text Void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-label-brief? : (-> Any Boolean : Label-Text)
  (lambda [v]
    (dc-markup-text? v)))

(define geo-label-option-brief? : (-> Any Boolean : Label-Option-Text)
  (lambda [v]
    (or (dc-markup-text? v)
        (not v))))

(define geo-label-maybe-brief? : (-> Any Boolean : Label-Maybe-Text)
  (lambda [v]
    (or (geo-label-option-brief? v)
        (void? v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO: transparent color makes the label clear underneath arrows but sometimes doesn't work for pdf
(define geo-label-brief
  (lambda [#:id [text-id : (Option Symbol) #false] #:alignment [align : Geo-Text-Alignment 'center]
           [text : Geo-Label-Text] [font : (Option Font)] [paint : Option-Fill-Paint]] : Geo
    (geo-markup #:id text-id #:alignment align
                #:color paint #:background transparent
                #:error-color 'GhostWhite #:error-background 'Firebrick
                text font)))
