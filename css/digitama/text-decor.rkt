#lang typed/racket/base

;;; https://drafts.csswg.org/css-text-decor

(provide (all-defined-out))

(require "syntax/digicore.rkt")
(require "../recognizer.rkt")
(require "color.rkt")

(define css-text-decor-line-options : (Listof Symbol) '(underline overline line-through blink))
(define css-text-decor-skip-options : (Listof Symbol) '(objects spaces ink edges box-decoration))

(define css-text-decor-style-option : (Listof Symbol) '(solid double dotted dashed wavy))

(define css-fold-decoration-line : CSS-Longhand-Update
  (lambda [_ old-options property]
    (if (list? old-options) (cons property old-options) (list property))))
  
(define <:text-decoration:> : CSS-Shorthand+Parser
  ;;; https://drafts.csswg.org/css-text-decor/#text-decoration-color-property
  (cons (CSS<*> (CSS<+> (CSS:<^> (<css-keyword> 'none) 'text-decoration-line)
                        (CSS:<^> (<css-keyword> css-text-decor-line-options) 'text-decoration-line css-fold-decoration-line)
                        (CSS:<^> (<css-keyword> css-text-decor-style-option) 'text-decoration-style)
                        (CSS:<^> (<css-color>) 'text-decoration-color)))
        '(text-decoration-line text-decoration-color text-decoration-style)))

(define css->text-decor-lines : (CSS->Racket (Listof Symbol))
  (lambda [[_ : Symbol] [value : Any]]
    (cond [(list? value) (filter symbol? value)]
          [else null])))
