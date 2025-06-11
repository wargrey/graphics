#lang typed/racket/base

(require racket/list)
(require racket/math)

(require colorspace/cie)
(require colorspace/ok)

(require "hsb.rkt")
(require "../misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-rgb<->cie : (-> CIE<->RGB CIE<->RGB Symbol Void)
  (lambda [rgb->cie cie->rgb name]
    (for ([tc (in-list examples)])
      (define color-name  (color->name (first tc)))
      (define-values (R G B) (values (second tc) (third tc) (fourth tc)))
      (define-values (L x y) (rgb->cie R G B))
      (define-values (r g b) (cie->rgb L x y))
      (define src : (Listof Flonum) (list R G B))
      (define cie : (Listof Flonum) (list r g b))

      (if (andmap datum=? src cie)
          (displayln (list (cons name color-name) R G B) (current-output-port))
          (displayln (cons (cons name color-name) (map (inst cons Flonum Flonum) src cie)) (current-error-port))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (test-rgb<->cie rgb->lab lab->rgb 'Lab)
  (test-rgb<->cie rgb->lch lch->rgb 'LCh)
  (test-rgb<->cie rgb->oklab oklab->rgb 'OkLab)
  (test-rgb<->cie rgb->oklch oklch->rgb 'OkLCh))
