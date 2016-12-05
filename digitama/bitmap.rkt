#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out typed/racket/draw typed/images/logos typed/images/icons))
(provide (all-from-out racket/flonum racket/fixnum))

(require typed/racket/draw)
(require typed/images/logos)
(require typed/images/icons)

(require racket/flonum)
(require racket/fixnum)

(require digimon/cheat)

(define-type Color (Instance Color%))
(define-type Color+sRGB (U Index Symbol String Color))
(define-type Bitmap (Instance Bitmap%))
(define-type Font (Instance Font%))

(define-cheat-opaque color%? #:is-a? Color% color%)
(define-cheat-opaque bitmap%? #:is-a? Bitmap% bitmap%)
(define-cheat-opaque font%? #:is-a? Font% font%)

(define-predicate racket-font-smoothing? Font-Smoothing)
(define-predicate racket-font-hinting? Font-Hinting)

(define the-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
(define the-invalid-image : Bitmap (read-bitmap (open-input-bytes #"placeholder")))

(define default-css-font : (Parameterof Font) (make-parameter (make-font)))
(define default-css-invalid-image : (Parameterof Bitmap) (make-parameter (x-icon)))

(define require-image : (-> String Symbol Positive-Real Any)
  (lambda [src.rkt id density]
    (define fallback (thunk (call-with-values (thunk (eval id (module->namespace src.rkt))) (λ _ (car _)))))
    (module-declared? src.rkt #true)
    (define value (dynamic-require src.rkt id fallback))
    (cond [(not (hash? value)) value]
          [else (hash-ref value (exact->inexact density)
                          (thunk (let ([all (sort (hash-keys value) >)])
                                   (if (pair? all) (hash-ref value (car all)) value))))])))

(define smart-font-size : (-> Font Nonnegative-Flonum)
  (let ([macosx? (eq? (system-type 'os) 'macosx)])
    (lambda [font]
      (define size : Nonnegative-Flonum (real->double-flonum (send font get-size)))
      (case (if (or macosx? (send font get-size-in-pixels)) 'px 'pt)
        [(pt) (fl* size (fl/ 96.0 72.0))]
        [else size]))))
