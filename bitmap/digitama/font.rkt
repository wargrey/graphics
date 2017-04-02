#lang typed/racket

(provide (all-defined-out))

(require "digicore.rkt")

(define generic-font-family-map : (->* (Symbol) ((Instance Font%)) Font-Family)
  (lambda [family [basefont (default-css-font)]]
    (case family
      [(swiss sans-serif)   'swiss]
      [(roman serif)        'roman]
      [(modern monospace)   'modern]
      [(decorative fantasy) 'decorative]
      [(script cursive)     'script]
      [(system system-ui)   'system]
      [(symbol math)        'symbol]
      [(default)            'default]
      [else (send basefont get-family)])))

(define generic-font-size-map : (->* (Symbol) ((Instance Font%)) Nonnegative-Real)
  (lambda [value [basefont (default-css-font)]]
    (define css-font-medium : Nonnegative-Flonum (smart-font-size (default-css-font)))
    (case value
      [(xx-large) (* 2/1 css-font-medium)]
      [(x-large)  (* 3/2 css-font-medium)]
      [(large)    (* 6/5 css-font-medium)]
      [(small)    (* 8/9 css-font-medium)]
      [(x-small)  (* 3/4 css-font-medium)]
      [(xx-small) (* 3/5 css-font-medium)]
      [(smaller)  (* 5/6 (smart-font-size basefont))] ; TODO: find a better function to deal with these two keywords.
      [(larger)   (* 6/5 (smart-font-size basefont))] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
      [else       css-font-medium])))

(define generic-font-weight-map : (->* ((U Symbol Integer)) ((Instance Font%)) Font-Weight)
  (lambda [value [basefont (default-css-font)]]
    (cond [(symbol? value)
           (case value
             [(normal light bold) value]
             [(bolder) (if (eq? (send basefont get-weight) 'light) 'normal 'bold)]
             [(lighter) (if (eq? (send basefont get-weight) 'bold) 'normal 'light)]
             [else (send basefont get-weight)])]
          [(<= value 300) 'light]
          [(>= value 700) 'bold]
          [else 'normal])))

(define generic-font-style-map : (->* (Symbol) ((Instance Font%)) Font-Style)
  (lambda [value [basefont (default-css-font)]]
    (case value
      [(normal italic) value]
      [(oblique slant) 'slant]
      [else (send basefont get-style)])))

(define system-ui : (-> Symbol String String)
  (let ([facebase ((inst make-hasheq Symbol String))])
    (lambda [symfont deface]
      (hash-ref! facebase symfont
                 (thunk (with-handlers ([exn? (λ _ deface)])
                          (let ([system-ui (dynamic-require 'racket/gui/base symfont)])
                            (or (and (font%? system-ui) (send system-ui get-face))
                                deface))))))))
