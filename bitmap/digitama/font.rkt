#lang typed/racket

(provide (all-defined-out))

(require "digicore.rkt")

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(define generic-font-family-map : (-> Symbol (Instance Font%) Font-Family)
  (lambda [family basefont]
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

(define select-font-face : (-> (Listof (U String Symbol)) (-> Symbol String) (Option String))
  (lambda [value family->face]
    (let select ([families value])
      (and (pair? families)
           (let ([family (car families)])
             (or (and (symbol? family) (family->face family))
                 (and (string? family) (face-filter family))
                 (select (cdr families))))))))

(define generic-font-size-map : (-> Symbol (Instance Font%) (Instance Font%) Nonnegative-Real)
  (lambda [size basefont rootfont]
    (define css-font-medium : Nonnegative-Flonum (smart-font-size rootfont))
    (case size
      [(xx-large) (* 2/1 css-font-medium)]
      [(x-large)  (* 3/2 css-font-medium)]
      [(large)    (* 6/5 css-font-medium)]
      [(small)    (* 8/9 css-font-medium)]
      [(x-small)  (* 3/4 css-font-medium)]
      [(xx-small) (* 3/5 css-font-medium)]
      [(smaller)  (* 5/6 (smart-font-size basefont))] ; TODO: find a better function to deal with these two keywords.
      [(larger)   (* 6/5 (smart-font-size basefont))] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
      [else       css-font-medium])))

(define generic-font-weight-map : (-> (U Symbol Integer) (Instance Font%) Font-Weight)
  (lambda [weight basefont]
    (cond [(symbol? weight)
           (case weight
             [(normal light bold) weight]
             [(bolder) (if (eq? (send basefont get-weight) 'light) 'normal 'bold)]
             [(lighter) (if (eq? (send basefont get-weight) 'bold) 'normal 'light)]
             [else (send basefont get-weight)])]
          [(<= weight 300) 'light]
          [(>= weight 700) 'bold]
          [else 'normal])))

(define generic-font-style-map : (-> Symbol (Instance Font%) Font-Style)
  (lambda [style basefont]
    (case style
      [(normal italic) style]
      [(oblique slant) 'slant]
      [else (send basefont get-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define face-filter : (-> String (Option String))
  (let ([&faces : (Boxof (Option (HashTable String String))) (box #false)])
    (lambda [face]
      (define the-face-set : (HashTable String String)
        (or (unbox &faces)
            (let ([the-set (for/hash : (HashTable String String) ([face (in-list (get-face-list))])
                             (values (string-downcase face) face))])
              (set-box! &faces the-set)
              the-set)))
      (hash-ref the-face-set (string-downcase face) (thunk #false)))))

(define system-ui : (-> Symbol String String)
  (let ([facebase ((inst make-hasheq Symbol String))])
    (lambda [symfont deface]
      (hash-ref! facebase symfont
                 (thunk (with-handlers ([exn? (λ _ deface)])
                          (let ([system-ui (dynamic-require 'racket/gui/base symfont)])
                            (or (and (font%? system-ui) (send system-ui get-face))
                                deface))))))))
