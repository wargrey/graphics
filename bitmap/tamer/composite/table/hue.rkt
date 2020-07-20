#lang typed/racket

(provide color-hues hue-colors hue-colors*)

(require bitmap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define color-hues : (Listof (Pairof Real Any))
  (list (cons 0.0 'Reds)
        (cons 30.0 'Oranges)
        (cons 60.0 'Yellows)
        (cons 90.0 'Yellow-Greens)
        (cons 120.0 'Greens)
        (cons 150.0 'Green-Cyans)
        (cons 180.0 'Cyans)
        (cons 210.0 'Cyan-Blues)
        (cons 240.0 'Blues)
        (cons 270.0 'Blue-Magentas)
        (cons 300.0 'Magentas)
        (cons 330.0 'Magenta-Reds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define placeholder : Bitmap (bitmap-blank))

(define bitmap-text% : (-> Real Font Bitmap)
  (lambda [% font]
    (bitmap-text (~a (inexact->exact (round (* % 100.0))) #\%) font)))

(define hue-colors : (-> Any
                         (->* (Real Real Real) (Real) FlColor) Real (Listof Real) (Listof Real)
                         [#:font Font] [#:rotate? Boolean]
                         [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                         Bitmap)
  (lambda [label hsb hue hs% vs%
                 #:font [font (default-font)] #:rotate? [rotate? #false]
                 #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (define &legend : (Boxof (Option Bitmap)) (box #false))
    (define title : Bitmap (bitmap-text (~a (inexact->exact (round hue)) #\° #\space label) font))
    
    (define cblocks : (Listof (Listof Bitmap))
      (for/list ([v% (in-list vs%)])
        (cons (bitmap-text% v% font)
              (for/list : (Listof Bitmap) ([h% (in-list hs%)])
                (define flc : FlColor (if rotate? (hsb hue v% h%) (hsb hue h% v%)))

                (unless (unbox &legend)
                  (set-box! &legend
                            (if (hwba? flc)
                                (bitmap-text "W\\B")
                                (bitmap-text "B\\S"))))
                
                (bitmap-rectangle cwidth cheight #:border flc #:fill flc)))))

    (define header : (Listof Bitmap)
      (cons (or (unbox &legend) (bitmap-blank))
            (map (λ [[% : Real]] (bitmap-text% % font)) hs%)))

    (bitmap-vc-append title
                      (bitmap-table* (list* header cblocks)
                                     '(cc) '(cc)
                                     gapsize gapsize))))

(define hue-colors* : (-> (->* (Real Real Real) (Real) FlColor)
                          (Listof (Pairof Real Any)) (Listof Real) (Listof Real)
                          [#:font Font] [#:cols Byte] [#:rotate? Boolean]
                          [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                          Bitmap)
  (lambda [hsb hue.labels hs% vs%
               #:cols [cols 3] #:font [font (default-font)] #:rotate? [rotate? #false]
               #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (bitmap-table cols
                  (map (λ [[hue.label : (Pairof Real Any)]]
                         (hue-colors (cdr hue.label)
                                     hsb (car hue.label) hs% vs%
                                     #:font font #:rotate? rotate?
                                     #:cell-width cwidth #:cell-height cheight #:gapsize gapsize))
                       hue.labels)
                  '(cc) '(cc)
                  (+ cwidth gapsize gapsize)
                  (+ cheight gapsize gapsize))))

(module+ main
  (define font : Font (desc-font #:weight 'bold))
  (define hsb-hs% : (Listof Real) (list 1.0 0.75 0.50 0.25 0.00))
  (define hsb-vs% : (Listof Real) (list 1.0 0.88 0.75 0.63 0.50 0.38 0.25 0.13 0.00))
  (define hwb-s% : (Listof Real) (list 0.0 0.20 0.40 0.60 0.80 1.00))

  (hue-colors* hsl color-hues hsb-hs% hsb-vs%)
  (hue-colors* hsv color-hues hsb-hs% hsb-vs%)
  (hue-colors* hsi color-hues hsb-hs% hsb-vs%)
  (hue-colors* hwb color-hues hwb-s% hwb-s% #:rotate? #true))
