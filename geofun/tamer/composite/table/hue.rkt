#lang typed/racket/base

(provide contrast-color-hues contrast-hue-colors contrast-hue-colors*)
(provide color-hues hue-colors hue-colors*)

(require racket/format)
(require racket/math)
(require racket/list)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define contrast-color-hues : (Listof (List Real Any Any))
  (list (list 180.0 'Cyans 'Reds)
        (list 210.0 'Cyan-Blues 'Oranges)
        (list 240.0 'Blues 'Yellows)
        (list 270.0 'Blue-Magentas 'Yellow-Greens)
        (list 300.0 'Magentas 'Greens)
        (list 330.0 'Magenta-Reds 'Green-Cyans)))

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
(define placeholder : Geo (geo-blank))

(define hue-text% : (-> Real Font Geo:Text)
  (lambda [% font]
    (geo-text (~a (exact-round (* % 100.0)) #\%) font)))

(define hue-colors : (-> Any
                         (->* (Real Real Real) (Real) FlColor) Real (Listof Real) (Listof Real)
                         [#:font Font] [#:rotate? Boolean]
                         [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                         Geo)
  (lambda [label hsb hue hs% vs%
                 #:font [font (default-font)] #:rotate? [rotate? #false]
                 #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (define &legend : (Boxof (Option Geo)) (box #false))
    (define title : Geo (geo-text (~a #\space #\space #\space #\space #\space #\space (exact-round hue) #\° #\space label) font))
    
    (define cblocks : (Listof (Listof Geo))
      (for/list ([v% (in-list vs%)])
        (cons (hue-text% v% font)
              (for/list : (Listof Geo) ([h% (in-list hs%)])
                (define flc : FlColor (if rotate? (hsb hue v% h%) (hsb hue h% v%)))

                (unless (unbox &legend)
                  (set-box! &legend
                            (cond [(hsla? flc) (geo-text "L\\S")]
                                  [(hsva? flc) (geo-text "V\\S")]
                                  [(hsia? flc) (geo-text "I\\S")]
                                  [else (geo-text "W\\B")])))
                
                (geo-rectangle cwidth cheight #:stroke flc #:fill flc)))))

    (define header : (Listof Geo)
      (cons (or (unbox &legend) (geo-blank))
            (map (λ [[% : Real]] (hue-text% % font)) hs%)))

    (geo-vc-append title
                   (geo-table* (list* header cblocks)
                               '(cc) '(cc)
                               gapsize gapsize))))

(define contrast-hue-colors : (-> Any Any
                                  (->* (Real Real Real) (Real) FlColor) Real (Listof Real) (Listof Real)
                                  [#:font Font] [#:rotate? Boolean]
                                  [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                                  Geo)
  (lambda [label constrast-label hsb hue half-hs% vs%
                 #:font [font (default-font)] #:rotate? [rotate? #false]
                 #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (define &legend : (Boxof (Option Geo)) (box #false))
    (define contrast-hue : Real (- hue 180.0))
    (define title : Geo (geo-text (format "    ~a° ~a     ~a ~a°" (exact-round hue) label constrast-label (exact-round contrast-hue)) font))
    (define hs% : (Listof Real) (append half-hs% (cdr (reverse half-hs%))))
    (define hues : (Listof Real) (append (make-list (sub1 (length half-hs%)) hue) (make-list (length half-hs%) contrast-hue)))
    
    (define cblocks : (Listof (Listof Geo))
      (for/list ([v% (in-list vs%)])
        (cons (hue-text% v% font)
              (for/list : (Listof Geo) ([h% (in-list hs%)]
                                        [h (in-list hues)])
                (define flc : FlColor (if rotate? (hsb h v% h%) (hsb h h% v%)))

                (unless (unbox &legend)
                  (set-box! &legend
                            (cond [(hsla? flc) (geo-text "L\\S")]
                                  [(hsva? flc) (geo-text "V\\S")]
                                  [(hsia? flc) (geo-text "I\\S")]
                                  [else (geo-blank)])))
                
                (geo-rectangle cwidth cheight #:stroke flc #:fill flc)))))

    (define header : (Listof Geo)
      (cons (or (unbox &legend) (geo-blank))
            (map (λ [[% : Real]] (hue-text% % font)) hs%)))

    (geo-vc-append title
                   (geo-table* (list* header cblocks)
                               '(cc) '(cc)
                               gapsize gapsize))))

(define hue-colors* : (-> (->* (Real Real Real) (Real) FlColor)
                          (Listof (Pairof Real Any)) (Listof Real) (Listof Real)
                          [#:font Font] [#:cols Byte] [#:rotate? Boolean]
                          [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                          Geo)
  (lambda [hsb hue.labels hs% vs%
               #:cols [cols 3] #:font [font (default-font)] #:rotate? [rotate? #false]
               #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (geo-table cols
               (map (λ [[hue.label : (Pairof Real Any)]]
                      (hue-colors (cdr hue.label)
                                  hsb (car hue.label) hs% vs%
                                  #:font font #:rotate? rotate?
                                  #:cell-width cwidth #:cell-height cheight #:gapsize gapsize))
                    hue.labels)
               '(cc) '(cc)
               (+ cwidth gapsize gapsize)
               (+ cheight gapsize gapsize))))

(define contrast-hue-colors* : (-> (->* (Real Real Real) (Real) FlColor)
                                   (Listof (List Real Any Any)) (Listof Real) (Listof Real)
                                   [#:font Font] [#:cols Byte] [#:rotate? Boolean]
                                   [#:cell-width Nonnegative-Real] [#:cell-height Nonnegative-Real] [#:gapsize Nonnegative-Real]
                                   Geo)
  (lambda [hsb hue.labels half-hs% vs%
               #:cols [cols 2] #:font [font (default-font)] #:rotate? [rotate? #false]
               #:cell-width [cwidth 35.0] #:cell-height [cheight 21.0] #:gapsize [gapsize 2.0]]
    (geo-table cols
               (map (λ [[hue.label : (List Real Any Any)]]
                      (contrast-hue-colors (cadr hue.label) (caddr hue.label)
                                           hsb (car hue.label) half-hs% vs%
                                           #:font font #:rotate? rotate?
                                           #:cell-width cwidth #:cell-height cheight #:gapsize gapsize))
                    hue.labels)
               '(cc) '(cc)
               (+ cwidth gapsize gapsize)
               (+ cheight gapsize gapsize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define font : Font (desc-font #:weight 'bold))
  (define hsb-hs% : (Listof Real) (list 1.0 0.75 0.50 0.25 0.00))
  (define hsb-vs% : (Listof Real) (list 1.0 0.88 0.75 0.63 0.50 0.38 0.25 0.13 0.00))
  (define hwb-s% : (Listof Real) (list 0.00 0.20 0.40 0.60 0.80 1.00))

  (contrast-hue-colors* hsl contrast-color-hues hsb-hs% hsb-vs%)
  (contrast-hue-colors* hsv contrast-color-hues hsb-hs% hsb-vs%)
  (contrast-hue-colors* hsi contrast-color-hues hsb-hs% hsb-vs%)
  (contrast-hue-colors* hwb contrast-color-hues hwb-s% hwb-s% #:rotate? #true))
