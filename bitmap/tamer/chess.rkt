#lang typed/racket

(require bitmap)
(require geofun/digitama/font)
(require geofun/constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-border (desc-stroke long-dash #:color 'gray #:width 1))

(define chesses : String "♔♕♖♗♘♙♚♛♜♝♞♟︎")

(define font-okay? : (-> String Font Boolean)
  (lambda [text font]
    (and (text-descender-exist? text font)
         (text-glyphs-exist? text font))))

(define bitmap-text* : (-> String Font Bitmap)
  (lambda [text font]
    (define large-font (desc-font (desc-font font #:size 'xx-large) #:size -4.0))

    (bitmap-vl-append (bitmap-text (font-face font) large-font)
                      (bitmap-frame (bitmap-text #:meanline green #:baseline red #:descent blue 
                                                 text large-font)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list : (Listof Bitmap)
    ([font (in-list (map (λ [[family : String]] : Font (desc-font #:family family)) (list-font-families)))]
     #:when (font-okay? chesses font))
    (bitmap-text* chesses font)))
