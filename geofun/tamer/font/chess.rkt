#lang typed/racket

(require geofun/vector)
(require geofun/digitama/font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define chesses : String "♔♕♖♗♘♙♚♛♜♝♞♟︎")

(define font-okay? : (-> String Font Boolean)
  (lambda [text font]
    (and (text-descender-exist? text font)
         (text-glyphs-exist? text font))))

(define geo-text* : (-> String Font Geo)
  (lambda [text font]
    (define large-font (desc-font (desc-font font #:size 'xx-large) #:size -4.0))

    (geo-vl-append (geo-text (font-face font) large-font)
                   (geo-frame (geo-text #:meanline 'green #:baseline 'red #:descent 'blue 
                                        text large-font)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (default-border (desc-stroke #:color 'gray #:width 1 #:dash 'long-dash))
  
  (for/list : (Listof Geo)
    ([font (in-list (map (λ [[family : String]] : Font (desc-font #:family family)) (list-font-families)))]
     #:when (font-okay? chesses font))
    (geo-text* chesses font)))
