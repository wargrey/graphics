#lang at-exp racket/gui

(require net/http-client)
(require net/head)

(require pict)
(require plot)
(require racket/draw)
(require math/flonum)
(require images/flomap)

(provide (all-defined-out))

(define max-size 320)

(define recv-digimon
  {lambda [digimon]
    (with-handlers ([exn? {lambda [e]
                            (lt-superimpose (with-input-from-string (exn-message e) {thunk (foldr vl-append (blank 0 0)
                                                                                                  {let l->p ()
                                                                                                    (match (read-line)
                                                                                                      [{? eof-object?} null]
                                                                                                      [{var line} (cons (desc line max-size) (l->p))])})})
                                            (blank max-size max-size))}])
      (define-values {status headers pipe} (http-conn-sendrecv! (http-conn-open "wikimon.net") digimon #:close? #true #:content-decode '{gzip}))
      (if (regexp-match? #px"\\s200\\sOK" status)
        (bitmap->flomap (make-object bitmap% pipe))
        (error (bytes->string/utf-8 status))))})

(define digimon-visualize
  {lambda [digimon]
    (define get-pixel {lambda [x y] (let ([flv (flomap-ref* digimon x y)]) (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))})
    (plot3d-pict #:width 320 #:height 320
                 (list (contour-intervals3d {lambda [x y] (let-values ([{v who cares} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon))))})

(define digimon-ark!
  {lambda [digimon #:lightness [threshold 0.64] #:rallies [rallies0 null] #:show? [show? #false] #:bgcolor [bgcolor (make-object color% "Gray")]]
    (define-values {width0 height0} (flomap-size digimon))
    (define d-ark (new {class frame% (super-new [label "Digimon Analysis"] [min-width width0] [min-height height0])
                         (field [rallies null])
                         (field [vark (let ([arc (new canvas% [parent this] [paint-callback {lambda [who-cares painter] (send painter draw-bitmap (flomap->bitmap digimon) 0 0)}])])
                                        (send arc set-canvas-background bgcolor) arc)])
                         
                         (define/public {auto-erase}
                           (for-each {lambda [rally] (erase (car rally) (cdr rally) 0)} rallies0))
                          
                         (define/override {on-superwindow-show ?}
                           (unless ? (let ([~r4 (curry ~r #:precision '{= 4})])
                                       (for-each {lambda [sally]
                                                   (define-values {x y} (values (car sally) (cdr sally)))
                                                   (define flv (flomap-ref* digimon x y))
                                                   (define-values {h s v} (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))
                                                   (printf "XY(~a, ~a):: HSV(~a, ~a, ~a)~n" x y (~r4 h) (~r4 s) (~r4 v))}
                                                 rallies))))

                         (define/override (on-subwindow-event who mouse)
                           (define-values {x y} (values (send mouse get-x) (send mouse get-y)))
                           (when (and (equal? vark who) (null? rallies0))
                             (case (send mouse get-event-type)
                               [{left-up} {begin (set! rallies (cons (cons x y) rallies))
                                                 (printf "(cons ~a ~a)~n" x y)
                                                 (erase x y 0)
                                                 (send vark refresh-now)}])))
                         
                         (define/private {erase x y level}
                           (when (and (< -1 x width0) (< -1 y height0)
                                      (let ([flv (flomap-ref* digimon x y)])
                                        (and (> (flvector-ref flv 0) 0.0)
                                             (let-values ([{h s l} (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3))])
                                               (> l threshold)))))
                             (flvector-set! (flomap-values digimon) (* 4 (+ x (* y width0))) 0.0)
                             (when (and (send this is-shown?) (zero? (remainder level width0))) (send vark refresh-now))
                             (erase (sub1 x) y (add1 level))
                             (erase (add1 x) y (add1 level))
                             (erase x (sub1 y) (add1 level))
                             (erase x (add1 y) (add1 level))))}))
    (send d-ark show (if (null? rallies0) #true show?))
    (send d-ark center)
    (unless (null? rallies0)
      (send d-ark auto-erase)
      (send d-ark show #false))})

(define stretch
  {lambda [target #:width [width max-size]]
    (scale target (/ width (pict-width target)))})

(define shrink
  {lambda [target #:width [width max-size]]
    (define scale% (/ width (pict-width target)))
    (if (>= scale% 1.0) target (scale target scale%))})

(define attach-background
  {lambda [front #:color [bgcolor #false] #:xoff [xoff 0] #:yoff [yoff 0]]
    (cc-superimpose ((if bgcolor (compose (curryr colorize bgcolor) filled-rectangle) blank) (+ xoff (pict-width front) xoff) (+ yoff (pict-height front) yoff)) front)})

(define desc
  {lambda [txt width #:font-size [size 12] #:head [head (blank 0 0)] #:style [fstyle null]]
    (define {desc0 txt size width fstyle}
      (call-with-current-continuation
       {lambda [return]
         (when (or (< width size) (zero? (string-length txt))) (return (list (blank 0 0) txt)))
         (define hit (min (string-length txt) (exact-floor (/ width size))))
         (define hpict (text (substring txt 0 hit) fstyle size))
         (define final (desc0 (substring txt hit) size (- width (pict-width hpict)) fstyle))
         (list (hc-append hpict (first final)) (second final))}))
    (define smart (desc0 txt size width fstyle))
    (vl-append head (cond [(zero? (string-length (second smart))) (first smart)]
                          [else (desc (second smart) width #:font-size size #:head (first smart) #:style fstyle)]))})

(define rgb->hsv
  {lambda [r g b]
    (define-values {max0 min0} (values (max r g b) (min r g b)))
    (define chroma (- max0 min0))
    (values (* 60 (cond [(zero? chroma) 0]
                        [(and (= max0 r) (>= g b)) (/ (- g b) chroma)]
                        [(and (= max0 r) (< g b)) (+ (/ (- g b) chroma) 6)]
                        [(= max0 g) (+ (/ (- b r) chroma) 2)]
                        [(= max0 b) (+ (/ (- r g) chroma) 4)]))
            (cond [(zero? max0) 0]
                  [else (/ (- max0 min0) max0)])
            max0)})

(define rgb->hsl
  {lambda [r g b]
    (define-values {max0 min0} (values (max r g b) (min r g b)))
    (define-values {chroma lightness} (values (- max0 min0) (/ (+ max0 min0) 2)))
    (values (* 60 (cond [(zero? chroma) 0]
                        [(and (= max0 r) (>= g b)) (/ (- g b) chroma)]
                        [(and (= max0 r) (< g b)) (+ (/ (- g b) chroma) 6)]
                        [(= max0 g) (+ (/ (- b r) chroma) 2)]
                        [(= max0 b) (+ (/ (- r g) chroma) 4)]))
            (cond [(= max0 min0) 0]
                  [(<= lightness 1/2) (/ (- max0 min0) (+ max0 min0))]
                  [(> lightness 1/2) (/ (- max0 min0) (- 2 (+ max0 min0)))])
            lightness)})
