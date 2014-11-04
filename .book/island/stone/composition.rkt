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
  {lambda [digimon0]
    (define digimon (flomap-flip-vertical digimon0))
    (define get-pixel {lambda [x y] (let ([flv (flomap-ref* digimon x y)]) (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))})
    (plot3d-pict (list (contour-intervals3d {lambda [x y] (let-values ([{v who cares} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.16))
                       (contour-intervals3d {lambda [x y] (let-values ([{who v cares} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.08))
                       (contour-intervals3d {lambda [x y] (let-values ([{who cares v} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.04))))})

(define digimon-ark
  {lambda [digimon0 #:lightness [threshold 0.64] #:rallies [rallies0 null] #:show? [show? #true] #:bgcolor [bgcolor (make-object color% "Gray")]]
    (define-values {width0 height0} (flomap-size digimon0))
    (define digimon (flomap-copy digimon0 0 0 width0 height0))
    (define d-ark (new {class frame% (super-new [label "Digimon Analyzer"] [min-width width0] [min-height height0])
                         (field [rallies null])
                         (field [vark (new canvas% [parent this] [paint-callback {lambda [who-cares painter] (send painter draw-bitmap (flomap->bitmap digimon) 0 0)}])])
                         (field [dark (let ([arc (send vark get-dc)])
                                        (send vark set-canvas-background bgcolor)
                                        (send arc set-pen (make-pen #:color bgcolor))
                                        arc)])
                         
                         (define/public {auto-erase}
                           (for-each {lambda [rally] (erase (car rally) (cdr rally))} (cons (cons 0 0) rallies0)))
                          
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
                                                 (erase x y)}])))
                         
                         (define/private {erase x y}
                           (define xy (* 4 (+ x (* y width0))))
                           (when (and (< -1 x width0) (< -1 y height0)
                                      (> (flvector-ref (flomap-values digimon) xy) 0.0)
                                      (let-values ([{h s l} (rgb->hsv (flvector-ref (flomap-values digimon) (+ 1 xy))
                                                                      (flvector-ref (flomap-values digimon) (+ 2 xy))
                                                                      (flvector-ref (flomap-values digimon) (+ 3 xy)))])
                                        (> l threshold)))
                             (flvector-set! (flomap-values digimon) xy 0.0)
                             (send dark draw-point x y)
                             (erase (sub1 x) y)
                             (erase (add1 x) y)
                             (erase x (sub1 y))
                             (erase x (add1 y))))}))
    (send d-ark show (if (null? rallies0) #true show?))
    (send d-ark center)
    (unless (null? rallies0)
      (send d-ark auto-erase)
      (sleep 1)
      (send d-ark show #false)
      (bitmap (flomap->bitmap (flomap-trim digimon #true))))})

(define info
  {lambda [items #:font-size [size 12] #:style [fstyle null]]
    (define seps (exact-round (* size 1/2)))
    (define rows (map {lambda [item] (cons (text (format "~a: " (car item)) fstyle size) (text (symbol->string (cdr item)) fstyle size))} items))
    (define col (let ([item (argmax (compose1 pict-width car) rows)]) (blank (+ (pict-width (car item)) seps) (+ (pict-height (car item)) seps))))
    (table 2 (flatten (map {lambda [row] (list (rc-superimpose col (car row)) (lc-superimpose col (cdr row)))} rows))
           cc-superimpose cc-superimpose seps 0)})

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

(define item
  {lambda [items #:font-size [size 12] #:style [fstyle null]]
    (for/fold ([skills (text "Skills:" fstyle size)]) ([skill (in-list items)])
      (vl-append skills (hc-append (arrowhead (exact-round (* size 2/3)) 0) (blank (exact-round (* size 1/2)) 0)
                                   (text (string-join (map symbol->string skill)) fstyle size))))})

(define rgb->hsv
  {lambda [r g b]
    (define-values {max0 min0} (values (max r g b) (min r g b)))
    (define chroma (- max0 min0))
    (values chroma
            (cond [(zero? max0) 0]
                  [else (/ chroma max0)])
            max0)})

(define ~desc
  {lambda [src]
    (define/match (~str chars capital? literal?)
      [{(? null?) _ _} null]
      [{(list #\space tail ...) _ _} (cons #\space (~str tail capital? literal?))]
      [{(list (or #\! #\? #\.) tail ...) _ _} (cons (car chars) (~str tail #true literal?))]
      [{(list #\" tail ...) _ _} (~str tail capital? (not literal?))]
      [{(list head tail ...) _ #true} (cons head (~str tail #false #true))]
      [{(list head tail ...) #true _} (cons (char-upcase head) (~str tail #false literal?))]
      [{(list head tail ...) _ _} (cons (char-downcase head) (~str tail #false #false))])
    (list->string (~str (string->list (string-trim (format "~s" src) @pregexp{[()]})) #true #false))})
