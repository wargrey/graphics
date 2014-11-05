#lang at-exp racket/gui

(require net/http-client)
(require net/head)

(require pict)
(require plot)
(require racket/draw)

(require math/flonum)
(require images/flomap)
(require images/icons/symbol)

(provide (all-defined-out))

(define wikimon.net (http-conn-open "wikimon.net"))

(define recv-digimon
  {lambda [digimon]
    (with-handlers ([exn? {lambda [e]
                            (lt-superimpose (make-flomap 4 (plot-width) (exact-round (/ (plot-width) 0.618)))
                                            (for/fold ([flmp #false]) ([line (in-list (with-input-from-string (exn-message e) {thunk (port->lines)}))])
                                              (define fline (desc line (plot-width) #:color "Red"))
                                              (if flmp (vl-append flmp fline) fline)))}])
      (define-values {status headers pipe} (http-conn-sendrecv! wikimon.net digimon #:close? #true #:content-decode '{gzip}))
      (if (regexp-match? #px"\\s200\\sOK" status)
        (bitmap->flomap (make-object bitmap% pipe))
        (error (bytes->string/utf-8 status))))})

(define digimon-visualize
  {lambda [digimon0]
    (define digimon (flomap-flip-vertical digimon0))
    (define get-pixel {lambda [x y] (let ([flv (flomap-ref* digimon x y)]) (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))})
    (plot3d-pict #:height (exact-round (/ (plot-width) 0.618))
                 (list (contour-intervals3d {lambda [x y] (let-values ([{v who cares} (get-pixel x y)]) (* v 1.618))}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.04))
                       (contour-intervals3d {lambda [x y] (let-values ([{who v cares} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.08))
                       (contour-intervals3d {lambda [x y] (let-values ([{who cares v} (get-pixel x y)]) v)}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:alphas (make-list 5 0.08))))})

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
    (define rows (map {lambda [item] (cons (car item) (text (symbol->string (cdr item)) fstyle size))} items))
    (define col (let ([item (argmax (compose1 pict-width cdr) rows)]) (blank (+ (pict-width (cdr item)) seps) (+ (pict-height (cdr item)) seps))))
    (vl-append (cdar rows) (table (sub1 (length items)) (map {lambda [row] (lc-superimpose col (cdr row))} (cdr rows)) cc-superimpose cc-superimpose seps 0))})

(define desc
  {lambda [content width #:font [font (make-font)] #:color [color "white"] #:head [head0 #false]]
    (define seps (if (list? content) (send (text-icon " " font #:color color #:trim? #false) get-width) 0))
    (define smart (let desc0 ([words content] [room width])
                    (with-handlers ([exn:fail:contract? (const (list (blank 0 0) words))])
                      (define ptxt (bitmap (text-icon (~a (sequence-ref words 0)) font #:color color #:trim? #false)))
                      (if (< room (pict-width ptxt))
                          (raise-range-error 'desc "words" "ending " (pict-width ptxt) words 0 room width)
                          (let ([final (desc0 (sequence-tail words 1) (- room (pict-width ptxt)))])
                            (list (hc-append seps ptxt (first final)) (second final)))))))
    (define headn (cond [(zero? (sequence-length (second smart))) (first smart)]
                        [else (desc (second smart) width #:font font #:color color #:head (first smart))]))
    (if head0 (vl-append head0 headn) headn)})

(define item
  {lambda [items #:font-size [size 12] #:style [fstyle null]]
    (for/fold ([skills (text "Skills:" fstyle size)]) ([skill (in-list items)])
      (vl-append skills (hc-append (exact-round (* size 1/2)) (arrowhead (exact-round (* size 2/3)) 0)
                                   (text (string-join (map symbol->string skill)) fstyle size))))})

(define rgb->hsv
  {lambda [r g b]
    (define-values {max0 min0} (values (max r g b) (min r g b)))
    (define chroma (- max0 min0))
    (values chroma #|(* 1/3 pi (cond [(zero? chroma) 0]
                                     [(= max0 r) (+ (/ (- g b) chroma) (if (< g b) 6 0))]
                                     [(= max0 g) (+ (/ (- b r) chroma) 2)]
                                     [(= max0 b) (+ (/ (- r g) chroma) 4)]))|#
            (cond [(zero? max0) 0]
                  [else (/ chroma max0)])
            max0)})

(define ~words
  {lambda [src]
    (define/match (~str chars capital? literal?)
      [{(? null?) _ _} null]
      [{(list #\space tail ...) _ _} (cons #\space (~str tail capital? literal?))]
      [{(list (or #\! #\? #\.) tail ...) _ _} (cons (car chars) (~str tail #true literal?))]
      [{(list #\" tail ...) _ _} (~str tail capital? (not literal?))]
      [{(list head tail ...) _ #true} (cons head (~str tail #false #true))]
      [{(list head tail ...) #true _} (cons (char-upcase head) (~str tail #false literal?))]
      [{(list head tail ...) _ _} (cons (char-downcase head) (~str tail #false #false))])
    (string-split (list->string (~str (string->list (string-trim (format "~s" src) @pregexp{[()]})) #true #false)))})
