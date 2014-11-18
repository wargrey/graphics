#lang at-exp racket/gui

(require net/http-client)

(require pict)
(require plot)
(require (only-in plot/utils
                  color-seq*
                  ->brush-color))
(require racket/draw)

(require math/flonum)
(require images/flomap)
(require images/icons/control)
(require images/icons/symbol)
(require images/icons/style)
(require images/icons/misc)

(provide (all-defined-out))

(define rosetta-stone-dir (make-parameter ".book/stone"))

(define wikimon-recv!
  {lambda [uri]
    (define-values {status headers pipe} (http-sendrecv "wikimon.net" uri #:content-decode null))
    (unless (regexp-match? #px"\\s200\\sOK" status) (error (bytes->string/utf-8 status)))
    (list status headers pipe)})

(struct digimon {name figure stage type attribute fields})

(define recv-digimon
  {lambda [diginame]
    (with-handlers ([exn? {lambda [e] (lt-superimpose (blank (plot-width) (plot-height))
                                                      (for/fold ([flmp #false]) ([line (in-list (with-input-from-string (exn-message e) {thunk (port->lines)}))])
                                                        (define fline (desc line (plot-width) #:color "Firebrick"))
                                                        (if flmp (vl-append flmp fline) fline)))}])
      (define metainfo (regexp-match (pregexp (string-append "<img alt=.([ァ-ヺ]+). src=.(/images/.+?jpg)."
                                                             ".+?レベル</font>.+?.white.>(.+?)</font>"
                                                             ".+?型（タイプ）</font>.+?.white.>(.+?)</font>"
                                                             ".+?属性</font>.+?.white.>(.+?)</font>"
                                                             ".+?>フィールド</font>(.+?)List of Digimon"))
                                     (third (wikimon-recv! (string-append "/" (string-titlecase (~a diginame)))))))
      (digimon (bytes->string/utf-8 (second metainfo))
               (bitmap->flomap (make-object bitmap% (third (wikimon-recv! (third metainfo)))))
               (bytes->string/utf-8 (fourth metainfo))
               (bytes->string/utf-8 (fifth metainfo))
               (bytes->string/utf-8 (sixth metainfo))
               (filter-map {lambda [field] (let ([png (format "~a/~a.png" (rosetta-stone-dir)
                                                              (build-string (string-length field)
                                                                            {lambda [i] (integer->char (- (char->integer (string-ref field i))
                                                                                                          (- (char->integer #\Ａ) (char->integer #\A))))}))])
                                             (if (file-exists? png) (bitmap->flomap (make-object bitmap% png 'png/alpha)) #false))}
                    (regexp-match* #px"[Ａ-Ｚ]{2}[ａ-ｚ]?" (bytes->string/utf-8 (seventh metainfo))))))})

(define digimon-visualize
  {lambda [digimon0]
    (define digimon (flomap-flip-vertical digimon0))
    (define get-pixel {lambda [x y] (let ([flv (flomap-ref* digimon x y)]) (rgb->hsv (flvector-ref flv 1) (flvector-ref flv 2) (flvector-ref flv 3)))})
    (define prop (/ (plot-height) (plot-width)))
    (define count 5)
    (plot3d-pict (list (contour-intervals3d {lambda [x y] (let-values ([{v who cares} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            #:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.04))
                       (contour-intervals3d {lambda [x y] (let-values ([{who v cares} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            ;#:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.08))
                       (contour-intervals3d {lambda [x y] (let-values ([{who cares v} (get-pixel x y)]) (* v prop))}
                                            0 (flomap-width digimon) 0 (flomap-height digimon)
                                            ;#:colors (color-seq* (list dark-metal-icon-color metal-icon-color light-metal-icon-color) count)
                                            #:alphas (make-list count 0.08))))})

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

(define color->flvector
  {lambda [color #:alpha [alpha #false]]
    (define clr (if (is-a? color color%) color (make-object color% (~a color))))
    (list->flvector (cons (if alpha alpha (send clr alpha))
                          (map {lambda [val] (exact->inexact (/ val #xFF))}
                               (list (send clr red) (send clr green) (send clr blue)))))})

(define digimoji
  {lambda [content #:height [size (plot-font-size)] #:color [color dark-metal-icon-color]]
    (define flcolor (color->flvector color))
    (define background (blank size size))
    (define {translate moji0 mojin}
      (define moji (cond [(char=? moji0 #\-) #\ー]
                         [(member moji0 '{#\ゕ #\ヵ}) (box #\カ)]
                         [(member moji0 '{#\ゖ #\ヶ}) (box #\ケ)]
                         [(member moji0 '{#\ぢ #\ヂ}) (list #\デ (box #\イ))]
                         [(member moji0 '{#\づ #\ヅ}) (list #\ド (box #\ウ))]
                         [(member moji0 '{#\ゐ #\ヰ}) (list #\ウ (box #\イ))]
                         [(member moji0 '{#\ゑ #\ヱ}) (list #\ウ (box #\エ))]
                         [(member moji0 '{#\ぁ #\ぃ #\ぅ #\ぇ #\ぉ #\っ #\ゃ #\ゅ #\ょ #\ゎ}) (box (integer->char (+ (char->integer moji0) 96 1)))]
                         [(member moji0 '{#\ァ #\ィ #\ゥ #\ェ #\ォ #\ッ #\ャ #\ュ #\ョ #\ヮ}) (box (integer->char (add1 (char->integer moji0))))]
                         [(char<=? #\あ moji0 #\ゔ) (integer->char (+ (char->integer moji0) 96))]
                         [(char<=? #\A moji0 #\Z) (char-downcase moji0)]
                         [else moji0]))
      (if (list? moji) (append moji mojin) (cons moji mojin)))
    (for/fold ([dgmj #false]) ([moji (in-list (foldr translate null (string->list (~a content))))])
      (define fmoji (format "~a/~a.png" (rosetta-stone-dir) (if (box? moji) (unbox moji) moji)))
      (define pmoji (cond [(file-exists? fmoji) (let*-values ([{flng} (flomap-trim (bitmap->flomap (make-object bitmap% fmoji 'png/alpha)))]
                                                              [{width0 height0} (flomap-size flng)]
                                                              [{scale%} (if (box? moji) 3/5 (/ (- size 2) (max width0 height0)))])
                                                  (for* ([x (in-range width0)] [y (in-range height0)])
                                                    (define pos (* (+ x (* y width0)) 4))
                                                    (unless (zero? (flvector-ref (flomap-values flng) pos))
                                                      (for ([offset (in-range 1 4)])
                                                        (flvector-set! (flomap-values flng) (+ pos offset) (flvector-ref flcolor offset)))))
                                                  ((if (box? moji) cb-superimpose cc-superimpose) background (bitmap (flomap->bitmap (flomap-scale flng scale%)))))]
                          [(equal? moji #\space) background]
                          [else (cc-superimpose background (text (~a moji) (cons (make-object color% color) null) size))]))
      (if dgmj (hc-append dgmj pmoji) pmoji))})

(define desc
  {lambda [content0 width #:ftext [ftext0 digimoji] #:height [size (default-icon-height)] #:color [color dark-metal-icon-color]]
    (define {ftext str}
      (define txt (cond [(equal? ftext0 text-icon) (text-icon str #:height size #:color color #:trim? #false)]
                        [(equal? ftext0 text) (text str (cons (make-object color% color) null) size)]
                        [else (ftext0 str #:height size #:color color)]))
      (cond [(flomap? txt) (bitmap (flomap->bitmap txt))]
            [(is-a? txt bitmap%) (bitmap txt)]
            [else txt]))
    (let desc-row ([content (~a content0)] [head0 #false])
      (define smart (let desc-col ([words content] [room width])
                      (with-handlers ([exn:fail:contract? (const (list (blank 0 0) words))])
                        (define ptxt (ftext (~a (sequence-ref words 0))))
                        (if (< room (pict-width ptxt))
                            (raise-range-error 'desc "words" "ending " (pict-width ptxt) words 0 room width)
                            (let ([final (desc-col (sequence-tail words 1) (- room (pict-width ptxt)))])
                              (list (hc-append ptxt (first final)) (second final)))))))
      (define headn (cond [(zero? (sequence-length (second smart))) (first smart)]
                          [else (desc-row (second smart) (first smart))]))
      (if head0 (vl-append head0 headn) headn))})

(define profile
  {lambda [monster details skills]
    (define fsize (plot-font-size))
    (define flcolor (color->flvector metal-icon-color))
    (define hbar (ghost (bitmap (bar-icon #:height fsize #:color metal-icon-color))))
    (define {fdigimoji txt [color light-metal-icon-color]} (digimoji (~a txt) #:height fsize #:color color))
    (define {fbg fg [wdth #false]} (let ([width (if wdth wdth (exact-round (+ fsize (pict-width fg))))]
                                         [height (exact-round (+ fsize (pict-height fg)))])
                                     (cc-superimpose (cellophane (bitmap (flomap->bitmap (flomap-shadow (make-flomap* width height flcolor) fsize flcolor))) 1.00) fg)))
    (define head (apply vr-append
                        (fbg (vc-append (pict-width hbar) (fdigimoji "Digital Monster") (fdigimoji (digimon-name monster))) (plot-width))
                        (map {lambda [emblem] (let ([% (/ (default-icon-height) (flomap-width emblem))])
                                                (cellophane (bitmap (flomap->bitmap (flomap-scale emblem %))) %))}
                             (digimon-fields monster))))
    (define body (let ([skill (apply vl-append (pict-width hbar) (map {lambda [skill] (hc-append (bitmap (bomb-icon #:height fsize)) hbar (fdigimoji skill))} skills))])
                   (fbg (vl-append (desc details (pict-width skill) #:ftext text #:height fsize #:color light-metal-icon-color) hbar skill))))
    (define watermark (vr-append (fdigimoji "wargrey" metal-icon-color) (rotate hbar (/ pi 2))))
    (rb-superimpose (rt-superimpose (lb-superimpose (blank (plot-width) (plot-height)) body) head) watermark)})

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
