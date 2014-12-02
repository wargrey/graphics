#lang at-exp racket/gui

(require net/http-client)

(require plot)
(require pict)
(require pict/convert)
(require racket/draw)

(require math/flonum)
(require images/flomap)
(require images/icons/symbol)
(require images/icons/style)

(provide (all-defined-out))
(provide (all-from-out pict plot racket/draw))
(provide (all-from-out math/flonum images/flomap images/icons/symbol images/icons/style))

(define rosetta-stone-dir (make-parameter (current-directory)))

(define digimon-illustration
  {lambda [monster #:max-width [width 400] #:max-height [height 600]]
    (define fdesc {lambda [txt width] (desc txt width #:ftext text #:height 12 #:color "black")})
    (define space (fdesc " " width))
    (define figure (bitmap (flomap->bitmap (flomap-resize (cdr (digimon-figure monster)) width #false))))
    (define name (let ([pname (fdesc (digimon-kana monster) (pict-width figure))]) (cc-superimpose (rounded-rectangle (pict-width figure) (+ (pict-height pname) 4) -0.4) pname)))
    (define profile (let* ([kvs (filter-map {match-lambda [(cons k v) (if (null? v) #false (cons (hc-append (fdesc k width) space) v))]}
                                            (list (cons 'レベル (digimon-stage monster)) (cons 'タイプ (digimon-type monster)) (cons '属性 (digimon-attribute monster))
                                                  (cons 'フィールド (digimon-fields monster)) (cons 'グループ (digimon-group monster))
                                                  (cons '必殺技 (digimon-attacks monster)) (cons '特徴 (digimon-profile monster))))]
                           [k-width (+ (pict-width (car (argmax (compose1 pict-width car) kvs))) (* (pict-width space) 3))]
                           [v-width (- (pict-width figure) k-width)])
                      (for/fold ([p (blank 0 0)]) ([kv (in-list kvs)])
                        (define pvalue (if (and (list? (cdr kv)) (cons? (cadr kv)))
                                           (apply hc-append (map (compose1 bitmap flomap->bitmap (curryr flomap-scale 0.50) cdr) (cdr kv)))
                                           (apply vl-append 2 (map (curryr fdesc v-width) (if (list? (cdr kv)) (cdr kv) (list (cdr kv)))))))
                        (define r-height (+ (pict-height pvalue) 2))
                        (vl-append p (lb-superimpose (hc-append (rc-superimpose (colorize (filled-rectangle k-width r-height #:draw-border? #false) "Gray") (car kv))
                                                                (lc-superimpose (blank v-width r-height) (hc-append (vline 1 r-height) space pvalue)))
                                                     (colorize (hline (pict-width figure) 1) "Black"))))))
    (define zukan (let* ([zk (vc-append 2 name figure profile)])
                    (cc-superimpose (colorize (filled-rectangle (+ (pict-width zk) 2) (+ (pict-height zk) 2)) "White") zk)))
    (define % (/ height (pict-height zukan)))
    (if (< % 1) (scale zukan %) zukan)})

(define digimon-qrcode
  {lambda [monster #:version [version 32] #:module-size [size 2]]
    (define content (let ([utf8->jisx0208 (bytes-open-converter "UTF-8" "JIS_X0208")])
                      (define-values {jis size status} (bytes-convert utf8->jisx0208 (string->bytes/utf-8 (digimon-profile monster))))
                      (bytes-close-converter utf8->jisx0208)
                      (build-list (/ size 2) {lambda [i] (let ([bi (* i 2)]) (integer-bytes->integer (subbytes jis bi (+ bi 2)) #false))})))
    (for-each (lambda [c] (unless (or (<= #x8140 c #x9FFC) (<= #xE040 c #xEBBF)) (printf "~x~n" c)))  content)
    (define-values {qrsize tsize} (let ([m+ (* version 4)]) (values (+ 17 m+) (add1 m+))))
    (define {square l [c "Black"]} (colorize (filled-rectangle l l #:draw-border? #false) c))
    (define {diag-fold pict0} (lt-superimpose pict0 (rotate pict0 (/ pi -2))))
    (define quiet-zone (cc-superimpose (square (+ qrsize 2) "White") (square qrsize "Gray")))
    (define finder (let ([fd (cc-superimpose (square 9 "White") (square 7) (square 5 "White") (square 3))]) (diag-fold (hc-append fd (blank tsize 9) fd))))
    (define timing (diag-fold (apply hc-append (make-list (/ (+ tsize 3) 2) (hc-append (square 1) (square 1 "White"))))))
    (define alignment (cc-superimpose (square 5) (square 3 "White") (square 1)))
    (define qrcode (pin-over (lt-superimpose quiet-zone finder) 7 7 timing))
    (scale qrcode size)})

(struct digimon {name kana figure stage type attribute fields group profile attacks}
  #:transparent
  #:property prop:pict-convertible digimon-illustration
  #:methods gen:custom-write
  [(define write-proc {lambda [this out mode]
                        ;(define rprint (case mode [{#true} write] [{#false} display] [{0 1} {lambda [val out] (print val out mode)}]))
                        (for ([field (in-vector (struct->vector this))])
                          (cond [(symbol? field) (fprintf out "#s(~a" (object-name this))]
                                [(list? field) (fprintf out " ~a" (map {lambda [val] (if (cons? val) (car val) val)} field))]
                                [(cons? field) (fprintf out " ~a" (car field))]
                                [else (fprintf out " ~a" field)]))
                        (fprintf out ")")})])

(define wikimon-recv!
  {lambda [uri]
    (define-values {status headers pipe} (http-sendrecv "wikimon.net" uri #:content-decode null))
    (unless (regexp-match? #px"\\s200\\sOK" status) (error (bytes->string/utf-8 status)))
    (list status headers pipe)})

(define recv-image
  {lambda [filename]
    (define rfile (format "/File:~a" filename))
    (define pxpng (pregexp (format "(?<=href..)/images[^>]+~a(?=.>)" filename)))
    (make-object bitmap% (third (wikimon-recv! (car (regexp-match* pxpng (third (wikimon-recv! rfile)))))) 'unknown/alpha)})

(define recv-digimon
  {lambda [diginame #:figure [filename #false] #:profile [details #false] #:attacks [attacks #false]]
    (with-handlers ([exn? {lambda [e] (lt-superimpose (blank (plot-width) (plot-height))
                                                      (for/fold ([flmp #false]) ([line (in-list (with-input-from-string (exn-message e) {thunk (port->lines)}))])
                                                        (define fline (desc line (plot-width) #:color "Firebrick"))
                                                        (if flmp (vl-append flmp fline) fline)))}])
      (define namemon (string-titlecase (~a diginame)))
      (define pxjp #px"[ぁ-ゖ゠-ヿ㐀-䶵一-鿋豈-頻（）]+")
      (define metainfo (map bytes->string/utf-8
                            (cdr (regexp-match (pregexp (string-append "⇨ English.+?<br\\s*/?>(.+?)\\s*</td>\\s*</tr>"
                                                                       ".+?<img alt=.([ァ-ー]+). src=.(/images/.+?jpg)"
                                                                       ".+?レベル(.+?)型（タイプ）(.+?)属性(.+?)List of Digimon"
                                                                       ".+?Attack Techniques</span></h1>(.+?)<h1>"))
                                               (third (wikimon-recv! (string-append "/" namemon)))))))
      (digimon namemon
               (second metainfo)
               (cond [(false? filename) (cons (third metainfo) (bitmap->flomap (make-object bitmap% (third (wikimon-recv! (third metainfo))))))]
                     [else (cons filename (recv-image filename))])
               (regexp-match* pxjp (fourth metainfo))
               (regexp-match* pxjp (fifth metainfo))
               (regexp-match* #px"ワクチン|データ|ウィルス|フリー|バリアブル|ヴァリアブル|不明" (sixth metainfo))
               (filter-map {lambda [f] (let* ([field (list->string (for/list ([fc (in-string f)]) (integer->char (- (char->integer fc) #xFEE0))))]
                                              [png (format "~a/~a.png" (rosetta-stone-dir) field)])
                                         (if (file-exists? png) (cons field (bitmap->flomap (make-object bitmap% png 'png/alpha))) #false))}
                           (regexp-match* #px"[Ａ-Ｚ]{2}[ａ-ｚ]?" (sixth metainfo)))
               (regexp-match* #px"四聖獣|十二神将|ロイヤルナイツ|七大魔王|三大天使|オリンポス十二神|四大竜|十闘士|クラックチーム|D-ブリガード|「BAN-TYO」|三銃士|ビッグデスターズ" (sixth metainfo))
               (if details (~a details) (regexp-replace* #px"</?font.*?>" (first metainfo) ""))
               (if (list? attacks) (map ~a attacks) (regexp-match* pxjp (seventh metainfo)))))})

(define digimon-ark
  {lambda [monster #:lightness [threshold 0.64] #:rallies [rallies0 null] #:show? [show? #true] #:close? [close? #true] #:bgcolor [bgcolor (get-panel-background)]]
    (define-values {width0 height0} (flomap-size (cdr (digimon-figure monster))))
    (define figure (flomap-copy (cdr (digimon-figure monster)) 0 0 width0 height0))
    (define {erase x y [hook void]}
      (define xy (* 4 (+ x (* y width0))))
      (when (and (< -1 x width0) (< -1 y height0)
                 (> (flvector-ref (flomap-values figure) xy) 0.0))
        (define-values {h s v} (rgb->hsv (flvector-ref (flomap-values figure) (+ 1 xy))
                                         (flvector-ref (flomap-values figure) (+ 2 xy))
                                         (flvector-ref (flomap-values figure) (+ 3 xy))))
        (when (> v threshold)
          (flvector-set! (flomap-values figure) xy 0.0)
          (hook x y)
          (erase (sub1 x) y hook)
          (erase x (sub1 y) hook)
          (erase (add1 x) y hook)
          (erase x (add1 y) hook))
        (list h s v)))
    (define {auto-erase [erase-hook void]}
      (for-each {lambda [rally]
                  (when (void? (erase (car rally) (cdr rally) erase-hook))
                    (printf "WARNING: (cons ~a ~a) is reduntant~n" (car rally) (cdr rally)))}
                (cons (cons 0 0) rallies0)))
    (define d-ark (new {class dialog% (super-new [label "Digimon Analyzer"] [style '{close-button}] [min-width width0] [min-height height0])
                         (field [rallies null] [ltxy #false])
                         (field [vark (new canvas% [parent this] [paint-callback {lambda [who-cares painter] (send painter draw-bitmap (flomap->bitmap figure) 0 0)}])])
                         (field [dark (let ([arc (send vark get-dc)])
                                        (send vark set-canvas-background bgcolor)
                                        (send arc set-pen (make-pen #:color bgcolor))
                                        arc)])
                         
                         (define/override {on-superwindow-show ?}
                           (cond [? (auto-erase {lambda [x y] (send dark draw-point x y)})
                                    (send vark refresh-now)
                                    (when close?
                                      (sleep 1)
                                      (send this show #false))]
                                 [else (for-each {lambda [sally] (printf "(cons ~a ~a)~n" (car sally) (cdr sally))} rallies)]))

                         (define/override (on-subwindow-event who mouse)
                           (when (equal? vark who)
                             (define-values {x0 y0} (values (send mouse get-x) (send mouse get-y)))
                             (case (send mouse get-event-type)
                               [{left-down} (set! ltxy (cons x0 y0))]
                               [{left-up} (let ([lt (if (cons? ltxy) ltxy (cons x0 y0))])
                                            (when (cons? ltxy)
                                              (set! ltxy #false)
                                              (newline))
                                            (for* ([x (in-range (min (car lt) (add1 x0)) (max (car lt) (add1 x0)))]
                                                   [y (in-range (min (cdr lt) (add1 y0)) (max (cdr lt) (add1 y0)))])
                                              (define v (erase x y))
                                              (send vark refresh-now)
                                              (if (void? v)
                                                  (printf "Ø XY(~a, ~a)~n" x y)
                                                  (let* ([~r4 (curry ~r #:precision '{= 4})]
                                                         [~print (curryr printf x y (~r4 (first v)) (~r4 (second v)) (~r4 (third v)))])
                                                    (if (> (third v) threshold)
                                                        {begin (set! rallies (append rallies (list (cons x y))))
                                                               (~print "√ XY(~a, ~a):: HSV(~a, ~a, ~a)~n")}
                                                        (~print "† XY(~a, ~a):: HSV(~a, ~a, ~a)~n"))))))])))}))
    (if show? (send d-ark show #true) (auto-erase))
    (bitmap (flomap->bitmap (flomap-trim figure #true)))})

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

(define color->flvector
  {lambda [color #:alpha [alpha #false]]
    (define clr (if (is-a? color color%) color (make-object color% (~a color))))
    (list->flvector (cons (if alpha alpha (send clr alpha))
                          (map {lambda [val] (exact->inexact (/ val #xFF))}
                               (list (send clr red) (send clr green) (send clr blue)))))})

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

;(frame (digimon-qrcode (recv-digimon 'Sakuyamon) #:version 16 #:module-size 4))
