#lang scribble/text

@(require "../digitama/digicore.rkt")

#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket -N "`basename $0 .rkt`" -t "$0" -- ${1+"$@|#\@|"}
|#

#lang at-exp typed/racket/gui

(require "../digitama/digicore.rkt")

(define digivice : Symbol (#%module))

(define-type Bitmap (Instance Bitmap%))

(module digitama racket
  (provide (all-defined-out))

  (require images/compile-time)

  (require (for-syntax racket/draw))
  (require (for-syntax racket/class))
  
  (require (for-syntax images/logos))
  (require (for-syntax images/icons/misc))
  (require (for-syntax images/icons/symbol))
  (require (for-syntax images/icons/stickman))

  (define-for-syntax splash-width 256)
  (define-for-syntax sprite-height 32)

  (define-values (default-logo default-image)
    (values (compiled-bitmap (let ([lambda.icon (lambda-icon #:height (* 3/8 splash-width))]
                                   [planet.icon (planet-logo #:height (* 1/2 splash-width))])
                               (define-values (lw lh) (values (send lambda.icon get-width) (send lambda.icon get-height)))
                               (define-values (pw ph) (values (send planet.icon get-width) (send planet.icon get-height)))
                               (define-values (w h) (values (max lw pw) (max pw ph)))
                               (define dc (make-object bitmap-dc% (make-bitmap w h #:backing-scale (send planet.icon get-backing-scale))))
                               (send dc draw-bitmap planet.icon (/ (- w pw) 2) (/ (- h ph) 2))
                               (send dc set-alpha 0.64)
                               (send dc draw-bitmap lambda.icon (/ (- w lw) 2) (/ (- h lh) 2))
                               (send dc get-bitmap)))
            (compiled-bitmap (regular-polygon-icon 4 #:color "LightSkyBlue" #:height splash-width))))

  (define-values (splash-fault-man splash-running-men)
    (values (compiled-bitmap (standing-stickman-icon #:body-color "Crimson" #:head-color "Crimson" #:height sprite-height))
            (compiled-bitmap-list (for/list ([step (in-range 0.0 1.0 1/12)])
                                    (running-stickman-icon step #:height sprite-height))))))

(require/typed
 (submod "." digitama)
 [default-logo Bitmap]
 [default-image Bitmap]
 [splash-fault-man Bitmap]
 [splash-running-men (Listof Bitmap)])

(define display-scale : Nonnegative-Real (or (get-display-backing-scale) 1.0))
(define blank-bitmap : Bitmap (make-bitmap 1 1 #:backing-scale display-scale))

(define canvas-color : (Instance Color%) (make-object color% "Azure"))
(define title-color : (Instance Color%) (make-object color% "Snow"))
(define text-color : (Instance Color%) (make-object color% "Gray"))
(define error-color : (Instance Color%) (make-object color% "Crimson"))
(define warning-color : (Instance Color%) (make-object color% "Gold"))
(define info-color : (Instance Color%) (make-object color% "ForestGreen"))
(define debug-color : (Instance Color%) (make-object color% "Gray"))

(define title-font : (Instance Font%) (make-font #:size 32 #:face "Helvetica, Bold" #:underlined? #true))
(define terminal-font : (Instance Font%) (make-font #:weight 'bold #:family 'modern))
(define error-font : (Instance Font%) (make-font #:style 'italic))

(define bitmap : (-> Path-String Bitmap Bitmap)
  (lambda [src.png default]
    (with-handlers ([exn:fail? (const default)])
      (read-bitmap src.png #:try-@|#\@|2x? #true))))

(define bitmap-blank : (->* () (Nonnegative-Real (Option Nonnegative-Real) #:backing-scale Nonnegative-Real) Bitmap)
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale display-scale]]
    (define width : Positive-Integer (max 1 (exact-ceiling w)))
    (define height : Positive-Integer (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

(define bitmap-text : (->* (String) ((U False String (Instance Color%)) (Instance Font%)) Bitmap)
  (lambda [content [fgcolor #false] [font (make-font)]]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% blank-bitmap))
    (define-values (width height descent ascent) (send dc get-text-extent content font #true))
    (send dc set-bitmap (bitmap-blank width height))
    (send dc set-font font)
    (when fgcolor (send dc set-text-foreground fgcolor))
    (send dc draw-text content 0 0 #true)
    (or (send dc get-bitmap) blank-bitmap)))

(define bitmap-desc : (-> String Positive-Real (U String (Instance Color%)) (Instance Font%) Bitmap)
  (lambda [description max-width.0 fgcolor font]
    (define dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (bitmap-blank)))
    (define max-width : Positive-Integer (exact-ceiling max-width.0))
    (define desc-extent : (-> String Integer Integer (Values String Natural Nonnegative-Real))
      (lambda [desc start end]
        (define subdesc : String (substring desc start end))
        (define-values (width height descent ascent) (send dc get-text-extent subdesc font #true))
        (values subdesc (exact-ceiling width) height)))

    (match-define-values (_ char-width phantom-height) (desc-extent " " 0 1))
    (define-values (descs ys width height)
      (for/fold ([descs : (Listof String) null] [ys : (Listof Real) null] [width : Natural 0] [height : Real 0])
                ([desc : String (in-list (string-split description (string #\newline)))])
        (define terminal : Index (string-length desc))
        (let desc-row : (Values (Listof String) (Listof Real) Natural Real)
          ([idx0 : Natural 0] [descs : (Listof String) descs] [ys : (Listof Real) ys]
                              [Widthn : Natural width] [yn : Real height])
          (define-values (descn widthn heightn idx)
            (let desc-col-expt : (Values String Natural Real Natural)
              ([interval : Natural 1] [open : Natural idx0] [close : Natural terminal]
                                      [backtracking : String ""] ; char-width is bad for forecasting the next width 
                                      [back-width : Natural max-width] [back-height : Nonnegative-Real phantom-height])
              (define idx : Natural (min close (+ open interval)))
              (define next-open : Natural (+ open (quotient interval 2)))
              (define-values (line width height) (desc-extent desc idx0 idx))
              (define found? : Boolean (and (> width max-width) (= interval 1) (<= back-width max-width)))
              (cond [found? (values backtracking back-width back-height (if (zero? open) terminal (max 0 (sub1 idx))))]
                    [(> width max-width) (desc-col-expt 1 next-open idx backtracking back-width back-height)]
                    [(= close idx) (values line width height idx)]
                    [else (desc-col-expt (arithmetic-shift interval 1) open close line width height)])))
          (if (fx= idx terminal)
              (values (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))
              (desc-row idx (cons descn descs) (cons yn ys) (max widthn Widthn) (+ yn heightn))))))
    
    (send dc set-bitmap (bitmap-blank (max 1 width) (max 1 phantom-height height)))
    (send dc set-font font)
    (send dc set-text-foreground fgcolor)
    (for ([desc (in-list (reverse descs))] [y (in-list (reverse ys))])
      (send dc draw-text desc 0 y #true))
    (or (send dc get-bitmap) (bitmap-blank))))

((lambda [[splash% : (Class #:implements Frame%)]]
   (parameterize* ([current-digimon "@(current-digimon)"]
                   [current-directory (digimon-digivice)]
                   [current-command-line-arguments (vector)])
     (send (make-object splash%) show #true)))
 (class frame% (inherit show)
   (define subframe%? ((inst make-subclass? Frame%) frame%))
   (define bitmap%? ((inst make-is-a? Bitmap%) bitmap%))

   (define splash-logger (make-logger 'splash /dev/log))
   (define logo.png : Path (build-path (digimon-icon) (string-append (#%digimon) ".png")))

   (define splash.bmp : Bitmap
     (bitmap (build-path (digimon-stone) (string-append (symbol->string digivice) ".png"))
             (let ([painter : (Instance Bitmap-DC%) (send default-image make-dc)])
               (define-values (title version customer author)
                 (values (#%digimon) (format "Version: ~a" (#%info 'version))
                         (string-append "Created for " (or (pkg-institution) (pkg-domain)))
                         (match (#%info 'pkg-authors void)
                           [(list 1st 2nd others ...) (format "By ~a, et al." 1st)]
                           [(list author) (format "By ~a" author)]
                           [else "By WarGrey Ju"])))
               
               (define-values (width height) (send painter get-size))
               (define inset : Integer (* (exact-round (/ height (or 32 'default 'height 'in '(images/icons)))) 2))
               
               (define logo.icon : Bitmap (bitmap logo.png default-logo))
               (send painter set-smoothing 'aligned)
               (send painter draw-bitmap logo.icon (/ (- width (send logo.icon get-width)) 2) inset)

               (match-define-values (_ ghost-height _ _) (send painter get-text-extent ""))
               (match-define-values (_ title-height _ _) (send painter get-text-extent title title-font))
               (match-define-values (_ version-height _ _) (send painter get-text-extent version normal-control-font))
               (match-define-values (_ customer-height _ _) (send painter get-text-extent customer tiny-control-font))
               (match-define-values (_ author-height _ _) (send painter get-text-extent author tiny-control-font))
               
               (define customer-y : Real (- height inset author-height customer-height))
               (define title-y : Real (- customer-y ghost-height version-height title-height))
               (send* painter
                 (set-text-foreground title-color)
                 (set-font title-font)
                 (draw-text title inset title-y)
                 (set-font normal-control-font)
                 (draw-text version inset (+ title-y title-height))
                 (set-text-foreground text-color)
                 (set-font tiny-control-font)
                 (draw-text customer inset customer-y)
                 (draw-text author inset (+ customer-y customer-height)))
               (or (send painter get-bitmap) default-image))))

   (define-values (width height) (get-display-size))
   (define splash-width : Natural (send splash.bmp get-width))
   (define splash-margin : Natural (exact-ceiling (/ splash-width 32)))
   (define splash-height : Natural (+ (send splash.bmp get-height) (send splash-fault-man get-height) splash-margin))
   (define splash-icon+gap : Positive-Integer (+ splash-margin (send splash-fault-man get-width)))

   (super-make-object (#%digimon) #false splash-width splash-height
                      (and width (exact-round (* 1/2 (- width splash-width))))
                      (and height (exact-round (* 1/2 (- height splash-height))))
                      '(no-resize-border no-caption no-system-menu hide-menu-bar float)
                      #true 0 0 '(left top) splash-width splash-height #false #false)

   (define progress-messages : (Listof Bitmap) null)
   (define progress-icons : (Listof Bitmap) null)
   (define progress-man : Bitmap (last splash-running-men))
   
   (define splash : (Instance Canvas%)
     (instantiate canvas% (this '(no-focus))
       [paint-callback (λ [[sketch : (Instance Canvas%)] [painter : (Instance DC<%>)]]
                         (define-values (text-top icon-alpha)
                           (cond [(eq? progress-man splash-fault-man) (values 0 0.24)]
                                 [else (values splash-width 1.0)]))
                         (send painter set-alpha 1.0)
                         (send painter draw-bitmap splash.bmp 0 0)
                         (let draw-message : Void ([messages : (Listof Bitmap) progress-messages]
                                                   [bottom : Integer splash-height])
                           (unless (or (null? messages) (<= bottom text-top))
                             (define info : Bitmap (car messages))
                             (define height : Positive-Integer (send info get-height))
                             (define hinfo : Positive-Integer (min (max 1 (- bottom text-top)) height))
                             (send painter draw-bitmap-section info
                                   splash-margin (- bottom hinfo) 0 (- height hinfo)
                                   (max 0 (- splash-width splash-margin splash-margin)) hinfo)
                             (draw-message (cdr messages) (- bottom hinfo))))
                         (send painter set-alpha icon-alpha)
                         (for ([icon (in-list (reverse (cons progress-man progress-icons)))]
                               [idx (in-naturals)])
                           (send painter draw-bitmap icon
                                 (+ (* splash-icon+gap idx) splash-margin)
                                 (+ splash-width splash-margin))))]))

   (define progress-update! : (-> Bitmap [#:icon (Option Bitmap)] [#:error (Option Bitmap)] Void)
     (let ([total : Index (length splash-running-men)]
           [sprite-frame : Index 0])
       (lambda [message #:icon [icon #false] #:error [error #false]]
         (set! progress-messages (cons message progress-messages))
         (when (bitmap%? icon)
           (define icon-size : Positive-Integer (max 1 (- splash-icon+gap splash-margin)))
           (define icon-w : Positive-Integer (send icon get-width))
           (define icon-h : Positive-Integer (send icon get-height))
           (define icon-scale : Real (/ icon-size (max icon-w icon-h) 1.0))
           (define scaled-icon : Bitmap
             (cond [(= icon-scale 1.0) icon]
                   [else (let ([scaled-bmp : Bitmap (bitmap-blank icon-size icon-size)])
                           (define dc : (Instance Bitmap-DC%) (send scaled-bmp make-dc))
                           (send dc set-smoothing 'aligned)
                           (send dc set-scale icon-scale icon-scale)
                           (send dc draw-bitmap icon
                                 (/ (- icon-size (* icon-w icon-scale)) 2)
                                 (/ (- icon-size (* icon-h icon-scale)) 2))
                           scaled-bmp)]))
           (set! progress-icons (cons scaled-icon progress-icons)))
         (when (false? error)
           (set! sprite-frame (remainder (add1 sprite-frame) total))
           (set! progress-man (list-ref splash-running-men sprite-frame)))
         (when (bitmap%? error)
           (set! progress-man splash-fault-man)
           (set! progress-messages (cons error progress-messages)))
         (send splash refresh-now))))

   (define ghostcat : Thread
     (thread (thunk (let ([log-evt : Log-Receiver (make-log-receiver splash-logger 'debug)])
                      (let dtrace : Void ()
                        (match (sync/enable-break log-evt)
                          [(vector 'info (? string? message) urgent 'splash)
                           (progress-update! (bitmap-text message info-color) #:icon (and (bitmap%? urgent) urgent))
                           (dtrace)]
                          [(vector 'warning (? string? message) _ 'splash)
                           (progress-update! (bitmap-text message warning-color))
                           (dtrace)]
                          [(vector (or 'fatal 'error) (? string? message) _ 'splash)
                           (progress-update! #:error (bitmap-text "Press any key to exit..." #false terminal-font)
                                             (bitmap-desc message (max 1 (- splash-width splash-margin splash-margin))
                                                          error-color error-font))]
                          [_ (dtrace)]))))))

   (define splash-load : (-> Path (Option Symbol) AnyValues)
     (let ([origin-load (current-load)])
       (lambda [modpath submods]
         (define modname : (Option String)
           (match (cons (file-name-from-path modpath) submods)
             [(cons _ (? symbol? modname)) (symbol->string modname)]
             [(cons modname (list-rest #false submods)) (~a submods)]
             [_ #false]))
         (when (string? modname)
           (progress-update! (bitmap-text (string-append "splash: loading " modname) debug-color)))
         (origin-load modpath submods))))

   (define/override (on-subwindow-char _ keyborad)
     (cond [(thread-running? ghostcat) #false]
           [else (and (show #false) #true)]))

   (define/override (on-superwindow-show show?)
     (unless (false? show?)
       (send splash set-canvas-background canvas-color) 
       (with-handlers ([exn? (λ [[e : exn]] (log-message splash-logger 'error (exn-message e) (exn-continuation-marks e)))])
         (parameterize ([current-load splash-load]
                        [current-logger splash-logger])
           (define modname : String (symbol->string digivice))
           (define modpath : Path (build-path (digimon-digivice) modname (string-append modname ".rkt")))
           (unless (file-readable? modpath) (error digivice "application not found!"))
           (define digivice% (dynamic-require modpath 'digivice% (thunk (error digivice "digivice% not found!"))))
           (cond [(false? (subframe%? digivice%)) (error digivice "digivice% should be a frame%!")]
                 [else (let ([d-ark (new digivice% [label (string-titlecase modname)])])
                         (send* d-ark
                           (set-icon (make-object bitmap% logo.png 'unknown/alpha))
                           (show #true)
                           (center 'both)
                           (maximize #true))
                         (show #false))]))))
     (when (false? show?)
       (current-logger /dev/log)
       (unless (thread-dead? ghostcat) (kill-thread ghostcat))))))
