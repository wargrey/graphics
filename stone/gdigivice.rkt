#lang scribble/text

@(require "../digitama/digicore.rkt")

#!/bin/sh

#|
# `raco setup` makes it hard to set other options,
# I have to keep the launch command simple.
exec racket -N "`basename $0 .rkt`" -t "$0" -- ${1+"$@|#\@|"}
|#

#lang at-exp typed/racket/gui

(require typed/pict)

(require "../digitama/digicore.rkt")

(define digivice : Symbol (#%module))

(module digitama racket
  (provide (all-defined-out))

  (require pict)
  (require images/compile-time)
  
  (require (for-syntax images/logos))
  (require (for-syntax images/icons/misc))
  (require (for-syntax images/icons/symbol))
  (require (for-syntax images/icons/stickman))

  (define-for-syntax splash-width 256)
  (define-for-syntax sprite-height 32)

  (define-values (default-logo default-image)
    (values (cc-superimpose (bitmap (compiled-bitmap (lambda-icon #:height (* 3/8 splash-width))))
                            (cellophane (bitmap (compiled-bitmap (planet-logo #:height (* 1/2 splash-width)))) 0.64))
            (bitmap (compiled-bitmap (regular-polygon-icon 4 #:color "LightSkyBlue" #:height splash-width)))))
  
  (define-values (splash-stickman splash-stickmen)
    (values (bitmap (compiled-bitmap (standing-stickman-icon #:body-color "Crimson" #:head-color "Crimson" #:height sprite-height)))
            (map bitmap (compiled-bitmap-list (for/list ([step (in-range 0.0 1.0 1/12)])
                                                (running-stickman-icon step #:height sprite-height)))))))

(require/typed
 (submod "." digitama)
 [default-logo pict]
 [default-image pict]
 [splash-stickman pict]
 [splash-stickmen (Listof pict)])

((lambda [[splash% : (Class #:implements Frame%)]]
   (parameterize* ([current-digimon "@(current-digimon)"]
                   [current-directory (digimon-digivice)]
                   [current-command-line-arguments (vector)])
     (send (make-object splash%) show #true)))
 (class frame% (inherit show)
   (define is-frame%? ((inst make-subclass? Frame%) frame%))
   (define is-frame? ((inst make-is-a? Frame%) frame%))

   (define splash-logger (make-logger 'splash /dev/log))
   (define logo.png : Path (build-path (digimon-icon) (string-append (#%digimon) ".png")))

   (define splash-image : pict
     (let ([splash.png : Path (build-path (digimon-stone) (string-append (symbol->string digivice) ".png"))])
       (if (file-readable? splash.png) (bitmap splash.png)
           (let* ([margin : Integer (exact-round (/ (pict-height default-image) 32))] ; default images/icons' height is 32
                  [customer : String (or (pkg-institution) (pkg-domain))]
                  [title-font : (Instance Font%) (make-font #:size 32 #:face "Helvetica, Bold" #:underlined? #true)]
                  [title-color : (Instance Color%) (make-object color% "Snow")]
                  [text-color : (Instance Color%) (make-object color% "Gray")])
             (cc-superimpose default-image
                             (ct-superimpose (blank (- (pict-height default-image) margin margin))
                                             (if (file-readable? logo.png) (bitmap logo.png) default-logo))
                             (lb-superimpose (blank (- (pict-height default-image) margin margin margin margin))
                                             (vl-append (text (#%digimon) (cons title-color title-font))
                                                        (text (format "Version: ~a" (#%info 'version)) (cons title-color normal-control-font))
                                                        (ghost (text "newline"))
                                                        (text (string-append "Created for " customer) (cons text-color tiny-control-font))
                                                        (text (match (#%info 'pkg-authors void)
                                                                [(list 1st 2nd others ...) (format "By ~a, et al." 1st)]
                                                                [(list author) (format "By ~a" author)]
                                                                [else "By WarGrey Ju"])
                                                              (cons text-color tiny-control-font)))))))))

   (define-values (width height) (get-display-size))
   (define splash-width : Natural (max 0 (exact-round (pict-width splash-image))))
   (define splash-margin : Real (/ splash-width 32))
   (define splash-height : Natural (max 0 (exact-round (+ (pict-height splash-image) (pict-height splash-stickman) splash-margin))))
   (define draw-splash : (-> (Instance DC<%>) Real Real Void) (make-pict-drawer splash-image))
   
   (super-make-object (#%digimon) #false splash-width splash-height
                      (and width (exact-round (* 1/2 (- width splash-width))))
                      (and height (exact-round (* 1/2 (- height splash-height))))
                      '(no-resize-border no-caption no-system-menu hide-menu-bar float)
                      #true 0 0 '(left top) splash-width splash-height #false #false)

   (define progress-icons : pict (blank 0 0))
   (define progress-man : pict (last splash-stickmen))
   (define progress-text : pict (blank 0 0))
   (define splash : (Instance Canvas%)
     (instantiate canvas% (this '(no-focus))
       [paint-callback (λ [[sketch : (Instance Canvas%)] [painter : (Instance DC<%>)]]
                         (define error? : Boolean (eq? progress-man splash-stickman))
                         (define splash-man-y : Real (+ splash-width splash-margin))
                         (define splash-man-offset : Real (+ (pict-width progress-icons) splash-margin))
                         (when error? (draw-splash painter 0 0))
                         (draw-pict progress-text painter 0 splash-width)
                         (draw-pict progress-icons painter 0 splash-man-y)
                         (draw-pict progress-man painter splash-man-offset splash-man-y)
                         (unless error? (draw-splash painter 0 0)))]))

   (define progress-update! : (-> (U pict (Listof pict)) [#:icon (Option pict)] Void)
     (let ([total : Index (length splash-stickmen)]
           [sprite-frame : Index 0])
       (lambda [picts #:icon [icon #false]]
         (define error? : Boolean (list? picts))
         (unless error? (set! sprite-frame (remainder (add1 sprite-frame) total)))
         (set! progress-man (if error? splash-stickman (list-ref splash-stickmen sprite-frame)))
         (when (pict? icon)
           (define icon-size : Real (pict-width progress-man))
           (set! progress-icons (hc-append splash-margin progress-icons (scale-to-fit icon icon-size icon-size))))
         (unless (null? picts)
           (define message : pict (if (list? picts) (apply vl-append picts) (vl-append progress-text picts)))
           (define drop-size : Real (- (pict-height message) (pict-height progress-man)))
           (set! progress-text (clip-descent (lift-above-baseline message (max 0 drop-size)))))
         (send splash refresh-now))))
   
   (define ghostcat : Thread
     (thread (thunk (let ([log-evt : Log-Receiver (make-log-receiver splash-logger 'debug)]
                          [color-error : (Instance Color%) (make-object color% "Crimson")]
                          [color-warning : (Instance Color%) (make-object color% "Yellow")]
                          [color-info : (Instance Color%) (make-object color% "ForestGreen")])
                      (let dtrace : Void ()
                        (match (sync/enable-break log-evt)
                          [(vector 'info (? string? message) urgent 'splash)
                           (progress-update! #:icon (and (pict? urgent) urgent) (text message (list color-info)))
                           (dtrace)]
                          [(vector 'warning (? string? message) _ 'splash)
                           (progress-update! (text message (list color-warning)))
                           (dtrace)]
                          [(vector (or 'fatal 'error) (? string? message) _ 'splash)
                           (progress-update! (list (text message (list color-error 'italic))
                                                   (text "Press any key to exit..." (cons 'bold 'modern))))]
                          [_ (dtrace)]))))))

   (define splash-load : (-> Path (Option Symbol) AnyValues)
     (let ([origin-load (current-load)]
           [color-debug : (Instance Color%) (make-object color% "Gray")])
       (lambda [modpath submods]
         (define modname : (Option String)
           (match (cons (file-name-from-path modpath) submods)
             [(cons _ (? symbol? modname)) (symbol->string modname)]
             [(cons modname (list-rest #false submods)) (~a submods)]
             [_ #false]))
         (when (string? modname)
           (progress-update! (text (string-append "splash: loading " modname) (list color-debug))))
         (origin-load modpath submods))))
   
   (define/override (on-subwindow-char _ keyborad)
     (cond [(thread-running? ghostcat) #false]
           [else (and (show #false) #true)]))

   (define/override (on-superwindow-show show?)
     (unless (false? show?)
       (with-handlers ([exn? (λ [[e : exn]] (log-message splash-logger 'error (exn-message e) (exn-continuation-marks e)))])
         (parameterize ([current-load splash-load]
                        [current-logger splash-logger])
           (define modname : String (symbol->string digivice))
           (define modpath : Path (build-path (digimon-digivice) modname (string-append modname ".rkt")))
           (unless (file-readable? modpath) (error digivice "application not found!"))
           (define digivice% (dynamic-require modpath 'digivice% (thunk (error digivice "digivice% not found!"))))
           (cond [(false? (is-frame%? digivice%)) (error digivice "digivice% should be a frame%!")]
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
