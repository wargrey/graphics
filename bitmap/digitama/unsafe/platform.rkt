#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require typed/racket/private/gui-types)

(require "convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/class)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (make-canvas-surface canvas)
    (define-values (w h) (send canvas get-client-size))
    (define nsview (send canvas get-handle))
    (define os (system-type 'os))

    (hash-ref! platform-database os
               (λ [] (let ([platform.rkt (platform-module-path os)])
                       (define make-platform-surface (dynamic-require platform.rkt 'make-platform-surface (λ [] #false)))

                       (and make-platform-surface
                            (make-platform-surface nsview w h))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (platform-module-path os)
    (case os
      [(macosx)  (collection-file-path "cocoa.rkt" "bitmap" "digitama" "unsafe" "platform")]
      [(windows) (collection-file-path "win32.rkt" "bitmap" "digitama" "unsafe" "platform")]
      [else      (collection-file-path   "gtk.rkt" "bitmap" "digitama" "unsafe" "platform")]))
  
  (define platform-database (make-hasheq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [make-canvas-surface (-> (Instance Canvas%) Bitmap-Surface)])
