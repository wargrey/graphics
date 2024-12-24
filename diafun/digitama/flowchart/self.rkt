#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/path)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flow geo:group
  ([skeleton : Geo:Path])
  #:type-name Dia:Flow
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-initial-path : (-> (Option Symbol) Real Real Real Geo-Print-Datum Gomamon)
  (lambda [id gw gh ts home]
    (define grid-width  (~length gw ((default-diaflow-block-width))))
    (define grid-height (~length gh grid-width))
    (define scale (make-rectangular ts (* ts (/ grid-width grid-height))))
    
    (make-gomamon
     #:id id #:at home
     #:T-scale scale #:U-scale scale
     grid-width grid-height)))

(define dia-singletion-path : (-> Gomamon)
  (let ([&path : (Boxof (Option Gomamon)) (box #false)])
    (lambda []
      (unless (unbox &path)
        (set-box! &path
                  (dia-initial-path 'diaflow:singleton:ghostcat 1.0 1.0 1.0 0.0)))

      (assert (unbox &path)))))
