#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/richtext/self)

(require "interface.rkt")
(require "../marker/self.rkt")
(require "../marker/guard.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-pin-angle : (-> (U Real (-> Real (Option Real))) (U Real (-> Real (Option Real)))
                                      Plot-Visualizer-Label-Placement
                                      (-> Real (Option Real)))
  (lambda [df/dx ddf/dxx placement]
    (λ [[x : Real]]
      (let ([+k (if (real? df/dx) df/dx (df/dx x))])
        (and +k
             (let ([c (if (real? ddf/dxx) ddf/dxx (or (ddf/dxx x) 0))]
                   [-k (- +k)])
               ;;; NOTE
               ; the facility that draws the pin line has no knownledge about the cartesian system,
               ; so that the resulting angle should be symmetrical about the x-axis
               ; if the angle is computed mathematically.
               (- (cond [(eq? placement 'auto) (if (>= c 0) (atan -1 +k) (atan +1 -k))]
                        [(eq? placement 'flip) (if (>= c 0) (atan +1 -k) (atan -1 +k))]
                        [(eq? placement 'left) (atan -1 +k)]
                        [else                  (atan +1 -k)]))))))))

(define plot-function-mark-guard : (-> (-> Real Real) (U False Geo-Rich-Text Plot:Mark 'name)
                                       Positive-Fixnum Positive-Index Real Real Real Real
                                       Real (Pairof Real Real)
                                       (Option Plot:Mark))
  (lambda [fx label idx total xmin xmax ymin ymax at-frac frac-rng]
    (cond [(plot:mark? label)
           (let* ([origin (plot:mark-point label)]
                  [dot (plot-mark-point-guard origin fx idx total xmin xmax ymin ymax at-frac frac-rng)])
             (and dot (cond [(and (= origin dot)) label]
                            [else (remake-plot:mark label #:point dot)])))]
          [(or label)
           (let ([dot (plot-mark-point-guard +nan.0 fx idx total xmin xmax ymin ymax at-frac frac-rng)])
             (and dot (let ([func-name (if (eq? label 'name) (format "~a" (object-name fx)) label)])
                        (plot-label func-name #:at dot))))]
          [else #false])))
