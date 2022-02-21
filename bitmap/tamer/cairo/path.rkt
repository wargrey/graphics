#lang typed/racket/base

(require bitmap/track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! cairo-example [100 #:at 25+25i #:turn-scale 0.25+0.50i] #:-
  (step-right-down 0.25 0.125)
  (step-right-up   0.25 0.125)
  (turn-right-down-left)
  (drift -0.5 '(-0.25-0.125i -0.25+0.125i))
  (close))


(module+ main
  (require bitmap/digitama/track)
  
  (reverse (track-footprints cairo-example))
  cairo-example)
