#lang typed/racket/base

(provide (all-defined-out) Geo-Side-Choice)

(require digimon/struct)
(require digimon/measure)

(require geofun/digitama/unsafe/typed/cairo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration dz:dock : DZ:Dock
  #:format "default-dia-zone-~a"
  ([side : Geo-Side-Choice 't]
   [position : Flonum 0.5]
   [distance : Length+% (&: 0.618)]
   [stretch? : Boolean #true]))
