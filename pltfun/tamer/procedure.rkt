#lang typed/racket/base

(require pltfun/procedure)
(require geofun/digitama/richtext/pexpr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(plt-procedure #false null)
(plt-procedure #false null #(=))
(plt-procedure #false #(C))
(plt-procedure "V - E + F = 2" #(V E F) #(=) #(8 12 6) 2)

(let* ([ds (list 1 2 3 4 5 6)]
       [result (apply + ds)])
  (plt-procedure #:body-fill 'LightGrey
                 (geo-scale (plt-procedure #:body-fill 'Grey
                                           '+
                                           (build-list (length ds)
                                                       (λ [[i : Index]] : Plt-Procedure-Label-Datum
                                                         (pexpr 'span (list "a" (pexpr 'sub (number->string (add1 i)))))))
                                           #(sum)
                                           ds result)
                            0.36)
                 #(λ list) #(sum)
                 (list '+ ds) result))
