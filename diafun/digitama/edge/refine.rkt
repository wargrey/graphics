#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/geometry/anchor)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/layer/type)

(require "../node/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: deal with curved prints
(define dia-2-tracks-relocate-source : (-> (GLayerof Geo) Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print)
  (lambda [source a:track b:track]
    (define A : Float-Complex (geo-path-clean-print-position a:track))
    (define B : Float-Complex (geo-path-clean-print-position b:track))
    
    (cons (car b:track)
          (or (dia-line-node-intersect source A B A)
              A))))

(define dia-2-tracks-relocate-target : (-> (GLayerof Geo) Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print)
  (lambda [target a:track b:track]
    (define A : Float-Complex (geo-path-clean-print-position a:track))
    (define B : Float-Complex (geo-path-clean-print-position b:track))

    (cons (car b:track)
          (or (dia-line-node-intersect target A B B)
              B))))

(define dia-2-tracks-relocate-endpoints : (-> (Option (GLayerof Geo)) (Option (GLayerof Geo)) (List Geo-Path-Clean-Print Geo-Path-Clean-Print)
                                              (List Geo-Path-Clean-Print Geo-Path-Clean-Print))
  (lambda [source target tracks]
    (define a:track : Geo-Path-Clean-Print (car tracks))
    (define b:track : Geo-Path-Clean-Print (cadr tracks))
    
    (list (if (not source) a:track (dia-2-tracks-relocate-source source a:track b:track))
          (if (not target) b:track (dia-2-tracks-relocate-target target a:track b:track)))))

(define dia-more-tracks-relocate-endpoints : (-> (GLayerof Geo) (Option (GLayerof Geo))
                                                 (List* Geo-Path-Clean-Print Geo-Path-Clean-Print Geo-Path-Clean-Print (Listof Geo-Path-Clean-Print))
                                                 Geo-Path-Clean-Prints)
  (lambda [source target tracks]
    (define h1st : Geo-Path-Clean-Print (car tracks))
    (define h2nd : Geo-Path-Clean-Print (cadr tracks))
    (define re:head : Geo-Path-Clean-Print (dia-2-tracks-relocate-source source h1st h2nd))
    
    (let relocate ([t2nd : Geo-Path-Clean-Print h2nd]
                   [t1st : Geo-Path-Clean-Print (caddr tracks)]
                   [skcart : (Listof Geo-Path-Clean-Print) (list h2nd)]
                   [tracks : (Listof Geo-Path-Clean-Print) (cdddr tracks)])
      (if (null? tracks)
          (let-values ([(re:tail) (if (not target) t1st (dia-2-tracks-relocate-target target t2nd t1st))]
                       [(body) (assert (reverse skcart) pair?)])
            (list* re:head (car body) (append (cdr body) (list re:tail))))
          (relocate t1st (car tracks) (cons t1st skcart) (cdr tracks))))))
