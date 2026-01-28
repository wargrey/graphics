#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/layer/type)

(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: deal with curved prints
(define dia-two-tracks-relocate-source : (-> (GLayerof Geo) GPath:Print GPath:Print GPath:Print)
  (lambda [source a:track b:track]
    (define A : Float-Complex (gpp-clean-position a:track))
    (define B : Float-Complex (gpp-clean-position b:track))
    
    (gpp:point (gpath:datum-cmd b:track)
               (or (dia-line-block-intersect source A B A)
                   A))))

(define dia-two-tracks-relocate-target : (-> (GLayerof Geo) GPath:Print GPath:Print GPath:Print)
  (lambda [target a:track b:track]
    (define A : Float-Complex (gpp-clean-position a:track))
    (define B : Float-Complex (gpp-clean-position b:track))

    (gpp:point (gpath:datum-cmd b:track)
               (or (dia-line-block-intersect target A B B)
                   B))))

(define dia-two-tracks-relocate-endpoints : (-> (Option (GLayerof Geo)) (Option (GLayerof Geo)) (List GPath:Print GPath:Print)
                                              (List GPath:Print GPath:Print))
  (lambda [source target tracks]
    (define a:track : GPath:Print (car tracks))
    (define b:track : GPath:Print (cadr tracks))
    
    (list (if (not source) a:track (dia-two-tracks-relocate-source source a:track b:track))
          (if (not target) b:track (dia-two-tracks-relocate-target target a:track b:track)))))

(define dia-more-tracks-relocate-endpoints : (-> (GLayerof Geo) (Option (GLayerof Geo))
                                                 (List* GPath:Print GPath:Print GPath:Print (Listof GPath:Print))
                                                 Geo-Path-Clean-Prints*)
  (lambda [source target tracks]
    (define h1st : GPath:Print (car tracks))
    (define h2nd : GPath:Print (cadr tracks))
    (define re:head : GPath:Print (dia-two-tracks-relocate-source source h1st h2nd))
    
    (let relocate ([t2nd : GPath:Print h2nd]
                   [t1st : GPath:Print (caddr tracks)]
                   [skcart : (Listof GPath:Print) (list h2nd)]
                   [tracks : (Listof GPath:Print) (cdddr tracks)])
      (if (null? tracks)
          (let-values ([(re:tail) (if (not target) t1st (dia-two-tracks-relocate-target target t2nd t1st))]
                       [(body) (assert (reverse skcart) pair?)])
            (list* re:head (car body) (append (cdr body) (list re:tail))))
          (relocate t1st (car tracks) (cons t1st skcart) (cdr tracks))))))
