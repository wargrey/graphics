#lang typed/racket/base

(provide (all-defined-out))

(require digimon/digitama/unsafe/release/ops)

(require geofun/digitama/self)
(require geofun/digitama/geometry/footprint)
(require geofun/digitama/layer/type)

(require "../block/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: deal with curved prints
(define dia-two-tracks-relocate-source : (-> (GLayerof Geo) GPath:Print GPath:Print (Option GPath:Print))
  (lambda [source a:track b:track]
    (define A : Float-Complex (gpp-clean-position a:track))
    (define B : Float-Complex (gpp-clean-position b:track))
    (define actual-ipt (dia-line-block-intersect source A B A))

    (and actual-ipt
         (gpp:point (gpath:datum-cmd b:track)
                    actual-ipt))))

(define dia-two-tracks-relocate-target : (-> (GLayerof Geo) GPath:Print GPath:Print (Option GPath:Print))
  (lambda [target a:track b:track]
    (define A : Float-Complex (gpp-clean-position a:track))
    (define B : Float-Complex (gpp-clean-position b:track))
    (define actual-ipt (dia-line-block-intersect target A B B))

    (and actual-ipt
         (gpp:point (gpath:datum-cmd b:track)
                    actual-ipt))))

(define dia-two-tracks-relocate-endpoints : (-> (Option (GLayerof Geo)) (Option (GLayerof Geo)) (List GPath:Print GPath:Print)
                                                (Option (Pairof (List GPath:Print GPath:Print) (Pairof Zero Zero))))
  (lambda [source target tracks]
    (define a:track : GPath:Print (car tracks))
    (define b:track : GPath:Print (cadr tracks))
    (define maybe-apt (if (not source) a:track (dia-two-tracks-relocate-source source a:track b:track)))
    (define maybe-bpt (if (not target) b:track (dia-two-tracks-relocate-target target a:track b:track)))

    (and maybe-apt maybe-bpt
         (cons (list maybe-apt maybe-bpt)
               (cons 0 0)))))

(define dia-more-tracks-relocate-endpoints : (-> (GLayerof Geo) (Option (GLayerof Geo))
                                                 (List* GPath:Print GPath:Print GPath:Print (Listof GPath:Print))
                                                 (Option (Pairof Geo-Path-Clean-Prints* (Pairof Index Index))))
  (lambda [source target tracks]
    (let relocate-head ([h1st : GPath:Print (car tracks)]
                        [h2nd : GPath:Print (cadr tracks)]
                        [rest : (Listof GPath:Print) (cddr tracks)]
                        [dropped-head-count : Index 0])
      (define maybe-head (dia-two-tracks-relocate-source source h1st h2nd))

      (cond [(not maybe-head) (and (pair? rest) (relocate-head h2nd (car rest) (cdr rest) (unsafe-idx+ dropped-head-count 1)))]
            [(not target) (cons (list* maybe-head h2nd rest) (cons dropped-head-count 0))]
            [else (let ([rtracks (reverse (list* maybe-head h2nd rest))])
                    (let relocate-tail ([t1st : GPath:Print (car rtracks)]
                                        [t2nd : GPath:Print (cadr rtracks)]
                                        [tser : (Listof GPath:Print) (cddr rtracks)]
                                        [dropped-tail-count : Index 0])
                      (define maybe-tail (dia-two-tracks-relocate-target target t2nd t1st))
                      
                      (cond [(not maybe-tail) (and (pair? tser) (relocate-tail t2nd (car tser) (cdr tser) (unsafe-idx+ dropped-tail-count 1)))]
                            [(null? tser) (cons (list t2nd maybe-tail) (cons dropped-head-count dropped-tail-count))]
                            [else (let ([clean-tracks (append (reverse tser) (list t2nd maybe-tail))])
                                    (and (pair? clean-tracks)
                                         (pair? (cdr clean-tracks))
                                         (cons clean-tracks (cons dropped-head-count dropped-tail-count))))])))]))))
