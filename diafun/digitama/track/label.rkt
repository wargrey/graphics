#lang typed/racket/base

(provide (all-defined-out))

(require digimon/digitama/unsafe/ops)

(require geofun/digitama/geometry/footprint)

(require geofun/digitama/dc/track)
(require geofun/digitama/track/self)
(require geofun/digitama/track/mult)
(require geofun/digitama/path/label)

(require "style.rkt")
(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Track-Label-Info (List Index Geo-Path-Label-Datum Nonnegative-Flonum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-label-info-filter : (-> Geo-Track-Infobase Geo-Path-Clean-Prints Index Index
                                          (Values (Listof Geo-Path-Label-Datum) (Listof Dia-Track-Label-Info)
                                                  (Listof Geo-Track-Info-Datum)))
  (lambda [infobase tracks dropped-head-count dropped-tail-count]
    (define idx$ : Index (unsafe-idx- (length tracks) dropped-tail-count))
    
    (let label-filter ([sofni : (Listof Dia-Track-Label-Info) null]
                       [slebal : (Listof Geo-Path-Label-Datum) null]
                       [extra-infos : (Listof Geo-Track-Info-Datum) null]
                       [orig-tracks : Geo-Path-Clean-Prints tracks]
                       [orig-src : (Option Float-Complex) #false]
                       [idx : Index 0])
      (if (and (pair? orig-tracks) (< idx idx$))

          (let-values ([(oself orest) (values (car orig-tracks) (cdr orig-tracks))])
            (cond [(eq? (gpath:datum-cmd oself) #\L)
                   (let* ([otarget (gpp-clean-position oself)]
                          [info (and orig-src (hash-ref infobase (cons orig-src otarget) (λ [] #false)))])
                     (define-values (maybe-label base-position maybe-mult extra++)
                       (if (geo:track:info? info)
                           (let ([labels (geo:track:info-labels info)]
                                 [t (geo:track:info-base-position info)]
                                 [mult (geo:track:info-multiplicity info)]
                                 [extra (geo:track:info-extra info)])
                             (if (null? extra)
                                 (values labels t mult extra-infos)
                                 (values labels t mult (append extra-infos extra))))
                           (values #false 0.0 #false extra-infos)))
                     
                     (define-values (label-info slabel++)
                       (if (or maybe-label)
                           (values (list (unsafe-idx- idx dropped-head-count) maybe-label base-position)
                                   (cons maybe-label slebal))
                           (values #false slebal)))
                     
                     (define mult-info : (Option Dia-Track-Label-Info)
                       (let ([labels (and maybe-mult
                                          (geo-track-multiplicities-map (geo:track:multiplicity-source maybe-mult)
                                                                        (geo:track:multiplicity-target maybe-mult)))])
                         (and labels
                              (list (unsafe-idx- idx dropped-head-count) labels
                                    (geo:track:multiplicity-base-position maybe-mult)))))
                     
                     (label-filter (cond [(and label-info mult-info) (list* label-info mult-info sofni)]
                                         [(and label-info) (cons label-info sofni)]
                                         [(and mult-info) (cons mult-info sofni)]
                                         [else sofni])
                                   slabel++ extra++ orest otarget
                                   (unsafe-idx+ idx 1)))]
                  
                  [(eq? (gpath:datum-cmd oself) #\M) (label-filter sofni slebal extra-infos orest (gpp-clean-position oself) idx)]
                          
                  ;;; TODO: deal with curves
                  [else (label-filter sofni slebal extra-infos orest orig-src (unsafe-idx+ idx 1))]))

          (values (reverse slebal) sofni extra-infos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-track-label-info->label : (-> (Dia-Track-Annotator S) (Dia-Track-Style-Spec S) (Listof Dia-Track-Label-Info) (Listof Geo:Path:Label))
  (lambda [make-label style-spec rinfos]
    (let info->label ([labels : (Listof Geo:Path:Label) null]
                      [sofni : (Listof Dia-Track-Label-Info) rinfos])
      (if (pair? sofni)
          (let* ([info (car sofni)]
                 [label (make-label (car info) (cadr info) (caddr info) style-spec)])
            (info->label (cond [(list? label) (append label labels)]
                               [(geo:path:label? label) (cons label labels)]
                               [else labels])
                         (cdr sofni)))
          labels))))
