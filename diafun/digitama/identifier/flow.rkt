#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require geofun/digitama/geometry/anchor)

(require "../node/dc.rkt")
(require "../edge/label.rkt")
(require "../style/flow.rkt")
(require "../interface/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-block-identify : DiaFlow-Block-Identifier
  (lambda [anchor]
    (if (keyword? anchor)

        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (diaflow-node-style-construct anchor (default-diaflow-canonical-start-name) (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (diaflow-node-style-construct anchor (default-diaflow-canonical-stop-name) (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [else (diaflow-block-text-identify anchor text)]))

        (diaflow-block-text-identify anchor (geo-anchor->string anchor)))))

(define default-diaflow-arrow-identify : DiaFlow-Arrow-Identifier
  (lambda [source target labels]
    (define stype : Symbol (dia:node-type source))
    (define ttype : (Option Symbol) (and target (dia:node-type target)))
    (define hints : (Listof String) (dia-edge-label-flatten labels))

    (define edge-style : Dia-Edge-Style
      (cond [(eq? stype 'Decision)
             (cond [(dia-edge-label-match? hints (default-diaflow-success-decision-labels))
                    (dia-edge-style-construct source target labels (default-diaflow-success-arrow-style-make) make-diaflow-success-arrow-style)]
                   [(dia-edge-label-match? hints (default-diaflow-failure-decision-labels))
                    (dia-edge-style-construct source target labels (default-diaflow-failure-arrow-style-make) make-diaflow-failure-arrow-style)]
                   [else (dia-edge-style-construct source target labels (default-diaflow-decision-arrow-style-make) make-diaflow-decision-arrow-style)])]
            [(dia-edge-label-match? hints (default-diaflow-loop-label-regexp))
             (dia-edge-style-construct source target labels (default-diaflow-loop-arrow-style-make) make-diaflow-loop-arrow-style)]
            [else (dia-edge-style-construct source target labels (default-diaflow-arrow-style-make) make-diaflow-arrow-style)]))

    (if (or (eq? stype 'Alternate) (eq? ttype 'Alternate))
        (dia-edge-swap-dash-style edge-style 'long-dash)
        edge-style)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
; `~text`   for collate (data filter)
; `text...` for delay
(define diaflow-block-text-identify : (-> Geo-Anchor-Name String (Option DiaFlow-Block-Datum))
  (lambda [anchor text]
    (define size (string-length text))
    
    (and (> size 0)
         (let* ([idx$ (- size 1)]
                [idx$2 (- size 2)]
                [ch0 (string-ref text 0)]
                [ch$ (string-ref text idx$)])
           (or (cond [(eq? ch0 #\^) ; also, check it before #\? for flowcharts of predicate functions
                      (diaflow-node-style-construct anchor (substring text 1 size) (default-diaflow-start-style-make) make-diaflow-start-style 'Start)]
                     [(eq? ch$ #\?)
                      (diaflow-node-style-construct anchor text (default-diaflow-decision-style-make) make-diaflow-decision-style)]
                     [(eq? ch$ #\!)
                      (diaflow-node-style-construct anchor text (default-diaflow-preparation-style-make) make-diaflow-preparation-style)]
                     [(eq? ch$ #\$)
                      (diaflow-node-style-construct anchor (substring text 0 idx$) (default-diaflow-stop-style-make) make-diaflow-stop-style 'Stop)]
                     [(eq? ch0 #\>)
                      (cond [(string-prefix? text ">>:")
                             (diaflow-node-style-construct anchor (substring text 3 size) (default-diaflow-input-style-make) make-diaflow-input-style 'manual)]
                            [(string-prefix? text ">>")
                             (diaflow-node-style-construct anchor (substring text 2 size) (default-diaflow-input-style-make) make-diaflow-input-style)]
                            [else #false])]
                     [(eq? ch$ #\<)
                      (and (string-suffix? text "<<")
                           (diaflow-node-style-construct anchor (substring text 0 idx$2) (default-diaflow-output-style-make) make-diaflow-output-style))]
                     [(eq? ch0 #\λ)
                      (diaflow-node-style-construct anchor (substring text 1 size) (default-diaflow-prefab-style-make) make-diaflow-prefab-style)]
                     [(eq? ch0 #\-)
                      (and (string-prefix? text "--")
                           (if (string-suffix? text "--")
                               (diaflow-node-style-construct anchor (substring text 2 idx$2) (default-diaflow-alternate-style-make) make-diaflow-alternate-style)
                               (diaflow-node-style-construct anchor (substring text 2 size)  (default-diaflow-alternate-style-make) make-diaflow-alternate-style)))]
                     [(eq? ch0 #\@)
                      (if (eq? ch$ #\.)
                          (diaflow-node-style-construct anchor (substring text 1 idx$) (default-diaflow-inspection-style-make) make-diaflow-inspection-style 'sink)
                          (diaflow-node-style-construct anchor (substring text 1 size) (default-diaflow-inspection-style-make) make-diaflow-inspection-style 'root))]
                     [(eq? ch0 #\&)
                      (if (eq? ch$ #\.)
                          (diaflow-node-style-construct anchor (substring text 1 idx$) (default-diaflow-reference-style-make) make-diaflow-reference-style 'sink)
                          (diaflow-node-style-construct anchor (substring text 1 size) (default-diaflow-reference-style-make) make-diaflow-reference-style 'root))]
                     [(eq? ch0 #\:)
                      (diaflow-node-style-construct anchor (substring text 1 size) (default-diaflow-operation-style-make) make-diaflow-operation-style)]
                     [else #false])
               (and (not (eq? ch0 #\.))
                    (diaflow-node-style-construct anchor text (default-diaflow-process-style-make) make-diaflow-process-style)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) diaflow-node-style-construct : (->* (Geo-Anchor-Name String (Option (DiaFlow-Node-Style-Make (∩ S Dia-Node-Style))) (-> (∩ S Dia-Node-Style)))
                                                         ((Option Symbol))
                                                         DiaFlow-Block-Datum)
  (lambda [anchor text mk-style mk-fallback-style [hint #false]]
    (list text (dia-node-style-construct anchor mk-style mk-fallback-style hint) hint)))
