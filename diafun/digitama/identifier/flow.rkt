#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/geometry/anchor)

(require "../style/flow.rkt")
(require "../interface/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-block-identify : DiaFlow-Block-Identifier
  (lambda [anchor]
    (if (keyword? anchor)

        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (diaflow-node-style-construct (default-diaflow-canonical-start-name) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (diaflow-node-style-construct (default-diaflow-canonical-stop-name) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [else (diaflow-node-style-construct text anchor (default-diaflow-arrow-label-make) make-diaflow-arrow-label-style)]))

        (let ([text (geo-anchor->string anchor)])
          (define size (string-length text))
          (and (> size 0)
               (let* ([idx$ (- size 1)]
                      [idx$2 (- size 2)]
                      [ch0 (string-ref text 0)]
                      [ch$ (string-ref text idx$)])
                 (or (cond [(eq? ch0 #\^) ; also consider we are flowing a predicate function
                            (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                           [(eq? ch$ #\?)
                            (diaflow-node-style-construct text anchor (default-diaflow-decision-style-make) make-diaflow-decision-style)]
                           [(eq? ch$ #\!)
                            (diaflow-node-style-construct text anchor (default-diaflow-preparation-style-make) make-diaflow-preparation-style)]
                           [(eq? ch$ #\$)
                            (diaflow-node-style-construct (substring text 0 idx$) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                           [(eq? ch0 #\>)
                            (and (string-prefix? text ">>")
                                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-input-style-make) make-diaflow-input-style))]
                           [(eq? ch$ #\<)
                            (and (string-suffix? text "<<")
                                 (diaflow-node-style-construct (substring text 0 idx$2) anchor (default-diaflow-output-style-make) make-diaflow-output-style))]
                           [(eq? ch0 #\/)
                            (and (string-prefix? text "//")
                                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-comment-style-make) make-diaflow-comment-style))]
                           [(eq? ch0 #\-)
                            (and (string-prefix? text "->")
                                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-subroutine-style-make) make-diaflow-subroutine-style))]
                           [(eq? ch0 #\@)
                            (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-inspection-style-make) make-diaflow-inspection-style)]
                           [(eq? ch0 #\&)
                            (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-reference-style-make) make-diaflow-reference-style)]
                           [else #false])
                     (diaflow-node-style-construct text anchor (default-diaflow-process-style-make) make-diaflow-process-style))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) diaflow-node-style-construct : (-> String Geo-Anchor-Name (Option (Dia-Node-Style-Make* S)) (-> S) (Pairof Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (define-values (node-key style) (dia-node-style-construct text anchor mk-style mk-fallback-style))
    (cons node-key style)))
