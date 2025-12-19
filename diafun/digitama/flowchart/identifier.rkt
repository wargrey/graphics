#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/path/label)
(require geofun/digitama/geometry/anchor)

(require "../block/dc.rkt")
(require "../track/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-block-identify : Dia-Block-Identifier
  (lambda [anchor]
    (if (keyword? anchor)

        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start") (string-ci=? text "begin"))
                 (dia-block-info anchor (default-diaflow-canonical-start-name) (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "done") (string-ci=? text "terminate") (string-ci=? text "exit") (string-ci=? text "return"))
                 (dia-block-info anchor text (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [else (let ([size (string-length text)])
                        (and (> size 0)
                             (diaflow-block-text-identify anchor text size)))]))

        (let* ([text (geo-anchor->string anchor)]
               [size (string-length text)])
          (and (> size 0)
               (diaflow-block-text-identify anchor text size))))))

(define default-diaflow-track-identify : Dia-Track-Identifier
  (lambda [source target labels extra-info]
    (define stype : Symbol (dia:block-type source))
    (define ttype : (Option Symbol) (and target (dia:block-type target)))
    (define hints : (Listof Bytes) (geo-path-label-flatten labels))

    (define track-style : Dia-Track-Style
      (cond [(eq? stype 'Decision)
             (cond [(geo-path-label-match? hints (default-diaflow-success-decision-regexp))
                    (dia-track-style-construct source target labels (default-diaflow-success-arrow-style-make) make-diaflow-success-arrow-style)]
                   [(geo-path-label-match? hints (default-diaflow-failure-decision-regexp))
                    (dia-track-style-construct source target labels (default-diaflow-failure-arrow-style-make) make-diaflow-failure-arrow-style)]
                   [else (dia-track-style-construct source target labels (default-diaflow-decision-arrow-style-make) make-diaflow-decision-arrow-style)])]
            [(eq? stype 'Selection)
             (dia-track-style-construct source target labels (default-diaflow-decision-arrow-style-make) make-diaflow-decision-arrow-style)]
            [(or (eq? stype 'Storage) (eq? ttype 'Storage))
             (if (pair? labels)
                 (dia-track-style-construct source target labels (default-diaflow-storage-arrow-style-make) make-diaflow-storage-arrow-style)
                 (dia-track-style-construct source target labels (default-diaflow-arrow-style-make) make-diaflow-arrow-style))]
            [(geo-path-label-match? hints (default-diaflow-loop-label-regexp))
             (dia-track-style-construct source target labels (default-diaflow-loop-arrow-style-make) make-diaflow-loop-arrow-style)]
            [else (dia-track-style-construct source target labels (default-diaflow-arrow-style-make) make-diaflow-arrow-style)]))

    (if (or (eq? stype 'Alternate) (eq? ttype 'Alternate))
        (dia-track-swap-dash-style track-style 'long-dash)
        track-style)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-text-identify : (-> Geo-Anchor-Name String Positive-Index (Option Dia-Block-Info))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    ; check '^' before '?' for flowcharts of predicate functions
    (cond [(eq? ch0 #\^) (dia-block-info anchor (substring text 1 size) (default-diaflow-start-style-make) make-diaflow-start-style 'Start)]
          [(eq? ch$ #\?) (dia-block-info anchor text (default-diaflow-decision-style-make) make-diaflow-decision-style)]
          [(eq? ch$ #\!) (dia-block-info anchor text (default-diaflow-preparation-style-make) make-diaflow-preparation-style)]
          [(eq? ch$ #\$) (dia-block-info anchor (substring text 0 idx$) (default-diaflow-stop-style-make) make-diaflow-stop-style 'Stop)]
          [(eq? ch0 #\:) (dia-block-info anchor (substring text 1 size) (default-diaflow-operation-style-make) make-diaflow-operation-style)]
          [(eq? ch0 #\λ)
           (cond [(eq? ch$ #\~) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-collation-style-make) make-diaflow-collation-style)]
                 [(eq? ch$ #\<) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-sort-style-make) make-diaflow-sort-style)]
                 [(eq? ch$ #\>) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-sort-style-make) make-diaflow-sort-style)]
                 [else (dia-block-info anchor (substring text 1 size) (default-diaflow-prefab-style-make) make-diaflow-process-style 'Predefined)])]
          [(eq? ch0 #\>)
           (cond [(string-prefix? text ">>:")
                  (dia-block-info anchor (substring text 3 size) (default-diaflow-keyboard-style-make) make-diaflow-input-style 'user)]
                 [(string-prefix? text ">>")
                  (dia-block-info anchor (substring text 2 size) (default-diaflow-input-style-make) make-diaflow-input-style)]
                 [(eq? ch$ #\<)
                  (dia-block-info anchor (substring text 1 idx$) (default-diaflow-collation-style-make) make-diaflow-collation-style)]
                 [else #false])]
          [(eq? ch0 #\<)
           (cond [(string-prefix? text "<<:")
                  (dia-block-info anchor (substring text 3 size) (default-diaflow-display-style-make) make-diaflow-output-style 'user)]
                 [(string-prefix? text "<<")
                  (dia-block-info anchor (substring text 2 size) (default-diaflow-output-style-make) make-diaflow-output-style)]
                 [(eq? ch$ #\>)
                  (dia-block-info anchor (substring text 1 idx$) (default-diaflow-sort-style-make) make-diaflow-sort-style)]
                 [else #false])]
          [(eq? ch0 #\-)
           (cond [(eq? ch$ #\=) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-extract-style-make)   make-diaflow-extract-style)]
                 [(eq? ch$ #\+) (dia-block-info anchor (substring text 1 size) (default-diaflow-selection-style-make) make-diaflow-selection-style)]
                 [(string-prefix? text "--")
                  (if (string-suffix? text "--")
                      (dia-block-info anchor (substring text 2 idx$2) (default-diaflow-alternate-style-make) make-diaflow-process-style 'Alternate)
                      (dia-block-info anchor (substring text 2 size)  (default-diaflow-alternate-style-make) make-diaflow-process-style 'Alternate))]
                 [else #false])]
          [(eq? ch0 #\=)
           (cond [(eq? ch$ #\*) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-junction-style-make) make-diaflow-junction-style)]
                 [(eq? ch$ #\-) (dia-block-info anchor (substring text 1 idx$) (default-diaflow-merge-style-make) make-diaflow-merge-style)]
                 [else #false])]
          [(eq? ch0 #\@)
           (if (eq? ch$ #\.)
               (dia-block-info anchor (substring text 1 idx$) (default-diaflow-inspection-style-make) make-diaflow-inspection-style 'sink)
               (dia-block-info anchor (substring text 1 size) (default-diaflow-inspection-style-make) make-diaflow-inspection-style 'root))]
          [(eq? ch0 #\&)
           (if (eq? ch$ #\.)
               (dia-block-info anchor (substring text 1 idx$) (default-diaflow-reference-style-make) make-diaflow-reference-style 'page-sink)
               (dia-block-info anchor (substring text 1 size) (default-diaflow-reference-style-make) make-diaflow-reference-style 'page-root))]
          [(eq? ch$ #\.)
           (and (string-suffix? text "...")
                (dia-block-info anchor (substring text 0 (- idx$2 1)) (default-diaflow-delay-style-make) make-diaflow-delay-style))]
          [(eq? ch0 #\/)
           (cond [(string-prefix? text "/doc/")
                  (if (eq? ch$ #\/)
                      (dia-block-info anchor (substring text 5 idx$) (default-diaflow-document-style-make) make-diaflow-storage-style 'Directory)
                      (dia-block-info anchor (substring text 5 size) (default-diaflow-document-style-make) make-diaflow-storage-style 'File))]
                 [(string-prefix? text "/db/")
                  (dia-block-info anchor (substring text 4 size) (default-diaflow-database-style-make) make-diaflow-database-style 'Database)]
                 [(string-prefix? text "/proc/")
                  (dia-block-info anchor (substring text 6 size) (default-diaflow-memory-style-make) make-diaflow-storage-style 'Memory)]
                 [else (dia-block-info anchor (substring text 1 size) (default-diaflow-storage-style-make) make-diaflow-storage-style)])]
          [(not (eq? ch0 #\.)) (dia-block-info anchor text (default-diaflow-process-style-make) make-diaflow-process-style)]
          [else #false])))
