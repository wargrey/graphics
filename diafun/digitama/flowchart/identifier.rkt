#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require geofun/digitama/path/label)
(require geofun/digitama/track/anchor)

(require "../block/dc.rkt")
(require "../block/interface.rkt")
(require "../track/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-track-identify : (Dia-Track-Identifier Flow-Track-Style)
  (lambda [source target labels extra-info]
    (define stype : Symbol (dia:block-type source))
    (define ttype : (Option Symbol) (and target (dia:block-type target)))
    
    (define track-style : (Option (Dia-Track-Style Flow-Track-Style))
      (cond [(eq? stype 'Decision)
             (cond [(geo-path-match-any-label? labels (default-flow-success-decision-regexp))
                    (flow-track-adjust source target labels default-flow~success~style)]
                   [(geo-path-match-any-label? labels (default-flow-failure-decision-regexp))
                    (flow-track-adjust source target labels default-flow~failure~style)]
                   [else (flow-track-adjust source target labels default-flow~decision~style)])]
            [(eq? stype 'Selection)
             (flow-track-adjust source target labels default-flow~decision~style)]
            [(or (eq? stype 'Storage) (eq? ttype 'Storage))
             (if (pair? labels)
                 (flow-track-adjust source target labels default-flow~storage~style)
                 (flow-track-adjust source target labels default-flow~line~style))]
            [(geo-path-match-any-label? labels (default-flow-loop-label-regexp))
             (flow-track-adjust source target labels default-flow~loop~style)]
            [else (flow-track-adjust source target labels default-flow~line~style)]))

    (and track-style
         (if (or (eq? stype 'Alternate) (eq? ttype 'Alternate))
             (dia-track-swap-dash-style track-style 'long-dash)
             track-style))))

(define default-flow-block-identify : (Dia-Block-Identifier Flow-Block-Style Flow-Block-Metadata)
  (lambda [anchor]
    (if (keyword? anchor)

        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start") (string-ci=? text "begin"))
                 (flow-block-info anchor text default-flow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "done") (string-ci=? text "terminate") (string-ci=? text "exit") (string-ci=? text "return"))
                 (flow-block-info anchor text default-flow-stop-style)]
                [else (let ([size (string-length text)])
                        (and (> size 0)
                             (diaflow-block-text-identify anchor text size)))]))

        (let* ([text (geo-anchor->string anchor)]
               [size (string-length text)])
          (and (> size 0)
               (diaflow-block-text-identify anchor text size))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-text-identify : (-> Geo-Anchor-Name String Positive-Index (Option (Dia-Block-Info Flow-Block-Style Flow-Block-Metadata)))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    ; check '^' before '?' for flowcharts of predicate functions
    (cond [(eq? ch0 #\^) (flow-block-info anchor (substring text 1 size) default-flow-start-style 'Start)]
          [(eq? ch$ #\?) (flow-block-info anchor text default-flow-decision-style)]
          [(eq? ch$ #\!) (flow-block-info anchor text default-flow-preparation-style)]
          [(eq? ch$ #\$) (flow-block-info anchor (substring text 0 idx$) default-flow-stop-style 'Stop)]
          [(eq? ch0 #\:) (flow-block-info anchor (substring text 1 size) default-flow-operation-style)]
          [(eq? ch0 #\λ)
           (cond [(eq? ch$ #\~) (flow-block-info anchor (substring text 1 idx$) default-flow-collation-style)]
                 [(eq? ch$ #\<) (flow-block-info anchor (substring text 1 idx$) default-flow-sort-style)]
                 [(eq? ch$ #\>) (flow-block-info anchor (substring text 1 idx$) default-flow-sort-style)]
                 [else (flow-block-info anchor (substring text 1 size) default-flow-process-style 'Predefined)])]
          [(eq? ch0 #\>)
           (cond [(string-prefix? text ">>:")
                  (flow-block-info anchor (substring text 3 size) default-flow-input-style 'user)]
                 [(string-prefix? text ">>")
                  (flow-block-info anchor (substring text 2 size) default-flow-input-style)]
                 [(eq? ch$ #\<)
                  (flow-block-info anchor (substring text 1 idx$) default-flow-collation-style)]
                 [else #false])]
          [(eq? ch0 #\<)
           (cond [(string-prefix? text "<<:")
                  (flow-block-info anchor (substring text 3 size) default-flow-output-style 'user)]
                 [(string-prefix? text "<<")
                  (flow-block-info anchor (substring text 2 size) default-flow-output-style)]
                 [(eq? ch$ #\>)
                  (flow-block-info anchor (substring text 1 idx$) default-flow-sort-style)]
                 [else #false])]
          [(eq? ch0 #\-)
           (cond [(eq? ch$ #\=) (flow-block-info anchor (substring text 1 idx$) default-flow-extract-style)]
                 [(eq? ch$ #\+) (flow-block-info anchor (substring text 1 size) default-flow-selection-style)]
                 [(string-prefix? text "--")
                  (if (string-suffix? text "--")
                      (flow-block-info anchor (substring text 2 idx$2) default-flow-process-style 'Alternate)
                      (flow-block-info anchor (substring text 2 size)  default-flow-process-style 'Alternate))]
                 [else #false])]
          [(eq? ch0 #\=)
           (cond [(eq? ch$ #\*) (flow-block-info anchor (substring text 1 idx$) default-flow-junction-style)]
                 [(eq? ch$ #\-) (flow-block-info anchor (substring text 1 idx$) default-flow-merge-style)]
                 [else #false])]
          [(eq? ch0 #\@)
           (if (eq? ch$ #\.)
               (flow-block-info anchor (substring text 1 idx$) default-flow-inspection-style 'sink)
               (flow-block-info anchor (substring text 1 size) default-flow-inspection-style 'root))]
          [(eq? ch0 #\&)
           (if (eq? ch$ #\.)
               (flow-block-info anchor (substring text 1 idx$) default-flow-reference-style 'page-sink)
               (flow-block-info anchor (substring text 1 size) default-flow-reference-style 'page-root))]
          [(eq? ch$ #\.)
           (and (string-suffix? text "...")
                (flow-block-info anchor (substring text 0 (- idx$2 1)) default-flow-delay-style))]
          [(eq? ch0 #\/)
           (cond [(string-prefix? text "/doc/")
                  (if (eq? ch$ #\/)
                      (flow-block-info anchor (substring text 5 idx$) default-flow-storage-style 'Directory)
                      (flow-block-info anchor (substring text 5 size) default-flow-file-style 'File))]
                 [(string-prefix? text "/db/")
                  (flow-block-info anchor (substring text 4 size) default-flow-database-style 'Database)]
                 [(string-prefix? text "/proc/")
                  (flow-block-info anchor (substring text 6 size) default-flow-storage-style 'Memory)]
                 [else (flow-block-info anchor (substring text 1 size) default-flow-storage-style)])]
          [(eq? ch0 #\.) #false]
          [(eq? ch0 #\\) (flow-block-info anchor (substring text 1 size) default-flow-process-style)]
          [else (flow-block-info anchor text default-flow-process-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-block-info : (->* (Geo-Anchor-Name String (-> (Dia-Block-Style Flow-Block-Style)))
                               (Flow-Block-Metadata)
                               (Dia-Block-Info Flow-Block-Style Flow-Block-Metadata))
  (lambda [anchor text mk-style [datum #false]]
    ((inst dia-block-info Flow-Block-Style Flow-Block-Metadata) anchor text mk-style default-flow-block-theme-adjuster datum)))

(define flow-track-adjust : (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Label-Datum) (-> (Dia-Track-Style Flow-Track-Style))
                              (Option (Dia-Track-Style Flow-Track-Style)))
  (lambda [source target label mk-style]
    ((inst dia-track-theme-adjust Flow-Track-Style Dia:Block (Option Dia:Block)) source target label mk-style default-flow-track-theme-adjuster)))
