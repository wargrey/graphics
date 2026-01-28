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
(define default-act-track-identify : (Dia-Track-Identifier Act-Track-Style)
  (lambda [source target labels extra-info]
    (cond [(or (dia:block-typeof? source act-object-style?)
               (dia:block*-typeof? target act-object-style?))
           (act-track-adjust source target labels default-act~object~flow~style)]
          [(dia:block-typeof? source act-decision-style?) (act-track-adjust source target labels default-act~decision~style)]
          [(dia:block-typeof? source act-merge-style?) (act-track-adjust source target labels default-act~decision~style)]
          [(dia:block-typeof? source act-fork-style?) (act-track-adjust source target labels default-act~parallel~style)]
          [else (act-track-adjust source target labels default-act~control~flow~style)])))

(define default-act-block-identify : (Dia-Block-Identifier Act-Block-Style Act-Block-Metadata)
  (lambda [anchor text size]
    (if (keyword? anchor)

        (cond [(eq? anchor '#:home) (act-block-info anchor text default-act-initial-style)]
              [else (act-object-text-identify anchor text size)])

        (act-control-text-identify anchor text size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define act-control-text-identify : (-> Symbol String Positive-Index (Option (Dia-Block-Info Act-Block-Style Act-Block-Metadata)))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    ; check '^' before '?' for flowcharts of predicate functions
    (cond [(eq? ch0 #\^) (act-block-info anchor (substring text 1 size) default-act-initial-style)]
          [(eq? ch0 #\:) (act-block-info anchor (substring text 1 size) default-act-object-style)]
          [(eq? ch$ #\?) (act-block-info anchor text default-act-decision-style)]
          [(eq? ch$ #\$)
           (if (string-suffix? text "~$")
               (act-block-info anchor (substring text 0 (- idx$2 1)) default-act-flow-final-style 'Flow)
               (act-block-info anchor (substring text 0 idx$) default-act-final-style 'Activity))]
          [(eq? ch0 #\λ) (act-block-info anchor (substring text 1 size) default-act-action-style 'Call)]
          [(eq? ch0 #\-)
           (cond [(eq? ch$ #\=) (act-block-info anchor (substring text 1 idx$) default-act-fork-style)]
                 [(eq? ch$ #\+) (act-block-info anchor (substring text 1 size) default-act-decision-style)]
                 [else #false])]
          [(eq? ch0 #\=) (and (eq? ch$ #\-) (act-block-info anchor (substring text 1 idx$) default-act-join-style))]
          [(eq? ch0 #\+)
           (cond [(eq? ch$ #\+) (act-block-info anchor (substring text 1 idx$) default-act-merge-style)]
                 [(eq? ch$ #\-) (act-block-info anchor (substring text 1 idx$) default-act-merge-style)]
                 [else #false])]
          [(eq? ch0 #\@)
           (if (eq? ch$ #\.)
               (act-block-info anchor (substring text 1 idx$) default-act-connector-style 'sink)
               (act-block-info anchor (substring text 1 size) default-act-connector-style 'root))]
          [(eq? ch0 #\&)
           (if (eq? ch$ #\.)
               (act-block-info anchor (substring text 1 idx$) default-act-signal-style 'page-sink)
               (act-block-info anchor (substring text 1 size) default-act-signal-style 'page-root))]
          [(eq? ch$ #\.)
           (and (string-suffix? text "...")
                (act-block-info anchor (substring text 0 (- idx$2 1)) default-act-time-event-style))]
          [(eq? ch0 #\\) (act-block-info anchor (substring text 1 size) default-act-action-style)]
          [else (act-block-info anchor text default-act-action-style)])))

(define act-object-text-identify : (-> Keyword String Positive-Index (Option (Dia-Block-Info Act-Block-Style Act-Block-Metadata)))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    (cond [(eq? ch0 #\/)
           (cond [(string-prefix? text "/doc/")
                  (if (eq? ch$ #\/)
                      (act-block-info anchor (substring text 5 idx$) default-act-central-buffer-style 'Directory)
                      (act-block-info anchor (substring text 5 size) default-act-central-buffer-style 'File))]
                 [(string-prefix? text "/db/")
                  (act-block-info anchor (substring text 4 size) default-act-central-buffer-style 'Database)]
                 [(string-prefix? text "/proc/")
                  (act-block-info anchor (substring text 6 size) default-act-central-buffer-style)]
                 [else (act-block-info anchor (substring text 1 size) default-act-central-buffer-style)])]
          [else (act-block-info anchor text default-act-object-style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define act-block-info : (->* (Geo-Anchor-Name String (-> (Dia-Block-Style Act-Block-Style)))
                              (Act-Block-Metadata)
                              (Dia-Block-Info Act-Block-Style Act-Block-Metadata))
  (lambda [anchor text mk-style [datum #false]]
    ((inst dia-block-info Act-Block-Style Act-Block-Metadata) anchor text mk-style default-act-block-theme-adjuster datum)))

(define act-track-adjust : (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Label-Datum) (-> (Dia-Track-Style Act-Track-Style))
                              (Option (Dia-Track-Style Act-Track-Style)))
  (lambda [source target label mk-style]
    ((inst dia-track-theme-adjust Act-Track-Style Dia:Block (Option Dia:Block)) source target label mk-style default-act-track-theme-adjuster)))
