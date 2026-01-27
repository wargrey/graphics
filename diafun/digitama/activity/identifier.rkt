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
    (define stype : Symbol (dia:block-type source))
    (define ttype : (Option Symbol) (and target (dia:block-type target)))
    
    (define track-style : (Option (Dia-Track-Style Act-Track-Style))
      (cond [(eq? stype 'Decision)
             (cond [(geo-path-match-any-label? labels (default-act-success-decision-regexp))
                    (act-track-adjust source target labels default-act~success~style)]
                   [(geo-path-match-any-label? labels (default-act-failure-decision-regexp))
                    (act-track-adjust source target labels default-act~failure~style)]
                   [else (act-track-adjust source target labels default-act~decision~style)])]
            [(eq? stype 'Merge)
             (act-track-adjust source target labels default-act~decision~style)]
            [(or (eq? stype 'Storage) (eq? ttype 'Storage))
             (if (pair? labels)
                 (act-track-adjust source target labels default-act~storage~style)
                 (act-track-adjust source target labels default-act~line~style))]
            [(geo-path-match-any-label? labels (default-act-loop-label-regexp))
             (act-track-adjust source target labels default-act~loop~style)]
            [else (act-track-adjust source target labels default-act~line~style)]))

    (and track-style
         (if (or (eq? stype 'Alternate) (eq? ttype 'Alternate))
             (dia-track-swap-dash-style track-style 'long-dash)
             track-style))))

(define default-act-block-identify : (Dia-Block-Identifier Act-Block-Style Act-Block-Metadata)
  (lambda [anchor text size]
    (if (keyword? anchor)

        (cond [(or (string-ci=? text "home") (string-ci=? text "start") (string-ci=? text "begin") (string-ci=? text "initial"))
               (act-block-info anchor text default-act-initial-style)]
              [(or (string-ci=? text "end") (string-ci=? text "done") (string-ci=? text "terminate") (string-ci=? text "exit") (string-ci=? text "return"))
               (act-block-info anchor text default-act-final-style)]
              [else (act-block-text-identify anchor text size)])

        (act-block-text-identify anchor text size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define act-block-text-identify : (-> Geo-Anchor-Name String Positive-Index (Option (Dia-Block-Info Act-Block-Style Act-Block-Metadata)))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    ; check '^' before '?' for flowcharts of predicate functions
    (cond [(eq? ch0 #\^) (act-block-info anchor (substring text 1 size) default-act-initial-style)]
          [(eq? ch$ #\?) (act-block-info anchor text default-act-decision-style)]
          [(eq? ch$ #\$)
           (if (string-suffix? text "->$")
               (act-block-info anchor (substring text 0 (- idx$2 1)) default-act-flow-final-style)
               (act-block-info anchor (substring text 0 idx$) default-act-final-style))]
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
          #;[(eq? ch0 #\&)
           (if (eq? ch$ #\.)
               (act-block-info anchor (substring text 1 idx$) default-act-reference-style 'page-sink)
               (act-block-info anchor (substring text 1 size) default-act-reference-style 'page-root))]
          [(eq? ch$ #\.)
           (and (string-suffix? text "...")
                (act-block-info anchor (substring text 0 (- idx$2 1)) default-act-time-event-style))]
          [(eq? ch0 #\\) (act-block-info anchor (substring text 1 size) default-act-action-style)]
          [else (act-block-info anchor text default-act-action-style)])))

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
