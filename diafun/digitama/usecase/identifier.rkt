#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/track/anchor)
(require geofun/digitama/path/label)

(require "../block/dc.rkt")
(require "../block/interface.rkt")
(require "../track/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-uc-block-identify : (Dia-Block-Identifier UC-Block-Style UC-Block-Metadata)
  (lambda [anchor text size]
    (if (symbol? anchor)
        (uc-block-info anchor text default-uc-case-style)
        (uc-block-info anchor text default-uc-actor-style))))

(define default-uc-track-identify : (Dia-Track-Identifier UC-Track-Style)
  (lambda [source target labels extra-info]
    (cond [(dia:block-diff-type? source target) (uc-track-adjust source target labels default-uc~association~style)]
          [(dia:block-typeof? source uc-actor-style?) (uc-track-adjust source target labels default-uc~generalization~style)]
          [(geo-path-match-any-label? labels #px"(?i:(<<|«)?include(»|>>)?)") (uc-track-adjust source target labels default-uc~include~style)]
          [(geo-path-match-any-label? labels #px"(?i:(<<|«)?extend(»|>>)?)") (uc-track-adjust source target labels default-uc~extend~style)]
          [else (uc-track-adjust source target labels default-uc~generalization~style)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define uc-block-info : (->* (Geo-Anchor-Name String (-> (Dia-Block-Style UC-Block-Style)))
                             (UC-Block-Metadata)
                             (Dia-Block-Info UC-Block-Style UC-Block-Metadata))
  (lambda [anchor text mk-style [datum #false]]
    ((inst dia-block-info UC-Block-Style UC-Block-Metadata) anchor text mk-style default-uc-block-theme-adjuster datum)))

(define uc-track-adjust : (-> Dia:Block (Option Dia:Block) (Listof Geo-Path-Label-Datum) (-> (Dia-Track-Style UC-Track-Style))
                              (Option (Dia-Track-Style UC-Track-Style)))
  (lambda [source target label mk-style]
    ((inst dia-track-theme-adjust UC-Track-Style Dia:Block (Option Dia:Block)) source target label mk-style default-uc-track-theme-adjuster)))
