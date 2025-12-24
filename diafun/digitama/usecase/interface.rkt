#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/path/label)

(require "../block/dc.rkt")
(require "../interface.rkt")

(require "block.rkt")
(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-identify : Dia-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (not (eq? (string-ref text 0) #\.))
         (if (symbol? anchor)
             (dia-block-info anchor text (default-diauc-ucase-style-make) make-diauc-ucase-style)
             (dia-block-info anchor text (default-diauc-actor-style-make) make-diauc-actor-style)))))

(define default-diauc-track-identify : Dia-Track-Identifier
  (lambda [source target labels extra-info]
    (define stype : Symbol (dia:block-type source))
    (define ttype : (Option Symbol) (and target (dia:block-type target)))
    (define hints : (Listof Bytes) (geo-path-label-flatten labels))

    (define track-style : Dia-Track-Style
      (cond [(eq? stype ttype)
             (cond [(eq? stype 'Actor)
                    (dia-track-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)]
                   [(geo-path-label-match? hints #px"[Ii][Nn][Cc][Ll][Uu][Dd][Ee]")
                    (dia-track-style-construct source target labels (default-diauc-include-arrow-style-make) make-diauc-include-arrow-style)]
                   [(geo-path-label-match? hints #px"[Ee][Xx][Tt][Ee][Nn][Dd]")
                    (dia-track-style-construct source target labels (default-diauc-extend-arrow-style-make) make-diauc-extend-arrow-style)]
                   [else (dia-track-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)])]
            [else (dia-track-style-construct source target labels (default-diauc-association-arrow-style-make) make-diauc-association-arrow-style)]))

    track-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-fallback-construct : Dia-Anchor->Block
  (lambda [id brief style width height direction subtype]
    (cond [(diauc-ucase-style? style) (diauc-block-ucase id brief style width height direction subtype)]
          [(diauc-actor-style? style) (diauc-block-actor id brief style width height direction subtype)])))
