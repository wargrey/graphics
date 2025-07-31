#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/path/label)

(require "../node/dc.rkt")
(require "../path/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-identify : Dia-Path-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (not (eq? (string-ref text 0) #\.))
         (if (symbol? anchor)
             (dia-path-block-style-construct anchor text (default-diauc-ucase-style-make) make-diauc-ucase-style)
             (dia-path-block-style-construct anchor text (default-diauc-actor-style-make) make-diauc-actor-style)))))

(define default-diauc-arrow-identify : Dia-Path-Arrow-Identifier
  (lambda [source target labels extra-info]
    (define stype : Symbol (dia:node-type source))
    (define ttype : (Option Symbol) (and target (dia:node-type target)))
    (define hints : (Listof Bytes) (geo-path-label-flatten labels))

    (define edge-style : Dia-Edge-Style
      (cond [(eq? stype ttype)
             (cond [(eq? stype 'Actor)
                    (dia-edge-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)]
                   [(geo-path-label-match? hints #px"[Ii][Nn][Cc][Ll][Uu][Dd][Ee]")
                    (dia-edge-style-construct source target labels (default-diauc-include-arrow-style-make) make-diauc-include-arrow-style)]
                   [(geo-path-label-match? hints #px"[Ee][Xx][Tt][Ee][Nn][Dd]")
                    (dia-edge-style-construct source target labels (default-diauc-extend-arrow-style-make) make-diauc-extend-arrow-style)]
                   [else (dia-edge-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)])]
            [else (dia-edge-style-construct source target labels (default-diauc-association-arrow-style-make) make-diauc-association-arrow-style)]))

    edge-style))
