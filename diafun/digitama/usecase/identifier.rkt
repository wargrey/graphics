#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)

(require geofun/digitama/geometry/anchor)

(require "../node/dc.rkt")
(require "../edge/label.rkt")
(require "../path/interface.rkt")

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-block-identify : Dia-Path-Block-Identifier
  (lambda [anchor]
    (define text (geo-anchor->string anchor))
    (define size (string-length text))
    
    (and (> size 0)
         (diauc-block-text-identify anchor text size))))

(define default-diauc-arrow-identify : Dia-Path-Arrow-Identifier
  (lambda [source target labels]
    (define stype : Symbol (dia:node-type source))
    (define ttype : (Option Symbol) (and target (dia:node-type target)))
    (define hints : (Listof Bytes) (dia-edge-label-flatten labels))

    (define edge-style : Dia-Edge-Style
      (cond [(eq? stype ttype)
             (cond [(eq? stype 'Actor)
                    (dia-edge-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)]
                   [(dia-edge-label-match? hints #px"[Ii][Nn][Cc][Ll][Uu][Dd][Ee]")
                    (dia-edge-style-construct source target labels (default-diauc-include-arrow-style-make) make-diauc-include-arrow-style)]
                   [(dia-edge-label-match? hints #px"[Ee][Xx][Tt][Ee][Nn][Dd]")
                    (dia-edge-style-construct source target labels (default-diauc-extend-arrow-style-make) make-diauc-extend-arrow-style)]
                   [else (dia-edge-style-construct source target labels (default-diauc-generalization-arrow-style-make) make-diauc-generalization-arrow-style)])]
            [(or ttype) (dia-edge-style-construct source target labels (default-diauc-association-arrow-style-make) make-diauc-association-arrow-style)]
            [else (dia-edge-style-construct source target labels (default-diauc-error-arrow-style-make) make-diauc-error-arrow-style)]))

    edge-style))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diauc-block-text-identify : (-> Geo-Anchor-Name String Positive-Index (Option Dia-Path-Block-Datum))
  (lambda [anchor text size]
    (define-values (idx$ idx$2) (values (- size 1) (- size 2)))
    (define-values (ch0 ch$) (values (string-ref text 0) (string-ref text idx$)))

    (cond [(eq? ch0 #\:) (dia-path-block-style-construct anchor (substring text 1 size) (default-diauc-actor-style-make) make-diauc-actor-style)]
          [(not (eq? ch0 #\.)) (dia-path-block-style-construct anchor text (default-diauc-ucase-style-make) make-diauc-ucase-style)]
          [else #false])))
