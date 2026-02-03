#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/font)

(require geofun/digitama/self)
(require geofun/digitama/dc/path)
(require geofun/digitama/track/anchor)

(require geofun/digitama/geometry/footprint)

(require "self.rkt")

(require "../../block/dc.rkt")
(require "../../block/style.rkt")

(require "../../track/style.rkt")
(require "../../track/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-note-track-realize : (-> (Option Dia:Block) (Option Dia:Block) (Pairof Geo-Path-Clean-Prints* (Pairof Index Index))
                                     Dia-Note-Factory (Option Nonnegative-Flonum)
                                     (Option Geo-Path))
  (lambda [source target retracks note-factory opacity]
    (define style-self : (Option (Dia-Track-Style Dia-Note-Track-Style))
      (cond [(and source target) (dia-note-track-adjust source target default-dia~block~note~style)]
            [(dia:block:note? target)
             (if (not source)
                 (dia-note-track-adjust target source default-dia~track~note~style)
                 (dia-note-track-adjust source target default-dia~block~note~style))]
            [(dia:block:note? source)
             (if (not target)
                 (dia-note-track-adjust source target default-dia~track~note~style)
                 (dia-note-track-adjust source target default-dia~block~note~style))]
            [else #false]))

    (and style-self
         (let* ([backstop-style ((dia-note-factory-λtrack-backstop-style note-factory))]
                [style-spec ((inst make-dia-track-style-spec Dia-Note-Track-Style) #:custom style-self #:backstop backstop-style #:opacity opacity)]
                [path (dia-note-track-build source target (car retracks) style-spec)])
           (and (geo? path) path)))))

(define dia-note-block-realize : (-> Geo-Anchor-Name String Positive-Index Dia-Note-Factory (Option Dia-Note-Describer)
                                     (Option Flonum) (Option Nonnegative-Flonum) (Option Nonnegative-Flonum)
                                     (Option Dia:Block:Note))
  (lambda [anchor text size note-factory note-desc direction scale opacity]
    (define make-note (dia-note-factory-builder note-factory))
    
    (and make-note
         (let*-values ([(text stereotype) (dia-block-caption-split-for-stereotype (substring text 2 size))]
                       [(note-info) (dia-note-block-info anchor text make-dia-note-block-style stereotype)]
                       [(text metadata style) (values (car note-info) (cadr note-info) (caddr note-info))])
           (and style
                (let* ([backstop-style ((dia-note-factory-λblock-backstop-style note-factory))]
                       [typeset (or (dia-note-factory-typesetter note-factory) dia-note-typeset)]
                       [style-spec ((inst make-dia-block-style-spec Dia-Note-Block-Style) #:custom style #:backstop backstop-style #:scale scale #:opacity opacity)])
                  (parameterize ([default-font-metrics (λ [[unit : Font-Unit]] (font-metrics-ref (dia-block-resolve-font style-spec) unit))])
                    (define-values (max-width max-height) (dia-block-resolve-size/inf style-spec))
                    
                    (let* ([id (geo-anchor->symbol anchor)]
                           [maybe-desc (cond [(not text) #false]
                                             [(not note-desc) (void)]
                                             [(hash? note-desc) (hash-ref note-desc anchor void)]
                                             [else (note-desc anchor text style-spec metadata)])]
                           [body (and maybe-desc (typeset id (if (void? maybe-desc) text maybe-desc) style-spec max-width max-height))])
                      (and body (make-note id body style-spec (dia-block-resolve-padding style-spec) direction metadata))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-note-typeset : Dia-Note-Typesetter
  (lambda [id text style max-width max-height]
    (dia-block-text->caption #:id id #:alignment 'left #:trim? #false
                             #:max-width max-width #:max-height max-height
                             text style)))

(define dia-note-track-build : (Dia-Track-Builder Dia-Note-Track-Style)
  (lambda [source target tracks style]
    (define-values (stip ttip) (dia-track-resolve-tips style))

    (cond [(dia:block:note? target)
           (geo-path* #:id (dia-track-id-merge (geo-id target) (and source (geo-id source)) #false)
                      #:stroke (dia-track-resolve-line-paint style)
                      #:source-tip ttip #:target-tip stip
                      #:tip-placement 'inside #:source-placement (if (not source) 'center 'inside)
                      tracks)]
          [(dia:block:note? source)
           (geo-path* #:id (dia-track-id-merge (and source (geo-id source)) (and target (geo-id target)) #false)
                      #:stroke (dia-track-resolve-line-paint style)
                      #:source-tip stip #:target-tip ttip
                      #:tip-placement 'inside #:target-placement (if (not target) 'center 'inside)
                      tracks)]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-note-track-adjust : (-> Dia:Block (Option Dia:Block) (-> (Dia-Track-Style Dia-Note-Track-Style))
                                    (Option (Dia-Track-Style Dia-Note-Track-Style)))
  (lambda [source target mk-style]
    ((inst dia-track-theme-adjust Dia-Note-Track-Style Dia:Block (Option Dia:Block)) source target null mk-style default-dia-note-track-theme-adjuster)))

(define dia-note-block-info : (->* (Geo-Anchor-Name (Option String) (-> (Dia-Block-Style Dia-Note-Block-Style)))
                                   (Dia-Note-Metadata)
                                   (Dia-Block-Info Dia-Note-Block-Style Dia-Note-Metadata))
  (lambda [anchor text mk-style [metadata #false]]
    ((inst dia-block-info Dia-Note-Block-Style Dia-Note-Metadata) anchor text mk-style default-dia-note-block-theme-adjuster metadata)))
