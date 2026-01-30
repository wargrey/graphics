#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require geofun/resize)

(require geofun/digitama/dc/gadget)
(require geofun/digitama/dc/dingbat)

(require diafun/digitama/block/dc)
(require diafun/digitama/block/dc/void)
(require diafun/digitama/block/interface)
(require diafun/digitama/track/interface)

(require diafun/digitama/flowchart/style)
(require diafun/digitama/flowchart/identifier)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-track-identify : (Dia-Track-Identifier Flow-Track-Style)
  (lambda [source target labels extra]
    (flow-track-adjust source target labels default-flow~storage~style)))

(define plt-flow-block-build : (Dia-Block-Builder Flow-Block-Style Flow-Block-Metadata)
  (lambda [id label style width height direction subtype]
    (dia-block-case style
     [(flow-process-style?) (plt-flow-block-process id label style width height direction subtype)]
     [(flow-input-style?)   (dia-ghost-block id style)]
     [(flow-output-style?)  (dia-ghost-block id style)]
     [(flow-storage-style?) (when (eq? subtype 'File)
                              (plt-flow-block-file id label style width height direction subtype))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) plt-flow-block-process : (Dia-Block-Create S Flow-Block-Metadata)
  (lambda [id caption style width height direction subtype]
    (define fbox : Geo:Sandglass
      (geo-sandglass #:fill (dia-block-resolve-fill-paint style)
                     #:stroke (dia-block-resolve-stroke-paint style)
                     #:neck-width (* width 0.22) #:neck-height (* width 0.12)
                     (* width 0.25)))

    (if (or (not direction) (not (zero? direction)))
        (create-dia-block #:id id subtype
                          #:fit-region 1.00 0.36 0.00 0.00
                          #:with style fbox caption)
        (create-dia-block #:id id subtype
                          #:fit-region 1.00 1.00 0.0 0.0
                          #:with style (geo-rotate fbox (- direction (* pi 0.5))) caption))))

(define #:forall (S) plt-flow-block-file : (Dia-Block-Create S Flow-Block-Metadata)
  (lambda [id caption style width height direction subtype]
    (define dog-ear% 0.1618)
    (create-dia-block #:id id subtype
                      #:fit-region 1.00 (- 1.0 dog-ear%) 0.0 1.0
                      #:alignment 0.0 0.0
                      #:create-with style [geo-file width height #:dog-ear-size (&: dog-ear%)]
                      caption)))
