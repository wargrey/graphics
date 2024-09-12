#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../node/style.rkt"))
(provide (all-from-out "../edge/style.rkt" "../edge/arrow.rkt"))

(require digimon/struct)
(require racket/string)

(require geofun/font)
(require geofun/stroke)
(require geofun/paint)

(require geofun/digitama/geometry/anchor)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/type.rkt")
(require "../edge/arrow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some aliases
; Annotation -> Comment
; Predefined-Process -> Subroutine
; On-Page-Connector -> Inspection
; Off-Page-Connector -> Reference
; DataFile -> Database
; Manual-Operation -> Operation
; Manual-Input -> Keyboard
; Initialization -> Preparation
(define-type GeoFlow-Node-Type
  (U 'Input 'Output 'Start 'Stop 'Process 'Decision
     'Comment 'Subroutine 'Inspection 'Reference
     'Database 'Document 'Operation 'Keyboard 'Preparation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))

(define-configuration diaflow-edge-style : GeoFlow-Edge-Style #:as dia-edge-base-style
  #:format "default-diaflow-edge-~a"
  ([font : (Option Font) (desc-font #:size 'small)]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint (desc-stroke #:width 2.0 #:color 'DimGray #:join 'round #:cap 'round)]
   [source-shape : Option-Edge-Shape #false]
   [target-shape : Option-Edge-Shape (make-dia-edge-arrow)]))

(define-configuration diaflow-arrow-style : GeoFlow-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint (void)]
   [source-shape : Maybe-Edge-Shape (void)]
   [target-shape : Maybe-Edge-Shape (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-label-construct : (Parameterof (-> Symbol String)) (make-parameter geo-anchor->string))
(define default-diaflow-canonical-start-name : (Parameterof String) (make-parameter ""))
(define default-diaflow-canonical-stop-name : (Parameterof String) (make-parameter ""))

(define default-diaflow-input-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Input-Style))) (make-parameter #false))
(define default-diaflow-output-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Output-Style))) (make-parameter #false))
(define default-diaflow-start-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Start-Style))) (make-parameter #false))
(define default-diaflow-stop-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Stop-Style))) (make-parameter #false))
(define default-diaflow-process-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Process-Style))) (make-parameter #false))
(define default-diaflow-decision-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Decision-Style))) (make-parameter #false))
(define default-diaflow-comment-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Comment-Style))) (make-parameter #false))
(define default-diaflow-subroutine-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Subroutine-Style))) (make-parameter #false))
(define default-diaflow-inspection-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Inspection-Style))) (make-parameter #false))
(define default-diaflow-reference-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Reference-Style))) (make-parameter #false))
(define default-diaflow-database-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Database-Style))) (make-parameter #false))
(define default-diaflow-document-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Document-Style))) (make-parameter #false))
(define default-diaflow-preparation-style-make : (Parameterof (Option (Dia-Node-Style-Make* GeoFlow-Preparation-Style))) (make-parameter #false))

(define-configuration diaflow-node-style : GeoFlow-Node-Style #:as dia-node-base-style
  #:format "default-diaflow-~a"
  ([block-width : Nonnegative-Flonum 256.0]
   [block-height : Nonnegative-Flonum 64.0]
   [font : (Option Font) (desc-font #:size 'xx-large)]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (desc-stroke #:width 2.0 #:color 'DarkGray)]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diaflow-start-style : GeoFlow-Start-Style #:as dia-node-style
  #:format "default-diaflow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-stop-style : GeoFlow-Stop-Style #:as dia-node-style
  #:format "default-diaflow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-input-style : GeoFlow-Input-Style #:as dia-node-style
  #:format "default-diaflow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'ForestGreen]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-output-style : GeoFlow-Output-Style #:as dia-node-style
  #:format "default-diaflow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Purple]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-process-style : GeoFlow-Process-Style #:as dia-node-style
  #:format "default-diaflow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'RoyalBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-decision-style : GeoFlow-Decision-Style #:as dia-node-style
  #:format "default-diaflow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Crimson]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-comment-style : GeoFlow-Comment-Style #:as dia-node-style
  #:format "default-diaflow-comment-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DimGray]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-subroutine-style : GeoFlow-Subroutine-Style #:as dia-node-style
  #:format "default-diaflow-subroutine-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DodgerBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-inspection-style : GeoFlow-Inspection-Style #:as dia-node-style
  #:format "default-diaflow-inspection-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-reference-style : GeoFlow-Reference-Style #:as dia-node-style
  #:format "default-diaflow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-database-style : GeoFlow-Database-Style #:as dia-node-style
  #:format "default-diaflow-database-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-document-style : GeoFlow-Document-Style #:as dia-node-style
  #:format "default-diaflow-document-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-operation-style : GeoFlow-Operation-Style #:as dia-node-style
  #:format "default-diaflow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-keyboard-style : GeoFlow-Keyboard-Style #:as dia-node-style
  #:format "default-diaflow-keyboard-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-preparation-style : GeoFlow-Preparation-Style #:as dia-node-style
  #:format "default-diaflow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Maroon]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-detect : (-> Geo-Anchor-Name (Values Symbol (Option Dia-Node-Style)))
  (lambda [anchor]
    (if (keyword? anchor)
        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (diaflow-node-style-construct (default-diaflow-canonical-start-name) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (diaflow-node-style-construct (default-diaflow-canonical-stop-name) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [else (values 'who-cares #false)]))
        (let ([text (geo-anchor->string anchor)])
          (define size (string-length text))
          (cond [(string-suffix? text "?")
                 (diaflow-node-style-construct text anchor (default-diaflow-decision-style-make) make-diaflow-decision-style)]
                [(string-suffix? text "!")
                 (diaflow-node-style-construct text anchor (default-diaflow-preparation-style-make) make-diaflow-preparation-style)]
                [(string-prefix? text "^")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-start-style-make) make-diaflow-start-style)]
                [(string-suffix? text "$")
                 (diaflow-node-style-construct (substring text 0 (sub1 size)) anchor (default-diaflow-stop-style-make) make-diaflow-stop-style)]
                [(string-prefix? text ">>")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-input-style-make) make-diaflow-input-style)]
                [(string-suffix? text "<<")
                 (diaflow-node-style-construct (substring text 0 (- size 2)) anchor (default-diaflow-output-style-make) make-diaflow-output-style)]
                [(string-prefix? text "//")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-comment-style-make) make-diaflow-comment-style)]
                [(string-prefix? text "->")
                 (diaflow-node-style-construct (substring text 2 size) anchor (default-diaflow-subroutine-style-make) make-diaflow-subroutine-style)]
                [(string-prefix? text "@")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-inspection-style-make) make-diaflow-inspection-style)]
                [(string-prefix? text "&")
                 (diaflow-node-style-construct (substring text 1 size) anchor (default-diaflow-reference-style-make) make-diaflow-reference-style)]
                [(> size 0) (diaflow-node-style-construct text anchor (default-diaflow-process-style-make) make-diaflow-process-style)]
                [else (values 'who-cares #false)])))))

(define #:forall (S) diaflow-node-style-construct : (-> String Geo-Anchor-Name (Option (Dia-Node-Style-Make* S)) (-> S) (Values Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (dia-node-style-construct text anchor mk-style mk-fallback-style)))
