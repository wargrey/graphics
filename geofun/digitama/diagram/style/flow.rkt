#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "eshape/arrow.rkt"))
(provide (all-from-out "node.rkt" "edge.rkt"))

(require digimon/struct)
(require racket/string)

(require "node.rkt")
(require "edge.rkt")
(require "eshape/type.rkt")
(require "eshape/arrow.rkt")

(require "../../../font.rkt")
(require "../../../stroke.rkt")
(require "../../../paint.rkt")

(require "../../geometry/anchor.rkt")

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
(define-type Geo-Flow-Node-Type
  (U 'Input 'Output 'Start 'Stop 'Process 'Decision
     'Comment 'Subroutine 'Inspection 'Reference
     'Database 'Document 'Operation 'Keyboard 'Preparation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-arrow-style-make : (Parameterof (Option Geo-Edge-Style-Make)) (make-parameter #false))

(define-configuration geo-flow-edge-style : Geo-Flow-Edge-Style #:as geo-edge-base-style
  #:format "default-flow-edge-~a"
  ([font : (Option Font) (desc-font #:size 'small)]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint (desc-stroke #:width 2.0 #:color 'DimGray #:join 'round #:cap 'round)]
   [source-shape : Option-Edge-Shape #false]
   [target-shape : Option-Edge-Shape (make-geo-edge-arrow)]))

(define-configuration geo-flow-arrow-style : Geo-Flow-Arrow-Style #:as geo-edge-style
  #:format "default-flow-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint (void)]
   [source-shape : Maybe-Edge-Shape (void)]
   [target-shape : Maybe-Edge-Shape (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-label-construct : (Parameterof (-> Symbol String)) (make-parameter geo-anchor->string))
(define default-flow-canonical-start-name : (Parameterof String) (make-parameter ""))
(define default-flow-canonical-stop-name : (Parameterof String) (make-parameter ""))

(define default-flow-input-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Input-Style))) (make-parameter #false))
(define default-flow-output-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Output-Style))) (make-parameter #false))
(define default-flow-start-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Start-Style))) (make-parameter #false))
(define default-flow-stop-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Stop-Style))) (make-parameter #false))
(define default-flow-process-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Process-Style))) (make-parameter #false))
(define default-flow-decision-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Decision-Style))) (make-parameter #false))
(define default-flow-comment-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Comment-Style))) (make-parameter #false))
(define default-flow-subroutine-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Subroutine-Style))) (make-parameter #false))
(define default-flow-inspection-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Inspection-Style))) (make-parameter #false))
(define default-flow-reference-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Reference-Style))) (make-parameter #false))
(define default-flow-database-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Database-Style))) (make-parameter #false))
(define default-flow-document-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Document-Style))) (make-parameter #false))
(define default-flow-preparation-style-make : (Parameterof (Option (Geo-Node-Style-Make* Geo-Flow-Preparation-Style))) (make-parameter #false))

(define-configuration geo-flow-node-style : Geo-Flow-Node-Style #:as geo-node-base-style
  #:format "default-flow-~a"
  ([block-width : Nonnegative-Flonum 128.0]
   [block-height : Nonnegative-Flonum 36.0]
   [font : (Option Font) (desc-font #:size 'large)]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (desc-stroke #:width 2.0 #:color 'DarkGray)]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration geo-flow-start-style : Geo-Flow-Start-Style #:as geo-node-style
  #:format "default-flow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-stop-style : Geo-Flow-Stop-Style #:as geo-node-style
  #:format "default-flow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-input-style : Geo-Flow-Input-Style #:as geo-node-style
  #:format "default-flow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'ForestGreen]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-output-style : Geo-Flow-Output-Style #:as geo-node-style
  #:format "default-flow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Purple]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-process-style : Geo-Flow-Process-Style #:as geo-node-style
  #:format "default-flow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'RoyalBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-decision-style : Geo-Flow-Decision-Style #:as geo-node-style
  #:format "default-flow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Crimson]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-comment-style : Geo-Flow-Comment-Style #:as geo-node-style
  #:format "default-flow-comment-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DimGray]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-subroutine-style : Geo-Flow-Subroutine-Style #:as geo-node-style
  #:format "default-flow-subroutine-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DodgerBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-inspection-style : Geo-Flow-Inspection-Style #:as geo-node-style
  #:format "default-flow-inspection-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-reference-style : Geo-Flow-Reference-Style #:as geo-node-style
  #:format "default-flow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-database-style : Geo-Flow-Database-Style #:as geo-node-style
  #:format "default-flow-database-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-document-style : Geo-Flow-Document-Style #:as geo-node-style
  #:format "default-flow-document-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-operation-style : Geo-Flow-Operation-Style #:as geo-node-style
  #:format "default-flow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-keyboard-style : Geo-Flow-Keyboard-Style #:as geo-node-style
  #:format "default-flow-keyboard-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-preparation-style : Geo-Flow-Preparation-Style #:as geo-node-style
  #:format "default-flow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Maroon]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-detect : (-> Geo-Anchor-Name (Values Symbol (Option Geo-Node-Style)))
  (lambda [anchor]
    (if (keyword? anchor)
        (let ([text (geo-anchor->string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (geo-flow-node-style-construct (default-flow-canonical-start-name) anchor (default-flow-start-style-make) make-geo-flow-start-style)]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (geo-flow-node-style-construct (default-flow-canonical-stop-name) anchor (default-flow-stop-style-make) make-geo-flow-stop-style)]
                [else (values 'who-cares #false)]))
        (let ([text (geo-anchor->string anchor)])
          (define size (string-length text))
          (cond [(string-suffix? text "?")
                 (geo-flow-node-style-construct text anchor (default-flow-decision-style-make) make-geo-flow-decision-style)]
                [(string-suffix? text "!")
                 (geo-flow-node-style-construct text anchor (default-flow-preparation-style-make) make-geo-flow-preparation-style)]
                [(string-prefix? text "^")
                 (geo-flow-node-style-construct (substring text 1 size) anchor (default-flow-start-style-make) make-geo-flow-start-style)]
                [(string-suffix? text "$")
                 (geo-flow-node-style-construct (substring text 0 (sub1 size)) anchor (default-flow-stop-style-make) make-geo-flow-stop-style)]
                [(string-prefix? text ">>")
                 (geo-flow-node-style-construct (substring text 2 size) anchor (default-flow-input-style-make) make-geo-flow-input-style)]
                [(string-suffix? text "<<")
                 (geo-flow-node-style-construct (substring text 0 (- size 2)) anchor (default-flow-output-style-make) make-geo-flow-output-style)]
                [(string-prefix? text "//")
                 (geo-flow-node-style-construct (substring text 2 size) anchor (default-flow-comment-style-make) make-geo-flow-comment-style)]
                [(string-prefix? text "->")
                 (geo-flow-node-style-construct (substring text 2 size) anchor (default-flow-subroutine-style-make) make-geo-flow-subroutine-style)]
                [(string-prefix? text "@")
                 (geo-flow-node-style-construct (substring text 1 size) anchor (default-flow-inspection-style-make) make-geo-flow-inspection-style)]
                [(string-prefix? text "&")
                 (geo-flow-node-style-construct (substring text 1 size) anchor (default-flow-reference-style-make) make-geo-flow-reference-style)]
                [(> size 0) (geo-flow-node-style-construct text anchor (default-flow-process-style-make) make-geo-flow-process-style)]
                [else (values 'who-cares #false)])))))

(define #:forall (S) geo-flow-node-style-construct : (-> String Geo-Anchor-Name (Option (Geo-Node-Style-Make* S)) (-> S) (Values Symbol S))
  (lambda [text anchor mk-style mk-fallback-style]
    (geo-node-style-construct text anchor mk-style mk-fallback-style)))
