#lang typed/racket/base

(provide (except-out (all-defined-out) geo-flow-node-style-refine))

(require digimon/struct)

(require racket/string)
(require racket/symbol)
(require racket/keyword)

(require "node.rkt")

(require "../../geometry/trail.rkt")
(require "../../../font.rkt")
(require "../../../stroke.rkt")
(require "../../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some aliases
; Annotation -> Comment
; Predefined-Process -> Subroutine
; On-Page-Connector -> Connector
; Off-Page-Connector -> Reference
; DataFile -> Database
; Manual-Operation -> Operation
; Manual-Input -> Keyboard
; Initialization -> Preparation
(define-type Geo-Flow-Block-Type
  (U 'Input 'Output 'Start 'Stop 'Process 'Decision
     'Comment 'Subroutine 'Connector 'Reference
     'Database 'Document 'Operation 'Keyboard 'Preparation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-label-construct : (Parameterof (-> Symbol String)) (make-parameter symbol->immutable-string))
(define default-flow-input-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-output-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-start-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-stop-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-process-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-decision-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-comment-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-subroutine-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-connector-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-reference-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-database-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-document-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))
(define default-flow-preparation-refine : (Parameterof (Option Geo-Node-Style-Refine)) (make-parameter #false))

(define-configuration geo-flow-node-style : Geo-Flow-Node-Style #:as geo-node-base-style
  #:format "default-flow-~a"
  ([block-width : Nonnegative-Flonum 128.0]
   [block-height : Nonnegative-Flonum 36.0]
   [font : (Option Font) (desc-font #:size 'large)]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (desc-stroke #:width 2.0 #:color 'DimGray)]
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

(define-configuration geo-flow-connector-style : Geo-Flow-Connector-Style #:as geo-node-style
  #:format "default-flow-connector-~a"
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
(define geo-flow-block-detect : (-> Geo-Anchor-Name (Values String (Option Geo-Node-Style)))
  (lambda [anchor]
    (if (keyword? anchor)
        (let ([text (keyword->immutable-string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (geo-flow-node-style-refine text anchor make-geo-flow-start-style (default-flow-start-refine))]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (geo-flow-node-style-refine text anchor make-geo-flow-stop-style (default-flow-stop-refine))]
                [else (values "who-cares" #false)]))
        (let ([text (symbol->immutable-string anchor)])
          (define size (string-length text))
          (cond [(string-suffix? text "?")
                 (geo-flow-node-style-refine text anchor make-geo-flow-decision-style (default-flow-decision-refine))]
                [(string-suffix? text "!")
                 (geo-flow-node-style-refine text anchor make-geo-flow-preparation-style (default-flow-preparation-refine))]
                [(string-prefix? text "^")
                 (geo-flow-node-style-refine (substring text 1 size) anchor make-geo-flow-start-style (default-flow-start-refine))]
                [(string-suffix? text "$")
                 (geo-flow-node-style-refine (substring text 0 (sub1 size)) anchor make-geo-flow-stop-style (default-flow-stop-refine))]
                [(string-prefix? text ">>")
                 (geo-flow-node-style-refine (substring text 2 size) anchor make-geo-flow-input-style (default-flow-input-refine))]
                [(string-suffix? text "<<")
                 (geo-flow-node-style-refine (substring text 0 (- size 2)) anchor make-geo-flow-output-style (default-flow-output-refine))]
                [(string-prefix? text "//")
                 (geo-flow-node-style-refine (substring text 2 size) anchor make-geo-flow-comment-style (default-flow-comment-refine))]
                #;[(string-prefix? text "->")
                 (geo-flow-node-style-refine (substring text 2 size) anchor make-geo-flow-subroutine-style (default-flow-subroutine-refine))]
                #;[(string-prefix? text "@")
                 (geo-flow-node-style-refine (substring text 1 size) anchor make-geo-flow-connector-style (default-flow-connector-refine))]
                [(string-prefix? text "&")
                 (geo-flow-node-style-refine (substring text 1 size) anchor make-geo-flow-reference-style (default-flow-reference-refine))]
                [(> size 0) (geo-flow-node-style-refine text anchor make-geo-flow-process-style (default-flow-process-refine))]
                [else (values "who-cares" #false)])))))

(define #:forall (C) geo-flow-node-style-refine : (-> String Geo-Anchor-Name (-> C) (Option (Geo-Node-Style-Refine* C)) (Values String C))
  (lambda [text anchor mk-style refine]
    (geo-node-style-refine text anchor (default-flow-label-construct) mk-style refine)))
