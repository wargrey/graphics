#lang typed/racket/base

(provide (except-out (all-defined-out) geo-flow-config-refine))

(require digimon/struct)

(require racket/string)
(require racket/symbol)
(require racket/keyword)

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

(define-type (Geo-Flow-Config-Refine G) (-> Symbol G Geo-Anchor-Name G))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-flow-label-construct : (Parameterof (-> Symbol String)) (make-parameter symbol->immutable-string))
(define default-flow-input-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-output-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-start-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-stop-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-process-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-decision-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-comment-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-subroutine-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-connector-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-reference-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-database-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-document-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))
(define default-flow-preparation-refine : (Parameterof (Option (Geo-Flow-Config-Refine Geo-Flow-Block-Config))) (make-parameter #false))

(define default-flow-block-width : (Parameterof Nonnegative-Flonum) (make-parameter 128.0))
(define default-flow-block-height : (Parameterof Nonnegative-Flonum) (make-parameter 36.0))
(define default-flow-font : (Parameterof (Option Font)) (make-parameter (desc-font #:size 'large)))
(define default-flow-font-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-flow-stroke-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (desc-stroke #:width 2.0 #:color 'DimGray)))
(define default-flow-fill-paint : (Parameterof Maybe-Fill-Paint) (make-parameter 'GhostWhite))

(struct geo-flow-block-config
  ([block-width : (Option Nonnegative-Flonum)]
   [block-height : (Option Nonnegative-Flonum)]
   [font : (Option Font)]
   [font-paint : Option-Fill-Paint]
   [stroke-paint : Maybe-Stroke-Paint]
   [fill-paint : Maybe-Fill-Paint])
  #:type-name Geo-Flow-Block-Config
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration geo-flow-start-config : Geo-Flow-Start-Config #:as geo-flow-block-config
  #:format "default-flow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-stop-config : Geo-Flow-Stop-Config #:as geo-flow-block-config
  #:format "default-flow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-input-config : Geo-Flow-Input-Config #:as geo-flow-block-config
  #:format "default-flow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'ForestGreen]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-output-config : Geo-Flow-Output-Config #:as geo-flow-block-config
  #:format "default-flow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Purple]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-process-config : Geo-Flow-Process-Config #:as geo-flow-block-config
  #:format "default-flow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'RoyalBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-decision-config : Geo-Flow-Decision-Config #:as geo-flow-block-config
  #:format "default-flow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Crimson]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-comment-config : Geo-Flow-Comment-Config #:as geo-flow-block-config
  #:format "default-flow-comment-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DimGray]
   [fill-paint : Maybe-Fill-Paint (void)]))
#|
(define-configuration geo-flow-subroutine-config : Geo-Flow-Subroutine-Config #:as geo-flow-block-config
  #:format "default-flow-subroutine-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [stroke-paint : Maybe-Stroke-Paint 'DodgerBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-connector-config : Geo-Flow-Connector-Config #:as geo-flow-block-config
  #:format "default-flow-connector-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))
|#
(define-configuration geo-flow-reference-config : Geo-Flow-Reference-Config #:as geo-flow-block-config
  #:format "default-flow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-database-config : Geo-Flow-Database-Config #:as geo-flow-block-config
  #:format "default-flow-database-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-document-config : Geo-Flow-Document-Config #:as geo-flow-block-config
  #:format "default-flow-document-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-operation-config : Geo-Flow-Operation-Config #:as geo-flow-block-config
  #:format "default-flow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-keyboard-config : Geo-Flow-Keyboard-Config #:as geo-flow-block-config
  #:format "default-flow-keyboard-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration geo-flow-preparation-config : Geo-Flow-Preparation-Config #:as geo-flow-block-config
  #:format "default-flow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Maroon]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-flow-block-detect : (-> Geo-Anchor-Name (Values String (Option Geo-Flow-Block-Config)))
  (lambda [anchor]
    (if (keyword? anchor)
        (let ([text (keyword->immutable-string anchor)])
          (cond [(or (string-ci=? text "home") (string-ci=? text "start"))
                 (geo-flow-config-refine text anchor make-geo-flow-start-config (default-flow-start-refine))]
                [(or (string-ci=? text "end") (string-ci=? text "terminate"))
                 (geo-flow-config-refine text anchor make-geo-flow-stop-config (default-flow-stop-refine))]
                [else (values "who-cares" #false)]))
        (let ([text (symbol->immutable-string anchor)])
          (define size (string-length text))
          (cond [(string-suffix? text "?")
                 (geo-flow-config-refine text anchor make-geo-flow-decision-config (default-flow-decision-refine))]
                [(string-suffix? text "!")
                 (geo-flow-config-refine text anchor make-geo-flow-preparation-config (default-flow-preparation-refine))]
                [(string-prefix? text "^")
                 (geo-flow-config-refine (substring text 1 size) anchor make-geo-flow-start-config (default-flow-start-refine))]
                [(string-suffix? text "$")
                 (geo-flow-config-refine (substring text 0 (sub1 size)) anchor make-geo-flow-stop-config (default-flow-stop-refine))]
                [(string-prefix? text ">>")
                 (geo-flow-config-refine (substring text 2 size) anchor make-geo-flow-input-config (default-flow-input-refine))]
                [(string-suffix? text "<<")
                 (geo-flow-config-refine (substring text 0 (- size 2)) anchor make-geo-flow-output-config (default-flow-output-refine))]
                [(string-prefix? text "//")
                 (geo-flow-config-refine (substring text 2 size) anchor make-geo-flow-comment-config (default-flow-comment-refine))]
                #;[(string-prefix? text "->")
                 (geo-flow-config-refine (substring text 2 size) anchor make-geo-flow-subroutine-config (default-flow-subroutine-refine))]
                #;[(string-prefix? text "@")
                 (geo-flow-config-refine (substring text 1 size) anchor make-geo-flow-connector-config (default-flow-connector-refine))]
                [(string-prefix? text "&")
                 (geo-flow-config-refine (substring text 1 size) anchor make-geo-flow-reference-config (default-flow-reference-refine))]
                [(> size 0) (geo-flow-config-refine text anchor make-geo-flow-process-config (default-flow-process-refine))]
                [else (values "who-cares" #false)])))))

(define #:forall (C) geo-flow-config-refine : (-> String Geo-Anchor-Name (-> C) (Option (Geo-Flow-Config-Refine C)) (Values String C))
  (lambda [text anchor config refine]
    (define key : Symbol (string->symbol text))
    (if (not refine)
        (values ((default-flow-label-construct) key) (config))
        (values ((default-flow-label-construct) key) (refine key (config) anchor)))))
