#lang typed/racket/base

(provide (all-defined-out))

(require digimon/format)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)
(require geofun/resize)

(require geofun/digitama/convert)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-variable-raw
  (lambda [#:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [style : Memory-Location-Style] [id : Symbol] [addr0 : Index] [memory : Bytes] [base : Byte]] : (Listof (List Geo Geo))
    (define size : Index (bytes-length memory))
    (define font : (Option Font) (dia-node-select-font style))
    (define color : Option-Fill-Paint (dia-node-select-font-paint style))
    (define loc-stroke : Maybe-Stroke-Paint (dia-node-select-stroke-paint style))
    (define loc-fill : Maybe-Fill-Paint (dia-node-select-fill-paint style))
    (define igr-color : Option-Fill-Paint
      (dia-node-select-font-paint (memory-location-style-ignored-paint style)
                                  memory-location-fallback-style? memory-location-base-style-ignored-paint))

    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof (List Geo Geo)) null])
      (if (< idx size)
          (let ([address (+ addr0 idx)])
            (define address-desc : String (string-append "0x" (~hexstring address)))
            (define raw-datum : Byte (bytes-ref memory idx))
            (define binary-desc : String (~binstring raw-datum 8))
            (define datum-desc : (Option String)
              (case base
                [(10) (number->string raw-datum)]
                [(16) (string-append "0x" (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0"))]
                [(8)  (string-append "0"  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0"))]
                [else #false]))
            
            (define label : Geo
              (if (string? datum-desc)
                  (geo-vc-append
                   (geo-text datum-desc font #:color color)
                   (geo-text binary-desc font #:lines '(line-through) #:color igr-color))
                  (geo-text binary-desc font #:color color)))
            
            (define-values (loc-width loc-height) (dia-node-smart-size label style))
            (define loc-box : Geo (geo-rectangle #:id (memory-address->id address) #:stroke loc-stroke #:fill loc-fill loc-width loc-height))
            (define addr : Geo
              (if (= idx 0)
                  (geo-vr-append (geo-text #:color color #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                                           id font)
                                 (geo-text #:lines '(line-through underline) #:color igr-color
                                           address-desc font))
                  (geo-text address-desc font #:lines '(line-through) #:color igr-color)))
            
            (gen-row (+ idx 1)
                     (cons (memory-location-fitted-row addr label loc-box) swor)))
          swor))))

(define dia-variable-datum
  (lambda [#:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [style : Memory-Location-Style] [id : Symbol] [address : Index] [datum : Any] [base : Byte]] : (List (List Geo Geo))
    (define datum-desc : String
      (if (exact-integer? datum)
          (case base
            [(10) (number->string datum)]
            [(16) (string-append "0x" (~r datum #:base 16))]
            [(8)  (string-append "0"  (~r datum #:base 8))]
            [else (string-append "0b" (~r datum #:base 2))])
          (format "~a" datum)))

    (define font : (Option Font) (dia-node-select-font style))
    (define color : Option-Fill-Paint (dia-node-select-font-paint style))
    (define label : (Option Geo) (dia-node-text-label id datum-desc style #:color color #:font font))
    (define-values (loc-width loc-height) (dia-node-smart-size label style))
    
    (define loc-box : Geo
      (geo-rectangle #:id (memory-address->id address)
                     #:stroke (dia-node-select-stroke-paint style)
                     #:fill (dia-node-select-fill-paint style)
                     loc-width loc-height))

    (define addr : Geo
      (geo-vr-append (geo-text #:color color
                               #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                               id font)
                     (geo-text #:lines '(line-through)
                               #:color (dia-node-select-font-paint (memory-location-style-ignored-paint style)
                                                                   memory-location-fallback-style?
                                                                   memory-location-base-style-ignored-paint)
                               (string-append "0x" (~hexstring address)) font)))
    
    (list (memory-location-fitted-row addr label loc-box))))

(define dia-padding-raw
  (lambda [[style : Memory-Location-Style] [addr0 : Index] [memory : Bytes] [base : Byte] [maybe-limit : (Option Index)]] : (Listof (List Geo Geo))
    (define size : Index (bytes-length memory))
    (define limit : Index (min size (or maybe-limit size)))
    (define font : (Option Font) (dia-node-select-font style))
    (define color : Option-Fill-Paint (dia-node-select-font-paint style))
    (define loc-stroke : Maybe-Stroke-Paint (dia-node-select-stroke-paint style))
    (define loc-fill : Maybe-Fill-Paint (dia-node-select-fill-paint style))

    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof (List Geo Geo)) null])
        (if (and (< idx size) (<= idx limit))
            (let*-values ([(address raw-datum) (values (+ addr0 idx) (bytes-ref memory idx))])
              (define datum-desc : String
                (if (< idx limit)
                    (case base
                      [(16) (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0")]
                      [(8)  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0")]
                      [else (byte->binstring raw-datum 8)])
                    (format "(+~a)" (- size limit))))

              (define label : Geo (geo-text datum-desc font #:color color))
              (define-values (loc-width loc-height) (dia-node-smart-size label style))
              (define loc-box : Geo (geo-rectangle #:id (memory-address->id address) #:stroke loc-stroke #:fill loc-fill loc-width loc-height))
              (define addr : Geo (geo-text (string-append "0x" (~hexstring address)) font #:color color))
              
              (gen-row (+ idx 1)
                       (cons (memory-location-fitted-row addr label loc-box) swor)))
            swor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memory-address->id : (-> Nonnegative-Fixnum Symbol)
  (lambda [address]
    (string->symbol (string-append "0x" (~hexstring address)))))

(define memory-location-fitted-row : (-> Geo (Option Geo) Geo (List Geo Geo))
  (lambda [address label loc-box]
    (define a-height (geo-height address))
    (define b-height (geo-height loc-box))
    
    (list (if (> a-height b-height)
              (geo-scale address (/ b-height a-height))
              address)
          
          (if (or label)
              (let ([fit-label (geo-fit label loc-box 1.0 1.0 (default-dia-node-margin))])
                (geo-cc-superimpose loc-box fit-label))
              loc-box))))
