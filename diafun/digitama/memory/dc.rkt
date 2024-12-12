#lang typed/racket/base

(provide (all-defined-out))

(require digimon/format)
(require digimon/digitama/unsafe/release/ops)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)
(require geofun/resize)

(require geofun/digitama/convert)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)

(require "style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-variable-raw
  (lambda [#:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [style : Memory-Location-Style] [id : Symbol] [memory0-address : Index] [mask : Natural]
           [memory : Bytes] [base : Positive-Byte]
           [start : Nonnegative-Fixnum 0] [maybe-end : Nonnegative-Fixnum (bytes-length memory)]] : (Listof (List Geo Geo))
    (define end : Index (if (<= maybe-end (bytes-length memory)) maybe-end (bytes-length memory)))
    (define font : (Option Font) (dia-node-select-font style))
    (define color : Option-Fill-Paint (dia-node-select-font-paint style))
    (define loc-stroke : Maybe-Stroke-Paint (dia-node-select-stroke-paint style))
    (define loc-fill : Maybe-Fill-Paint (dia-node-select-fill-paint style))
    (define igr-color : Option-Fill-Paint
      (dia-node-select-font-paint (memory-location-style-ignored-paint style)
                                  memory-location-fallback-style? memory-location-base-style-ignored-paint))

    (let gen-row ([idx : Nonnegative-Fixnum start]
                  [swor : (Listof (List Geo Geo)) null])
      (if (< idx end)
          (let ([address (+ memory0-address idx)])
            (define address-desc : String (memory-address->string address mask))
            (define raw-datum : Byte (bytes-ref memory idx))
            (define binary-desc : String (~binstring raw-datum 8))
            (define datum-desc : (Option String) (memory-raw-datum->string raw-datum base))
            
            (define label : Geo
              (if (string? datum-desc)
                  (geo-vc-append
                   (geo-text datum-desc font #:color color)
                   (geo-text binary-desc font #:lines '(line-through) #:color igr-color))
                  (geo-text binary-desc font #:color color)))
            
            (define-values (loc-width loc-height) (dia-node-smart-size label style))
            (define loc-box : Geo (geo-rectangle #:id (memory-address->id address) #:stroke loc-stroke #:fill loc-fill loc-width loc-height))
            (define addr : Geo
              (if (= idx start)
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
           [style : Memory-Location-Style] [id : Symbol] [address : Natural] [mask : Natural]
           [datum : Any] [base : Positive-Byte]] : (List (List Geo Geo))
    (define datum-desc : String (memory-datum->string style datum base mask))
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
                               (memory-address->string address mask) font)))
    
    (list (memory-location-fitted-row addr label loc-box))))

(define dia-variable-data
  (lambda [#:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [style : Memory-Location-Style] [id : Symbol] [addr0 : Natural] [type-size : Byte] [mask : Natural]
           [data : (Vectorof Any)] [base : Positive-Byte]] : (Listof (List Geo Geo))
    (define size : Index (vector-length data))
    
    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof (List Geo Geo)) null])
      (if (< idx size)
          (gen-row (+ idx 1)
                   (append (dia-variable-datum #:segment vsegment #:rendering-segment rsegment
                                               style (memory-array-id id idx)
                                               (+ addr0 (* idx type-size)) mask
                                               (vector-ref data idx) base)
                           swor))
          swor))))

(define dia-vector-raw
  (lambda [#:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [style : Memory-Location-Style] [id : Symbol] [addr0 : Index] [type-size : Byte] [mask : Natural]
           [memory : Bytes] [base : Positive-Byte]] : (Listof (List Geo Geo))
    (define end : Index (bytes-length memory))
    
    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [subscript : Index 0]
                  [swor : (Listof (List Geo Geo)) null])
      (if (< idx end)
          (let ([idx++ (+ idx type-size)])
            (gen-row idx++
                     (unsafe-idx+ subscript 1)
                     (append (dia-variable-raw #:segment vsegment #:rendering-segment rsegment
                                               style (memory-array-id id subscript) addr0 mask
                                               memory base idx idx++)
                             swor)))
          swor))))

(define dia-padding-raw
  (lambda [[style : Memory-Location-Style] [addr0 : Index] [mask : Natural]
           [memory : Bytes] [base : Byte] [maybe-limit : (Option Index)]] : (Listof (List Geo Geo))
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
              (define addr : Geo (geo-text (memory-address->string address mask) font #:color color))
              
              (gen-row (+ idx 1)
                       (cons (memory-location-fitted-row addr label loc-box) swor)))
            swor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-table-label : (-> (U String Symbol) Geo:Text)
  (lambda [name]
    (geo-text name default-table-header-font)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memory-array-id : (-> Symbol Natural Symbol)
  (lambda [id idx]
    (string->symbol (format "~a[~a]" id idx))))

(define memory-address->id : (-> Natural Symbol)
  (lambda [address]
    (string->symbol (string-append "0x" (~hexstring address)))))

(define memory-address->string : (-> Natural Natural String)
  (lambda [address mask]
    (string-append "0x" (~r (if (> mask 0) (bitwise-and mask address) address) #:base 16))))

(define memory-datum->string : (-> Memory-Location-Style Any Positive-Byte Natural String)
  (lambda [style datum base mask]
    (cond [(and (memory-pointer-style? style) (exact-nonnegative-integer? datum))
           (memory-address->string datum mask)]
          [(exact-integer? datum)
           (case base
             [(10) (number->string datum)]
             [(16) (string-append "0x" (~r datum #:base 16))]
             [(8)  (string-append "0"  (~r datum #:base 8))]
             [else (string-append "0b" (~r datum #:base 2))])]
          [(char? datum)
           (cond [(char-graphic? datum) (format "'~a'" datum)]
                 [else (case datum
                         [(#\null) "'\\0'"]
                         [(#\space) "' '"]
                         [(#\tab) "'\\t'"]
                         [(#\newline) "'\\n'"]
                         [(#\return) "'\\r'"]
                         [else (format "~s" datum)])])]
          [else (format "~a" datum)])))

(define memory-raw-datum->string : (-> Byte Positive-Byte (Option String))
  (lambda [raw-datum base]
    (case base
      [(10) (number->string raw-datum)]
      [(16) (string-append "0x" (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0"))]
      [(8)  (string-append "0"  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0"))]
      [else #false])))

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
