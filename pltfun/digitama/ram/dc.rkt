#lang typed/racket/base

(provide (all-defined-out))

(require digimon/format)
(require digimon/digitama/unsafe/release/ops)

(require geofun/font)
(require geofun/resize)
(require geofun/composite)

(require geofun/digitama/self)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)
(require geofun/digitama/paint/self)

(require exprfun/digitama/presets)

(require "style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plt-RAM-Variable-Layout (-> (Option Geo) Geo (Option Geo) Geo (Pairof Geo (Listof Geo))))

(struct ram-variable
  ([name : (Option Geo)]
   [address : Geo]
   [datum : (Option Geo)]
   [shape : Geo])
  #:transparent
  #:type-name RAM-Variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-variable-raw
  (lambda [#:segment [vsegment : Symbol]
           #:rendering-segment [rsegment : (Option Symbol)]
           #:width [loc-width : Nonnegative-Flonum]
           #:height [loc-height : Nonnegative-Flonum]
           [style : (Expr-Slot-Style-Layers* RAM-Block-Style)]
           [id : Symbol] [ram0-address : Index]
           [mask : Natural]
           [ram : Bytes]
           [base : Positive-Byte]
           [start : Nonnegative-Fixnum 0]
           [maybe-end : Nonnegative-Fixnum (bytes-length ram)]] : (Listof RAM-Variable)
    (define end : Index (if (<= maybe-end (bytes-length ram)) maybe-end (bytes-length ram)))
    (define font : (Option Font) (expr-slot-resolve-font style))
    (define color : Option-Fill-Paint (expr-slot-resolve-font-paint style))
    (define loc-stroke : Maybe-Stroke-Paint (expr-slot-resolve-stroke-paint style))
    (define loc-fill : Maybe-Fill-Paint (expr-slot-resolve-fill-paint style))
    (define igr-color : Option-Fill-Paint
      (expr-slot-resolve-fill-paint style ram-block-style-ignored-paint
                                    ram-location-backstop-style? ram-block-backstop-style-ignored-paint))
    
    (let gen-row ([idx : Nonnegative-Fixnum start]
                  [swor : (Listof RAM-Variable) null])
      (if (< idx end)
          (let ([address (+ ram0-address idx)])
            (define address-desc : String (ram-address->string address mask))
            (define raw-datum : Byte (bytes-ref ram idx))
            (define binary-desc : String (~binstring raw-datum 8))
            (define datum-desc : (Option String) (ram-raw-datum->string raw-datum base))
            
            (define label : Geo
              (if (string? datum-desc)
                  (geo-vc-append
                   (geo-text datum-desc font #:color color)
                   (geo-text binary-desc font #:lines '(line-through) #:color igr-color))
                  (geo-text binary-desc font #:color color)))
            
            (define loc-box : Geo (geo-rectangle #:id (ram-address->id address) #:stroke loc-stroke #:fill loc-fill loc-width loc-height))
            (define-values (var addr)
              (if (= idx start)
                  (values (geo-text #:color color #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                                    id font)
                          (geo-text #:lines '(line-through underline) #:color igr-color
                                    address-desc font))
                  (values #false
                          (geo-text address-desc font #:lines '(line-through) #:color igr-color))))
            
            (gen-row (+ idx 1)
                     (cons (RAM-Variable var addr label loc-box)
                           swor)))
          swor))))

(define plt-variable-datum
  (lambda [#:segment [vsegment : Symbol]
           #:rendering-segment [rsegment : (Option Symbol)]
           #:width [loc-width : Nonnegative-Flonum]
           #:height [loc-height : Nonnegative-Flonum]
           [style : (Expr-Slot-Style-Layers* RAM-Block-Style)]
           [id : Symbol]
           [address : Natural]
           [mask : Natural]
           [datum : Any]
           [base : Positive-Byte]] : (List RAM-Variable)
    (define datum-desc : String (ram-datum->string (car style) datum base mask))
    (define font : (Option Font) (expr-slot-resolve-font style))
    (define color : Option-Fill-Paint (expr-slot-resolve-font-paint style))
    (define label : (Option Geo) (expr-slot-text-term datum-desc style #:id id #:color color #:font font))
    
    (define loc-box : Geo
      (geo-rectangle #:id (ram-address->id address)
                     #:stroke (expr-slot-resolve-stroke-paint style)
                     #:fill (expr-slot-resolve-fill-paint style)
                     loc-width loc-height))

    (define-values (var addr)
      (values (geo-text #:color color
                        #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                        id font)
              (geo-text #:lines '(line-through)
                        #:color (expr-slot-resolve-fill-paint style ram-block-style-ignored-paint
                                                              ram-location-backstop-style? ram-block-backstop-style-ignored-paint)
                        (ram-address->string address mask) font)))
    
    (list (RAM-Variable var addr label loc-box))))

(define plt-padding-raw
  (lambda [#:width [loc-width : Nonnegative-Flonum]
           #:height [loc-height : Nonnegative-Flonum]
           [style : (Expr-Slot-Style-Layers* RAM-Block-Style)]
           [addr0 : Index]
           [mask : Natural]
           [ram : Bytes]
           [base : Byte]
           [maybe-limit : (Option Index)]] : (Listof RAM-Variable)
    (define size : Index (bytes-length ram))
    (define limit : Index (min size (or maybe-limit size)))
    (define font : (Option Font) (expr-slot-resolve-font style))
    (define color : Option-Fill-Paint (expr-slot-resolve-font-paint style))
    (define loc-stroke : Maybe-Stroke-Paint (expr-slot-resolve-stroke-paint style))
    (define loc-fill : Maybe-Fill-Paint (expr-slot-resolve-fill-paint style))

    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof RAM-Variable) null])
      (if (and (< idx size) (<= idx limit))
          (let*-values ([(address raw-datum) (values (+ addr0 idx) (bytes-ref ram idx))])
            (define datum-desc : String
              (if (< idx limit)
                  (case base
                    [(16) (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0")]
                    [(8)  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0")]
                    [else (byte->binstring raw-datum 8)])
                  (format "(+~a)" (- size limit))))

            (define label : Geo (geo-text datum-desc font #:color color))
            (define addr : Geo (geo-text (ram-address->string address mask) font #:color color))
            (define loc-box : Geo
              (geo-rectangle #:id (ram-address->id address)
                             #:stroke loc-stroke
                             #:fill loc-fill
                             loc-width loc-height))
              
            (gen-row (+ idx 1)
                     (cons (RAM-Variable #false addr label loc-box)
                           swor)))
          swor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-variable-data
  (lambda [#:segment [vsegment : Symbol]
           #:rendering-segment [rsegment : (Option Symbol)]
           #:width [loc-width : Nonnegative-Flonum]
           #:height [loc-height : Nonnegative-Flonum]
           [style : (Expr-Slot-Style-Layers* RAM-Block-Style)]
           [id : Symbol]
           [addr0 : Natural]
           [type-size : Byte]
           [mask : Natural]
           [data : (Vectorof Any)]
           [base : Positive-Byte]] : (Listof RAM-Variable)
    (define size : Index (vector-length data))
    
    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof RAM-Variable) null])
      (if (< idx size)
          (gen-row (+ idx 1)
                   (append (plt-variable-datum #:segment vsegment #:rendering-segment rsegment
                                               #:width loc-width #:height loc-height
                                               style (ram-array-id id idx)
                                               (+ addr0 (* idx type-size)) mask
                                               (vector-ref data idx) base)
                           swor))
          swor))))

(define plt-vector-raw
  (lambda [#:segment [vsegment : Symbol]
           #:rendering-segment [rsegment : (Option Symbol)]
           #:width [loc-width : Nonnegative-Flonum]
           #:height [loc-height : Nonnegative-Flonum]
           [style : (Expr-Slot-Style-Layers* RAM-Block-Style)]
           [id : Symbol]
           [addr0 : Index]
           [type-size : Byte]
           [mask : Natural]
           [ram : Bytes]
           [base : Positive-Byte]] : (Listof RAM-Variable)
    (define end : Index (bytes-length ram))
    
    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [subscript : Index 0]
                  [swor : (Listof RAM-Variable) null])
      (if (< idx end)
          (let ([idx++ (+ idx type-size)])
            (gen-row idx++
                     (unsafe-idx+ subscript 1)
                     (append (plt-variable-raw #:segment vsegment #:rendering-segment rsegment
                                               #:width loc-width #:height loc-height
                                               style (ram-array-id id subscript) addr0 mask
                                               ram base idx idx++)
                             swor)))
          swor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-ram-table-label : (-> (U String Symbol) Geo:Text)
  (lambda [name]
    (geo-text name expr-preset-header-font)))

(define plt-ram-variable-layout : Plt-RAM-Variable-Layout
  (lambda [name addr datum shape]
    (define address
      (cond [(not name) addr]
            [else (geo-vr-append name addr)]))
    
    (define a-height (geo-height address))
    (define s-height (geo-height shape))
    
    (list (if (> a-height s-height)
              (geo-scale address (/ s-height a-height))
              address)
          
          (if (or datum)
              (let ([fit-label (geo-try-dsfit datum shape 1.0 1.0 (default-expr-slot-margin))])
                (cond [(not fit-label) shape]
                      [else (geo-cc-superimpose shape fit-label)]))
              shape))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ram-array-id : (-> Symbol Natural Symbol)
  (lambda [id idx]
    (string->symbol (format "~a[~a]" id idx))))

(define ram-address->id : (-> Natural Symbol)
  (lambda [address]
    (string->symbol (string-append "0x" (~hexstring address)))))

(define ram-address->string : (-> Natural Natural String)
  (lambda [address mask]
    (string-append "0x" (~r (if (> mask 0) (bitwise-and mask address) address) #:base 16))))

(define ram-datum->string : (-> RAM-Block-Style Any Positive-Byte Natural String)
  (lambda [style datum base mask]
    (cond [(and (ram-pointer-style? style) (exact-nonnegative-integer? datum))
           (ram-address->string datum mask)]
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

(define ram-raw-datum->string : (-> Byte Positive-Byte (Option String))
  (lambda [raw-datum base]
    (case base
      [(10) (number->string raw-datum)]
      [(16) (string-append "0x" (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0"))]
      [(8)  (string-append "0"  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0"))]
      [else #false])))
