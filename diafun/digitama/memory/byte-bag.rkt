#lang typed/racket/base

(provide (all-defined-out))

(require digimon/format)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)
(require geofun/resize)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-padding-bytes
  (lambda [#:color [color : Color] #:stroke [stroke : Maybe-Stroke-Paint] #:fill [fill : Maybe-Fill-Paint] #:address-color [acolor : Color]
           [grid-width : Nonnegative-Flonum] [grid-height : Nonnegative-Flonum]
           [addr0 : Index] [memory : Bytes] [base : Byte] [font : Font]] : (Listof (List Geo Geo))
    (define size : Index (bytes-length memory))

    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof (List Geo Geo)) null])
        (cond [(>= idx size) swor]
              [else (let*-values ([(address raw-datum) (values (+ addr0 idx) (bytes-ref memory idx))])
                      (define datum-desc : String
                        (case base
                          [(16) (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0")]
                          [(8)  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0")]
                          [else (byte->binstring raw-datum 8)]))

                      (define row : (List Geo Geo)
                        (list (geo-text (string-append "0x" (~hexstring address)) font #:color acolor)
                              (geo-cc-superimpose (geo-rectangle grid-width grid-height #:stroke stroke #:fill fill)
                                                  (geo-text datum-desc font #:color color))))
                      
                      (gen-row (+ idx 1) (cons row swor)))]))))

(define dia-variable-raw
  (lambda [#:color [color : Color] #:stroke [stroke : Maybe-Stroke-Paint] #:fill [fill : Maybe-Fill-Paint] #:ignored-color [igr-color : Color]
           #:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [grid-width : Nonnegative-Flonum] [grid-height : Nonnegative-Flonum]
           [id : Symbol] [addr0 : Index] [memory : Bytes] [base : Byte]
           [font : Font] [larger-font : Font]] : (Listof (List Geo Geo))
    (define size : Index (bytes-length memory))
    
    (let gen-row ([idx : Nonnegative-Fixnum 0]
                  [swor : (Listof (List Geo Geo)) null])
      (cond [(>= idx size) swor]
            [else (let ([address (+ addr0 idx)])
                    (define address-desc : String (string-append "0x" (~hexstring address)))
                    (define raw-datum : Byte (bytes-ref memory idx))
                    (define binary-desc : String (~binstring raw-datum 8))
                    (define datum-desc : (Option String)
                      (case base
                        [(10) (number->string raw-datum)]
                        [(16) (string-append "0x" (~r raw-datum #:base '(up 16) #:min-width 2 #:pad-string "0"))]
                        [(8)  (string-append "0"  (~r raw-datum #:base 8        #:min-width 3 #:pad-string "0"))]
                        [else #false]))

                    (define row : (List Geo Geo)
                      (list (if (= idx 0)
                                (geo-vr-append (geo-text #:color color #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                                                         id font)
                                               (geo-text address-desc font #:lines '(line-through underline) #:color igr-color))
                                (geo-text address-desc font #:lines '(line-through) #:color igr-color))
                            (geo-cc-superimpose (geo-rectangle grid-width grid-height #:id id #:stroke stroke #:fill fill)
                                                (if (not datum-desc)
                                                    (geo-text binary-desc font #:color color)
                                                    (geo-vc-append
                                                     (geo-text datum-desc larger-font #:color color)
                                                     (geo-text binary-desc font #:lines '(line-through) #:color igr-color))))))
                    
                    (gen-row (+ idx 1) (cons row swor)))]))))

(define dia-variable-datum
  (lambda [#:color [color : Color] #:stroke [stroke : Maybe-Stroke-Paint] #:fill [fill : Maybe-Fill-Paint] #:ignored-color [igr-color : Color]
           #:segment [vsegment : Symbol] #:rendering-segment [rsegment : (Option Symbol)]
           [grid-width : Nonnegative-Flonum] [grid-height : Nonnegative-Flonum] [inset : Nonnegative-Flonum]
           [id : Symbol] [address : Index] [datum : Any] [base : Byte]
           [font : Font] [larger-font : Font]] : (List (List Geo Geo))
    (define box : Geo (geo-rectangle grid-width grid-height #:id id #:stroke stroke #:fill fill))
    (define address-desc : String (string-append "0x" (~hexstring address)))
    (define datum-desc : String
      (if (exact-integer? datum)
          (case base
            [(10) (number->string datum)]
            [(16) (string-append "0x" (~r datum #:base '(up 16)))]
            [(8)  (string-append "0"  (~r datum #:base 8))]
            [else (string-append "0b" (~r datum #:base 2))])
          (format "~a" datum)))
    
    (list
     (list (geo-vr-append (geo-text #:color color #:lines (if (and rsegment (not (eq? vsegment rsegment))) '(line-through) null)
                                    id larger-font)
                          (geo-text address-desc font #:lines '(line-through) #:color igr-color))
           (geo-cc-superimpose box (geo-fit (geo-text datum-desc larger-font #:color color) box
                                            1.0 1.0 (* inset 2.0)))))))
