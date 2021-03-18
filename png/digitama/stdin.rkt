#lang typed/racket/base

;;; https://www.w3.org/TR/PNG/#5DataRep

(provide (all-defined-out))

(require bitmap/stdio)

(require "crc.rkt")
(require "enum.rkt")
(require "chunk.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-png-header : (-> Input-Port (Values Positive-Index Positive-Index Positive-Byte PNG-Color-Type
                                                 PNG-Compression-Method PNG-Filter-Method PNG-Interlace-Method))
  ;;; https://www.w3.org/TR/PNG/#5PNG-file-signature
  ;;; https://www.w3.org/TR/PNG/#11IHDR
  ;;; https://www.w3.org/TR/PNG/#13Error-checking
  (lambda [/dev/stdin]
    (read-signature /dev/stdin #"\211PNG\r\n\32\n" 'png "unrecognized datastream")

    (let ([header (read-png-boundary-chunk-header* /dev/stdin #"IHDR" 13 read-png-ihdr-chunk)])
      (unless (png-bit-depth-acceptable? (png-ihdr-chunk-bit-depth header) (png-ihdr-chunk-color-type header))
        (throw-check-error /dev/stdin 'png "unacceptable bit depth ~a for color type ~a"
                           (png-ihdr-chunk-bit-depth header) (png-ihdr-chunk-color-type header)))
      
      (values (png-ihdr-chunk-width header) (png-ihdr-chunk-height header)
              (png-ihdr-chunk-bit-depth header) (png-ihdr-chunk-color-type header)
              (png-ihdr-chunk-compression-method header) (png-ihdr-chunk-filter-method header)
              (png-ihdr-chunk-interlace-method header)))))

(define read-png-nonIDATs : (-> Input-Port PNG-Color-Type (Listof (Pairof Symbol Bytes)))
  ;;; https://www.w3.org/TR/PNG/#5ChunkOrdering
  ;;; https://www.w3.org/TR/PNG/#13Decoders.Errors
  ;;; https://www.w3.org/TR/PNG/#14Ordering-of-chunks
  (lambda [/dev/stdin color-type]
    (let read-chunk ([chunks : (Listof (Pairof Symbol Bytes)) null])
      (define name : Bytes (peek-bytes* /dev/stdin 4 4))
      (cond [(equal? name #"IDAT") chunks]
            [(equal? name #"IEND") (throw-syntax-error /dev/stdin 'png "there shall be at least one IDAT chunk")]
            [else (let-values ([(size name data crc) (read-png-chunk /dev/stdin)])
                    (read-chunk chunks))]))))

(define read-png-IDATs : (-> Input-Port (Values Bytes (Listof (Pairof Symbol Bytes))))
  ;;; https://www.w3.org/TR/PNG/#11IDAT
  ;;; https://www.w3.org/TR/PNG/#11IEND
  ;;; https://www.w3.org/TR/PNG/#14Ordering-of-chunks
  (lambda [/dev/stdin]
    (let read-idat ([idats : (Listof Bytes) null])
      (define name : Bytes (peek-bytes* /dev/stdin 4 4))
      (if (equal? name #"IDAT")
          (let-values ([(size name data) (read-png-chunk* /dev/stdin)])
            (read-idat (cons data idats)))
          (values (apply bytes-append (reverse idats))
                  (let read-extra-chunk ([chunks : (Listof (Pairof Symbol Bytes)) null])
                    (define name : Bytes (peek-bytes* /dev/stdin 4 4))
                    (cond [(equal? name #"IEND") (read-png-boundary-chunk-header* /dev/stdin name 0 void) chunks]
                          [else (read-extra-chunk chunks)])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read/peek-png-chunk-info : (-> Input-Port (Values Index Bytes Index))
  ;;; https://www.w3.org/TR/PNG/#5Chunk-layout
  (lambda [/dev/stdin]
    (define chlength : Index (read-muint32 /dev/stdin index? 'read-png-thunk-header))
    (define checksum : Index (checksum-crc32 (peek-nbytes /dev/stdin (+ chlength 4))))
    
    (values chlength (read-nbytes /dev/stdin 4) checksum)))

(define read/check-png-chunk-crc32 : (-> Input-Port Bytes Index Void)
  ;;; https://www.w3.org/TR/PNG/#5Chunk-layout
  (lambda [/dev/stdin type exptcrc]
    (unless (eq? exptcrc (read-muint32 /dev/stdin))
      (throw-check-error /dev/stdin 'png "~a chunk has been corrupted" type))))

(define read-png-chunk : (-> Input-Port (Values Index Bytes Bytes Natural))
  ;;; https://www.w3.org/TR/PNG/#5Chunk-layout
  (lambda [/dev/stdin]
    (define chlength : Index (read-muint32 /dev/stdin index? 'read-png-thunk))
    (values chlength
            (read-nbytes /dev/stdin 4)
            (if (> chlength 0) (read-nbytes /dev/stdin chlength) #"")
            (read-muint32 /dev/stdin))))

(define read-png-chunk* : (-> Input-Port (Values Index Bytes Bytes))
  ;;; https://www.w3.org/TR/PNG/#5Chunk-layout
  ;;; https://www.w3.org/TR/PNG/#13Error-checking
  (lambda [/dev/stdin]
    (define-values (size type data crc) (read-png-chunk /dev/stdin))
    (define exptcrc : Natural (png-crc* type data))
    (unless (= crc exptcrc)
      (throw-check-error /dev/stdin 'png "~a chunk has been corrupted" type))
    (values size type data)))

(define read-png-boundary-chunk-header* : (All (a) (-> Input-Port Bytes Index (-> Input-Port a) a))
  ;;; https://www.w3.org/TR/PNG/#11IHDR
  ;;; https://www.w3.org/TR/PNG/#11IEND
  (lambda [/dev/stdin exptype exptsize read-data]
    (define-values (size type crc) (read/peek-png-chunk-info /dev/stdin))
    (unless (equal? type exptype)
      (throw-syntax-error /dev/stdin 'png "~a chunk is absent" exptype))
    (unless (= size exptsize)
      (throw-syntax-error /dev/stdin 'png "~a chunk has an unexpected length" type))

    (begin0 (read-data /dev/stdin)
            (read/check-png-chunk-crc32 /dev/stdin exptype crc))))
