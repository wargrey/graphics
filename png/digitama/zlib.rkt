#lang typed/racket/base

;;; https://www.ietf.org/rfc/rfc1950.txt
;;; https://www.ietf.org/rfc/rfc1951.txt

(provide parse-png-zlib png-zlib-inflate)

(require bitmap/stdio)
(require racket/fixnum)

(define parse-png-zlib : (-> Input-Port Bytes Index Index (Values Byte Natural (Option Natural)))
  ;;; https://www.w3.org/TR/PNG/#10CompressionCM0
  (lambda [/dev/stdin zlib start end]
    (define zlib-magic (integer-bytes->integer zlib #false #true start (fx+ start 2)))
    (unless (= (remainder zlib-magic 31) 0)
      (throw-check-error /dev/stdin 'png "zlib stream not found"))
    (define CMF+FLG (bytes-ref zlib start))
    (define FLaGs (bytes-ref zlib (fx+ start 1)))
    (define fdict? (bitwise-bit-set? FLaGs 5))
    (define flevel (bitwise-bit-field FLaGs 6 8))
    (define ?DICT (and fdict? (parse-uint32 zlib (fx+ start 2))))
    (define adler32 (parse-uint32 zlib (fx- end 4)))
    (unless (= CMF+FLG #b01111000) ; cmethod === 8, cminfo === 7
      (throw-check-error /dev/stdin 'png "zlib stream header is invalid"))
    (values flevel adler32 ?DICT)))

(define png-zlib-inflate : (-> Input-Port Bytes Index Index Natural (Listof Bytes))
  (lambda [/dev/stdin zlib-blocks start end adler32]
    (let read-block : (Listof Bytes) ([block-idx : Index start]
                                      [bit-idx : Byte 0]
                                      [checksum : Natural 1]
                                      [blocks : (Listof Bytes) null])
      (when (>= block-idx end)
        (throw-check-error /dev/stdin 'png "zlib stream has been truncated"))
      (define header-src : Byte (bytes-ref zlib-blocks block-idx))
      (define header : Byte (if (= bit-idx 0) header-src (assert (fxrshift header-src bit-idx) byte?)))
      (define BFINAL : Boolean (bitwise-bit-set? header 0))
      (define BTYPE : Byte (bitwise-bit-field header 1 3))
      (define-values (block block-idx++ bit-idx++ checksum++)
        (case BTYPE
          [(#b00) (read-non-compressed-block zlib-blocks block-idx checksum)]
          [else (throw-check-error /dev/stdin 'png "reserved zlib compression method")]))
      (cond [(not BFINAL) (read-block block-idx++ bit-idx++ checksum++ (cons block blocks))]
            [(= adler32 checksum++) (reverse (cons block blocks))]
            [else (throw-check-error /dev/stdin 'png "IDATs has been corrupted")]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-non-compressed-block : (-> Bytes Index Natural (Values Bytes Index Byte Natural))
  (lambda [zlib-stream idx adler32]
    (define LEN : Index (parse-luint16 zlib-stream (fx+ idx 1)))
    #;(define NLEN/useless : Index (parse-luint16 zlib-stream (fx+ idx 3)))
    (define start (assert (fx+ idx 5) index?))
    (define end (assert (fx+ start LEN) index?))
    (values (subbytes zlib-stream start end) end 0
            (update-adler32 adler32 zlib-stream start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define modulus : Index 65521) ; the largest prime that smaller than 65536

(define update-adler32 : (-> Natural Bytes Integer Integer Natural)
  (lambda [initial-adler32 raw start stop]
    (let adler32++ : Natural ([idx : Integer start]
                              [s1 : Index (fxand initial-adler32 #xFFFF)]
                              [s2 : Index (fxand (fxrshift initial-adler32 16) #xFFFF)])
      (cond [(>= idx stop) (+ (fxlshift s2 16) s1)]
            [else (let* ([s1++ (fxremainder (+ s1 (bytes-ref raw idx)) modulus)]
                         [s2++ (fxremainder (+ s2 s1++) modulus)])
                    (adler32++ (add1 idx) s1++ s2++))]))))
