#lang typed/racket/base

;;; https://www.w3.org/TR/PNG/#D-CRCAppendix

(provide png-crc png-crc*)

(define-syntax-rule (magic c)
  (let ([c>>1 (arithmetic-shift c -1)])
    (cond [(not (bitwise-bit-set? c 0)) c>>1]
          [else (bitwise-xor #xedb88320 c>>1)])))

(define tables : (HashTable Index (Vectorof Natural)) (make-hasheq))

(define make-png-crc-table : (-> Index (Vectorof Natural))
  (lambda [n]
    (define table : (Vectorof Natural) (make-vector n))
    (let update ([ulong : Natural 0])
      (when (< ulong n)
        (vector-set! table ulong (magic (magic (magic (magic (magic (magic (magic (magic ulong #;0) #;1) #;2) #;3) #;4) #;5) #;6) #;7))
        (update (+ ulong 1))))
    table))

(define update-png-crc : (-> Natural Bytes Index Natural)
  (lambda [initial-crc raw n]
    (define table : (Vectorof Natural) (hash-ref! tables n (λ [] (make-png-crc-table n))))
    (for/fold ([c : Natural initial-crc])
              ([b : Byte (in-bytes raw)])
      (bitwise-xor (vector-ref table (remainder (bitwise-xor c b) n))
                   (arithmetic-shift c -8)))))

(define png-crc : (->* (Bytes) (Index) Natural)
  (lambda [raw [n 256]]
    (define initial-crc : Natural #xffffffff)
    (define final-crc : Natural (update-png-crc initial-crc raw n))
    (bitwise-xor final-crc initial-crc #|one's complement|#)))


(define png-crc* : (->* (Bytes Bytes) (Index) Natural)
  (lambda [chunk-type chunk-data [n 256]]
    (define initial-crc : Natural #xffffffff)
    (define type-crc : Natural (update-png-crc initial-crc chunk-type n))
    (define final-crc : Natural (update-png-crc type-crc chunk-data n))
    (bitwise-xor final-crc initial-crc #|one's complement|#)))
