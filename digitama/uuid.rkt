#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4122, A Universally Unique IDentifier (UUID) URN Namespace    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide uuid:random)

(define clock-sequence-semaphore : Semaphore (make-semaphore 1))

(define version+timestamp : (-> (U 1 2 3 4 5) String)
  (lambda [version]
    (define actually-us : Natural (max (exact-round (* (current-inexact-milliseconds) 1000)) 0))
    (define simulated-ns : Natural (remainder (max (current-gc-milliseconds) 0) 1000))
    (define ts : String (~a #:align 'right #:width 15 #:left-pad-string "0"
                            (format "~x" (+ (* actually-us 1000) simulated-ns))))
    (define time-low : String (substring ts 7 15))
    (define time-mid : String (substring ts 3 7))
    (define time-high : String (substring ts 0 3))
    (format "~a-~a-~a~a" time-low time-mid version time-high)))

(define variant+clock-sequence : (-> Natural)
  (lambda []
    (dynamic-wind (thunk (semaphore-wait clock-sequence-semaphore))
                  (thunk (+ #b1000000000000000 #| ensure the N is 8, 9, A, or B |#
                            (remainder (current-memory-use) #b11111111111111)))
                  (thunk (semaphore-post clock-sequence-semaphore)))))

(define uuid:random : (-> String)
  (lambda []
    (define pr+gc : Fixnum (current-process-milliseconds))
    (define gc : Fixnum (current-gc-milliseconds))
    (format "~a-~x-~a~a" (version+timestamp 4) (variant+clock-sequence)
            (~a #:align 'right #:width 5 #:left-pad-string "0" (format "~x" (bitwise-and #xfffff gc)))
            (~a #:align 'right #:width 7 #:left-pad-string "0" (format "~x" (bitwise-and #xfffffff pr+gc))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define uuids : (HashTable String Integer) (make-hash))
  
  (for ([i (in-range 64)])
    (define uuid : String (uuid:random))
    (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

  (define errno : Natural
    (for/fold ([errno : Natural 0]) ([(uuid count) (in-hash uuids)])
      (printf "~a: ~a~n" uuid count)
      (if (= count 1) errno (add1 errno))))

  (unless (zero? errno)
    (printf "~a duplicates~n" errno)
    (exit errno)))
