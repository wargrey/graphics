#lang typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4122, A Universally Unique IDentifier (UUID) URN Namespace    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide uuid:random)

(define-type Racket-Place-Status
  (Vector Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
          Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))

(require/typed racket/base
               [vector-set-performance-stats! (-> Racket-Place-Status (Option Thread) Void)])

(define clock-sequence-semaphore : Semaphore (make-semaphore 1))
(define pseudo-random-numbers : Racket-Place-Status (vector 0 0 0 0 0 0 0 0 0 0 0 0))

(define version+timestamp : (-> (U 1 2 3 4 5) String)
  (lambda [version]
    (vector-set-performance-stats! pseudo-random-numbers #false)
    (define process-ms : Nonnegative-Fixnum (vector-ref pseudo-random-numbers 0))
    (define actually-us : Natural (max (exact-round (* (current-inexact-milliseconds) 1000)) 0))
    (define simulated-ns : Natural (remainder process-ms 1000))
    (define ts : String (~a (format "~x" (+ (* actually-us 1000) simulated-ns))
                            #:align 'right #:width 15 #:left-pad-string "0"))
    (define time-low : String (substring ts 7 15))
    (define time-mid : String (substring ts 3 7))
    (define time-high : String (substring ts 0 3))
    (format "~a-~a-~a~a" time-low time-mid version time-high)))

(define variant+clock-sequence : (-> Natural)
  (lambda []
    (dynamic-wind (thunk (semaphore-wait clock-sequence-semaphore))
                  (thunk (let ([gctime (vector-ref pseudo-random-numbers 2)]
                               [place-gcount (vector-ref pseudo-random-numbers 3)]
                               [thd-switches (vector-ref pseudo-random-numbers 4)]
                               [memory (+ (vector-ref pseudo-random-numbers 10) (current-memory-use))])
                           (+ #b1000000000000000 #| ensure the N is 8, 9, A, or B |#
                              (remainder (+ gctime place-gcount thd-switches memory)
                                         #b11111111111111))))
                  (thunk (semaphore-post clock-sequence-semaphore)))))

(define uuid:random : (-> String)
  (lambda []
    (format "~a-~x-~a~a" (version+timestamp 4) (variant+clock-sequence)
            (~a (format "~x" (bitwise-and #xffffff (vector-ref pseudo-random-numbers 8)))
                #:align 'right #:width 6 #:left-pad-string "0")
            (~a (format "~x" (bitwise-and #xffffff (vector-ref pseudo-random-numbers 9)))
                #:align 'right #:width 6 #:left-pad-string "0"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define uuids : (HashTable String Integer) (make-hash))
  (for ([i (in-range 64)])
    (define uuid : String (uuid:random))
    (hash-set! uuids uuid (add1 (hash-ref uuids uuid (const 0)))))

  (exit (for/fold ([errno : Natural 0]) ([(uuid count) (in-hash uuids)])
          (printf "~a: ~a~n" uuid count)
          (if (= count 1) errno (add1 errno)))))
