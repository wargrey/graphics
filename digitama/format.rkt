#lang at-exp typed/racket

(provide (all-defined-out))

@require{sugar.rkt}

(define ~n_w : (-> Nonnegative-Integer String String)
  (lambda [count word]
    (format "~a ~a" count (plural count word))))

(define ~w=n : (-> Nonnegative-Integer String String)
  (lambda [count word]
    (format "~a=~a" (plural count word) count)))

(define ~t : (-> Natural Natural String)
  (lambda [n w]
    (~r #:min-width w #:pad-string "0" n)))

(define ~% : (-> Flonum [#:precision (U Integer (List '= Integer))] String)
  (lambda [% #:precision [prcs '(= 2)]]
    (~r (* 100.0 (max 0 %)) #:precision prcs)))

(define ~uptime : (-> Natural String)
  (lambda [s]
    (let*-values ([(d s) (quotient/remainder s 86400)]
                  [(h s) (quotient/remainder s 3600)]
                  [(m s) (quotient/remainder s 60)])
      (format "~a+~a:~a:~a" d (~t h 2) (~t m 2) (~t s 2)))))

(define ~size : (-> Nonnegative-Real Symbol [#:precision (U Integer (List '= Integer))] String)
  (lambda [size unit #:precision [prcs '(= 3)]]
    (define-type/enum units : Unit 'Bytes 'KB 'MB 'GB 'TB)
    (let try-next-unit : String ([s size] [us (cast (member unit units) (Listof Unit))])
      (cond [(and (symbol=? (car us) 'Bytes) (< s 1024.0)) (~n_w (cast s Nonnegative-Integer) "Byte")]
            [(or (< s 1024.0) (zero? (sub1 (length us)))) (format "~a~a" (~r s #:precision prcs) (car us))]
            [else (try-next-unit (/ s 1024.0) ((inst cdr Unit Unit) us))]))))

(module digitama typed/racket
  (provide (all-defined-out))

  (define plural : (-> Nonnegative-Integer String String)
    (lambda [n word]
      (define dict : (HashTable String String) #hash(("story" . "stories") ("Story" . "Stories")))
      (cond [(= n 1) word]
            [else (hash-ref dict word (λ _ (string-append word "s")))]))))

(require (submod "." digitama))
