#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (all-from-out digimon/digitama/predicate))
(provide (all-from-out digimon/digitama/unsafe/ops))
(provide (all-from-out digimon/digitama/stdio/parser))

(require digimon/digitama/ioexn)
(require digimon/digitama/predicate)
(require digimon/digitama/unsafe/ops)
(require digimon/digitama/stdio/parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-keyword : (All (a) (case-> [Bytes Fixnum -> Symbol]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) -> a]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) (U Symbol Procedure) -> a]
                                         [Bytes Fixnum (-> Any Boolean : #:+ a) (U Symbol Procedure) (-> Symbol String Any Nothing) -> a]))
  (case-lambda
    [(src start)
     (let ([3chkey? : Boolean (= (bytes-ref src (+ start 3)) 32)])
       (string->symbol (bytes->string/utf-8 src #false start (+ start (if 3chkey? 3 4)))))]
    [(src start key?) (assert (parse-keyword src start) key?)]
    [(src start key? op)
     (let ([kw (parse-keyword src start)])
       (cond [(key? kw) kw]
             [else (throw-enum-error op key? kw)]))]
    [(src start key? op throw)
     (let ([kw (parse-keyword src start)])
       (cond [(key? kw) kw]
             [else (throw (if (symbol? op) op (assert (object-name op) symbol?))
                          (exn-constraint->string key?) kw)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-pascal-string : (-> Bytes Fixnum (Values String Fixnum))
  (lambda [src start]
    (define-values (pascal _size pend) (pascal-string src start))
    (values pascal pend)))

(define parse-pascal-string*n : (-> Bytes Fixnum Byte (Values String Fixnum))
  (lambda [src start n]
    (define-values (pascal psize pend) (pascal-string src start))
    (values pascal (unsafe-fx+ pend (unsafe-fx- (unsafe-fx- n 1) (remainder psize n))))))

(define parse-utf16-string : (-> Bytes Fixnum (Values String Fixnum))
  (lambda [src start]
    (define size : Index (parse-size src start 4))
    (cond [(= size 0) (values "" size)]
          [else (let-values ([(unicode chwidth) (values (make-string size #\null) 2)])
                  (let fill-string! ([dest-idx : Fixnum 0]
                                     [src-idx : Fixnum (unsafe-fx+ start 4)]
                                     [end-char : (Option Char) #false])
                    (if (< dest-idx size)
                        (let* ([next-idx (unsafe-fx+ src-idx chwidth)]
                               [cc (integer->char (integer-bytes->integer src #false #true src-idx next-idx))])
                          (string-set! unicode dest-idx cc)
                          (fill-string! (unsafe-fx+ dest-idx 1) next-idx cc))
                        (values (if (eq? end-char #\null) (substring unicode 0 (unsafe-fx- size 1)) unicode)
                                (unsafe-fx+ (unsafe-fx+ start 4) (unsafe-fx* size chwidth))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pascal-string : (-> Bytes Fixnum (Values String Fixnum Fixnum))
  (lambda [src start]
    ;;; NOTE: Pascal strings are usually useless if they contains not-ASCII code
    ;;;   in which case there should be Unicode strings stored elsewhere.
    (define size : Byte (unsafe-bytes-ref src start))
    (define bstart : Fixnum (unsafe-fx+ start 1))
    (define bend : Fixnum (unsafe-fx+ bstart size))
    
    (values (bytes->string/latin-1 src #false bstart bend)
            size bend)))
