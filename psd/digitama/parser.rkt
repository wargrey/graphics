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
     (let ([3chkey? : Boolean (= (bytes-ref src (unsafe-fx+ start 3)) 32)])
       (string->symbol (bytes->string/utf-8 src #false start (unsafe-fx+ start (if 3chkey? 3 4)))))]
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
(define parse-pascal-string : (-> Bytes Index (Values String Index))
  (lambda [src start]
    (define-values (pascal _size pend) (pascal-string src start))
    (values pascal pend)))

(define parse-pascal-string*n : (-> Bytes Index Byte (Values String Index))
  (lambda [src start n]
    (define-values (pascal psize pend) (pascal-string src start))
    (values pascal (unsafe-idx+ pend (unsafe-idx- (unsafe-idx- n 1) (remainder psize n))))))

(define parse-utf16-string : (-> Bytes Index (Values String Index))
  (lambda [src start]
    (define size : Index (parse-size src start 4))
    (define src-start : Index (unsafe-idx+ start 4))
    
    (if (> size 0)
        (let ([size-1 (- size 1)])
          (let extract-string ([dst-idx : Index 0]
                               [src-idx : Index src-start]
                               [srahc : (Listof Char) null])
            (if (< dst-idx size-1)
                (let* ([nxt-idx (unsafe-idx+ src-idx 2)]
                       [cc (integer->char (integer-bytes->integer src #false #true src-idx nxt-idx))])
                  (extract-string (unsafe-idx+ dst-idx 1) nxt-idx (cons cc srahc)))
                (values (list->string (reverse srahc)) src-idx))))
        (values "" src-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pascal-string : (-> Bytes Index (Values String Index Index))
  (lambda [src start]
    ;;; NOTE: Pascal strings are usually useless if they contains not-ASCII code
    ;;;   in which case there should be Unicode strings stored elsewhere.
    (define size : Byte (unsafe-bytes-ref src start))
    (define bstart : Index (unsafe-idx+ start 1))
    (define bend : Index (unsafe-idx+ bstart size))
    
    (values (bytes->string/latin-1 src #false bstart bend)
            size bend)))
