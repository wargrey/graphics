#lang typed/racket/base

(require racket/file)
(require racket/format)

(require "../digitama/huffman.rkt")

(define display-huffman-dictionary : (->* (Huffman-Dictionary) (String) Void)
  (lambda [symbols [title ""]]
    (unless (equal? title "") (displayln title))
    (for ([(symbol value) (in-hash symbols)])
      (printf "  ~e: ~a~n"
              (integer->char symbol)
              (~r #:base 2 #:min-width (cdr value) #:pad-string "0"
                  (car value))))))

(define wiki-src #"A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED")
(define this-src (file->bytes (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))))

(define wiki-test (time (huffman-tree->dictionary (make-huffman-tree wiki-src))))
(define this-test (time (huffman-tree->dictionary (make-huffman-tree this-src))))

(display-huffman-dictionary wiki-test "Testcase form Wikipedia")
(display-huffman-dictionary this-test "Testcase of the sources")

(define wiki-encoder (time (make-huffman-encoder (make-huffman-tree wiki-src))))
(define this-encoder (time (make-huffman-encoder (make-huffman-tree this-src))))

(define wiki-huffman (time (wiki-encoder wiki-src)))
(define this-huffman (time (this-encoder this-src)))

(cons (number->string (car wiki-huffman) 2) (cdr wiki-huffman))
(cons (number->string (car this-huffman) 2) (cdr this-huffman))

(integer-length (car wiki-huffman))
(integer-length (car this-huffman))
