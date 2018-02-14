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

(define src (file->bytes (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))))

(define wiki-test (time (huffman-tree->dictionary (make-huffman-tree #"A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED"))))
(define this-test (time (huffman-tree->dictionary (make-huffman-tree src))))

(display-huffman-dictionary wiki-test "Testcase form Wikipedia")
(display-huffman-dictionary this-test "Testcase of the sources")
