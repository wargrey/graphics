#lang typed/racket/base

;;; https://en.wikipedia.org/wiki/Huffman_coding
;;; http://rosettacode.org/wiki/Huffman_coding#Racket

(provide Huffman-Node Huffman-Dictionary Huffman-Encoder)
(provide make-huffman-tree huffman-tree->dictionary make-huffman-encoder)

(require racket/unsafe/ops)

(define-type Huffman-Node (U Internal Leaf))
(define-type Huffman-Encoder (->* (Bytes) (Index Index) (Pairof Natural Index)))
(define-type Huffman-Dictionary (HashTable Byte (Pairof Integer Integer)))

(struct node ([weight : Index]) #:transparent #:type-name Node)
(struct internal node ([ch0 : Huffman-Node] [ch1 : Huffman-Node]) #:transparent #:type-name Internal)
(struct leaf node ([symbol : Byte]) #:transparent #:type-name Leaf)

(define make-huffman-tree : (->* (Bytes) (Index Index) Huffman-Node)
  (lambda [src [idx0 0] [idxn 0]]
    (define end : Index (if (<= idxn idx0) (bytes-length src) idxn))
    ;;;;;;;;;;;;;;;;;;; this is much faster than manually inserting via lists ;;;;;;;;;;;;;;;;;;;;;;
    (define symbols : (HashTable Byte Integer) (make-hasheq))
    (for ([b (in-bytes src idx0 end)]) (hash-update! symbols b add1 generate-zero-when-not-present))
    (define nodes : (Listof Huffman-Node)
      (for/fold ([nodes : (Listof Huffman-Node) null])
                ([(symbol weight) (in-hash symbols)])
        (insert-node nodes (leaf (assert weight index?) symbol))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (huffman-nodes->tree nodes)))

(define huffman-tree->dictionary : (-> Huffman-Node Huffman-Dictionary)
  (lambda [tree]
    (define symbols : Huffman-Dictionary (make-hasheq))
    (when (> (node-weight tree) 0)
      (let tr ([deep : Integer 0]
               [code : Integer 0]
               [node : Huffman-Node tree])
        (cond [(internal? node)
               (let ([ndeep : Integer (unsafe-fx+ deep 1)]
                     [codebase : Integer (unsafe-fxlshift code 1)])
                 (tr ndeep (unsafe-fx+ codebase 0) (internal-ch0 node))
                 (tr ndeep (unsafe-fx+ codebase 1) (internal-ch1 node)))]
              [else (hash-set! symbols (leaf-symbol node) (cons code deep))])))
    symbols))

(define make-huffman-encoder : (-> Huffman-Node Huffman-Encoder)
  (lambda [tree]
    (define symbols : Huffman-Dictionary (huffman-tree->dictionary tree))
    (λ [[src : Bytes] [idx0 0] [idxn 0]] : (Pairof Natural Index)
      (define end : Index (if (<= idxn idx0) (bytes-length src) idxn))
      (let encode ([code : Integer 0]
                   [total : Integer 0]
                   [idx : Integer idx0])
        (cond [(unsafe-fx= idx end) (cons (assert code exact-nonnegative-integer?) (assert total index?))]
              [else (let ([b : Byte (bytes-ref src idx)])
                      (define code.deep : (Pairof Integer Integer) (hash-ref symbols b break-when-symbol-not-found))
                      (define len : Integer (cdr code.deep))
                      (encode (bitwise-ior (arithmetic-shift code len) (car code.deep))
                              (unsafe-fx+ total len)
                              (unsafe-fx+ idx 1)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define generate-zero-when-not-present : (-> Zero)
  (lambda []
    0))

(define break-when-symbol-not-found : (-> Nothing)
  (lambda []
    (error 'huffman "unmatched dictionary")))

(define huffman-nodes->tree : (-> (Listof Huffman-Node) Huffman-Node)
  (lambda [nodes]
    (cond [(null? nodes) (leaf 0 0)]
          [else (let tree ([tail : (Listof Huffman-Node) nodes])
                  (let ([ch0 : Huffman-Node (unsafe-car tail)]
                        [ch1.tail : (Listof Huffman-Node) (unsafe-cdr tail)])
                    (cond [(null? ch1.tail) ch0]
                          [else (let ([ch1 (car ch1.tail)])
                                  (tree (insert-node (cdr ch1.tail)
                                                     (internal (assert (+ (node-weight ch0) (node-weight ch1)) index?)
                                                               ch0 ch1))))])))])))

(define insert-node : (-> (Listof Huffman-Node) Huffman-Node (Listof Huffman-Node))
  (lambda [nodes node]
    (define weight : Index (node-weight node))
    (let insert ([smalls : (Listof Huffman-Node) null]
                 [tail : (Listof Huffman-Node) nodes])
      (cond [(null? tail) (reverse (cons node smalls))]
            [(<= weight (node-weight (car tail))) (append (reverse smalls) (cons node tail))]
            [else (insert (cons (car tail) smalls) (cdr tail))]))))
