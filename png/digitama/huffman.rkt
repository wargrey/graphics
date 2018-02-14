#lang typed/racket/base

;;; https://en.wikipedia.org/wiki/Huffman_coding

(provide Huffman-Node Huffman-Dictionary)
(provide make-huffman-tree huffman-tree->dictionary)

(require racket/unsafe/ops)

(define-type Huffman-Node (U Internal Leaf))
(define-type Huffman-Dictionary (HashTable Byte (Pairof Integer Integer)))

(struct Node ([weight : Index]) #:transparent)
(struct Internal Node ([ch0 : Huffman-Node] [ch1 : Huffman-Node]) #:transparent)
(struct Leaf Node ([symbol : Byte]) #:transparent)

(define make-huffman-tree : (->* (Bytes) (Index Index) Huffman-Node)
  (lambda [src [idx0 0] [idxn 0]]
    (define end : Index (if (<= idxn idx0) (bytes-length src) idxn))
    ;;;;;;;;;;;; this is much faster than manually inserting via list. ;;;;;;;;;;;;;;
    (define symbols : (HashTable Byte Integer) (make-hasheq))
    (for ([b (in-bytes src idx0 end)]) (hash-update! symbols b add1 generate-zero-when-not-present))
    (define nodes : (Listof Huffman-Node)
      (for/fold ([nodes : (Listof Huffman-Node) null])
                ([(symbol weight) (in-hash symbols)])
        (insert-node nodes (Leaf (assert weight index?) symbol))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (cond [(null? nodes) (Leaf 0 0)]
          [else (let tree ([tail : (Listof Huffman-Node) nodes])
                  (let ([ch0 : Huffman-Node (unsafe-car tail)]
                        [ch1.tail : (Listof Huffman-Node) (unsafe-cdr tail)])
                    (cond [(null? ch1.tail) ch0]
                          [else (let ([ch1 (car ch1.tail)])
                                  (tree (insert-node (cdr ch1.tail)
                                                     (Internal (assert (+ (Node-weight ch0) (Node-weight ch1)) index?)
                                                               ch0 ch1))))])))])))

(define huffman-tree->dictionary : (-> Huffman-Node Huffman-Dictionary)
  (lambda [tree]
    (define symbols : Huffman-Dictionary (make-hasheq))
    (when (> (Node-weight tree) 0)
      (let tr ([deep : Integer 0]
               [code : Integer 0]
               [node : Huffman-Node tree])
        (cond [(Internal? node)
               (let ([ndeep : Integer (unsafe-fx+ deep 1)]
                     [codebase : Integer (unsafe-fxlshift code 1)])
                 (tr ndeep (unsafe-fx+ codebase 0) (Internal-ch0 node))
                 (tr ndeep (unsafe-fx+ codebase 1) (Internal-ch1 node)))]
              [else (hash-set! symbols (Leaf-symbol node) (cons code deep))])))
    symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define generate-zero-when-not-present : (-> Zero)
  (lambda []
    0))

(define insert-node : (-> (Listof Huffman-Node) Huffman-Node (Listof Huffman-Node))
  (lambda [nodes node]
    (define weight : Index (Node-weight node))
    (let insert ([smalls : (Listof Huffman-Node) null]
                 [tail : (Listof Huffman-Node) nodes])
      (cond [(null? tail) (reverse (cons node smalls))]
            [(<= weight (Node-weight (car tail))) (append (reverse smalls) (cons node tail))]
            [else (insert (cons (car tail) smalls) (cdr tail))]))))
