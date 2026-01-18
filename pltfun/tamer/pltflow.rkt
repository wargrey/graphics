#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require geofun/markup)
(require pltfun/procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define aoc-assignment-desc : (-> Symbol (-> Any String))
  (lambda [a]
    (λ [[v : Any]] : String
      (format "~s" v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define IPO.dia
  ((inst plt-flow-function String) #:downward? #true #:output-desc "Output"
                                   'Process "Input"))

(define hh-mutate.dia
  (plt-flow-assignment #:read-desc "读取" #:write-desc "写入"
                       'x '|+ 1| (<span> null "x + " (<span> '([style . normal]) "1"))))

(define predicate.dia
  (plt-flow-join #:input-desc '("a" "b") #:downward? #false
                 natural? (list 3 5)))

(define sort.dia
  (plt-flow-function #:input-desc  <a:small>
                     #:output-desc <a:small>
                     (procedure-rename (λ [[xs : (Listof Natural)]] : (Listof Natural)
                                         (sort xs <))
                                       'sort)
                     (list 3 9 3 5 3 4)))

(define read-char-for-line.dia
  (plt-flow-join #:downward? #false #:and? #false
                 #:output-desc (list <s:small>)
                 read-char (list (open-input-bytes #"\n" 'rptin)
                                 (open-input-bytes #"" 'rptin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read.dia
  (plt-flow-read #:reader read #:peek-size 20 #:output-desc (aoc-assignment-desc 'line) #:block-scale 0.85
                 (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5" 'rptin) 6))

(define read-char.dia
  (plt-flow-read #:reader read-char #:peek-size 20 #:output-desc (aoc-assignment-desc 'line)
                 (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5" 'rptin) 6))

(define read-line.dia
  (plt-flow-read #:reader read-line #:peek-size 20 #:output-desc (aoc-assignment-desc 'line)
                 (open-input-bytes #"7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9" 'rptin) 6))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  IPO.dia
  sort.dia
  hh-mutate.dia
  predicate.dia
  
  read-char-for-line.dia
  read.dia
  read-char.dia
  read-line.dia)
