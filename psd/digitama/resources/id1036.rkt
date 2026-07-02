#lang typed/racket/base

(require "format.rkt")

(require "../exn.rkt")
(require "../parser.rkt")

(unsafe-provide 0x40c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x40c : (-> Integer String Bytes Fixnum Index (List Positive-Real) PSD-Thumbnail)
  (lambda [id name block idx size argl]
    (PSD-Thumbnail id name
                   (integer->thumbnail-format (parse-int32 block idx) throw-enum-error)
                   (parse-size block (unsafe-fx+ idx 4)  4 positive-index?) ; width
                   (parse-size block (unsafe-fx+ idx 8)  4 positive-index?) ; height
                   (parse-size block (unsafe-fx+ idx 12) 4 positive-index?) ; padded row bytes
                   (parse-size block (unsafe-fx+ idx 16) 4 positive-index?) ; total size
                   (parse-size block (unsafe-fx+ idx 20) 4 positive-index?) ; compressed size
                   (parse-size block (unsafe-fx+ idx 24) 2 byte?)           ; bits per pixels
                   (parse-size block (unsafe-fx+ idx 26) 2 index?)          ; number of planes
                   #false)))
