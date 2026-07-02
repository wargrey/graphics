#lang typed/racket/base

(require "format.rkt")

(require "../exn.rkt")
(require "../parser.rkt")

(unsafe-provide 0x408)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define 0x408 : (-> Integer String Bytes Fixnum Index Null PSD-Grid+Guides)
  (lambda [id name block idx size argl]
    (PSD-Grid+Guides id name
                     (parse-uint32 block idx) ; version
                     (parse-uint32 block (unsafe-fx+ idx 4)) ; horizontal
                     (parse-uint32 block (unsafe-fx+ idx 8)) ; vertical

                     (let parse-guide ([fgridcount : Index (parse-size block (unsafe-fx+ idx 12) 4)]
                                       [start : Fixnum (unsafe-fx+ idx 16)]
                                       [guides : (Listof (Pairof Fixnum PSD-Guide-Direction)) null])
                       (cond [(= fgridcount 0) (reverse guides)]
                             [else (parse-guide (unsafe-fx- fgridcount 1)
                                                (unsafe-fx+ start 5)
                                                (cons (cons (parse-int32 block start) ; location
                                                            (integer->vhselect (parse-uint8 block (unsafe-fx+ start 4))
                                                                               throw-enum-error))
                                                      guides))])))))
