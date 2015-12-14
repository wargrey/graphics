#lang typed/racket

(provide (all-defined-out))

(module digitama racket
  (provide (all-defined-out))
  
  (require pict)
  
  (define pict-a (ellipse 64 32 #:border-color "cyan"))
  (define pict-b (linestyle 'short-dash (ellipse 64 32 #:border-color "red")))
  (define dot (disk 4 #:color "blue"))
  (define combined (hc-append 128 (rc-superimpose pict-a dot) pict-b))
 
  (pin-arrow-line 0 combined
                  dot rc-find
                  pict-b lc-find
                  #:line-width 1
                  #:style 'short-dash
                  #:color "green"
                  #:label (text "对象－角色 图")
                  #:hide-arrowhead? #true))

(require (submod "." digitama))
