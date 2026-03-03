#lang typed/racket/base

(provide (all-defined-out))

(require geofun/track)

(require racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-note-here! : (->* (Gomamon Real Real)
                                  (#:stereotype (Option Any)) #:rest String
                                  Void)
  (lambda [goma length direction.rad #:stereotype [sotype #false] . comments]
    (gomamon-note-here*! #:stereotype sotype
                         goma length direction.rad comments)))

(define gomamon-note! : (->* (Gomamon (U Geo-Anchor-Name Complex) Real Real)
                             (#:stereotype (Option Any)) #:rest String
                             Void)
  (lambda [goma target length direction.rad #:stereotype [sotype #false] . comments]
    (gomamon-note*! #:stereotype sotype
                    goma target length direction.rad comments)))

(define gomamon-note*! : (->* (Gomamon (U Geo-Anchor-Name Complex) Real Real (U String (Listof String)))
                              (#:stereotype (Option Any))
                              Void)
  (lambda [#:stereotype [sotype #false] goma target length direction.rad comments]
    (gomamon-jump-to! goma target)
    (gomamon-note-here*! #:stereotype sotype
                         goma length direction.rad comments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-note-here*! : (->* (Gomamon Real Real (U String (Listof String))) (#:stereotype (Option Any)) Void)
  (lambda [#:stereotype [sotype #false] goma length direction.rad comments]
    (define body : String
      (cond [(string? comments) comments]
            [(null? comments) ""]
            [(null? (cdr comments)) (car comments)]
            [else (string-join comments "\n")]))

    (define needs//? : Boolean (not (string-prefix? body "//")))
    
    (gomamon-radial-back! goma length direction.rad
                          (string->symbol (cond [(and needs//? sotype) (format "//~a#~a" body sotype)]
                                                [(or sotype) (format "~a#~a" body sotype)]
                                                [(or needs//?) (string-append "//" body)]
                                                [else body])))))

