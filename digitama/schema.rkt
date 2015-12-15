#lang racket

(provide module:schema)

(require datalog)
(require pict)

(require (for-syntax syntax/parse))

;;; Object-Rule Model [http://www.orm.net/pdf/ORM2.pdf]

(define object-type
  (lambda [desc #:width [w 0] #:height [h 0] #:text-color [color-text #false] #:border-color [color-border #false]]
    (define roomsize 12) ; this is the min size that keeps the resulting pict clear
    (define content (foldr vc-append (blank 0) (map (curryr text null) (string-split (~a desc) (string #\newline)))))
    (cc-superimpose (rounded-rectangle (max w (+ (pict-width content) roomsize)) (max h (+ (pict-height content) roomsize))
                                       #:border-color color-border #:border-width 1)
                    (if (false? color-text) content (colorize content color-text)))))

(define model->object-role-diagram
  (lambda [model]
    (object-type model)))

(define-syntax (module:schema stx)
  (syntax-parse stx #:literals []
    [(_ schtheory subdefinition ...)
     #'(begin (module diagram racket
                (provide object-role-diagram)
                
                (require datalog)
                (require pict)

                (define schtheory (make-theory))
                (define object-role-diagram (text "(~a subdefinition ...)")))

              (module schema typed/racket))]))
