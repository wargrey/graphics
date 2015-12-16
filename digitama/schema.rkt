#lang racket

(provide (except-out (all-defined-out) module:datalog))

(require (for-syntax racket/list))

(define object-role-diagram
  (lambda [schema]
    (entity schema)))

(define-syntax (module:datalog stx)
  (syntax-case stx []
    [(_ model statements ...)
     (with-syntax ([(clauses ...)
                    (for/fold ([facts null])
                              ([definition (in-list (syntax->list #'(statements ...)))])
                      (syntax-case definition [: % as define-table define-fact]
                        [(define-table table as alias ([field : DataType % comment ...] ...))
                         (append facts (list #'(object 'table)))]
                        [(define-fact [fact ... % comment] ...)
                         (append facts (syntax->list #'([fact ...] ...)))]))])
       #'(module theory racket/base
           (provide (all-defined-out))
           (provide (all-from-out datalog))
           
           (require datalog)
           (define model (make-theory))
           (void (datalog model (! clauses) ...))))]))

(define-syntax (define-schema stx)
  (syntax-case stx []
    [(_ model statements ...)
     (with-syntax ([([table [fields Types] ...] ...)
                    (filter-map (lambda [definition]
                                  (syntax-case definition [: % as define-table define-fact]
                                    [(define-table table as alias ([field : DataType % comment ...] ...))
                                     #'(table [field DataType] ...)]
                                    [(define-fact fact ...) #false]))
                                (syntax->list #'(statements ...)))])
     #'(begin (module:datalog model statements ...)
              (struct table (fields ...) #:prefab) ...
              
              (module* typed typed/racket/base
                (require/typed/provide (submod "..")
                                       [#:struct table ([fields : Types] ...)] ...))))]))

(module digitama racket
  (provide (all-defined-out))

  (require pict)
  
  ;;; Object-Rule Model [http://www.orm.net/pdf/ORM2.pdf]
  (define entity
    (lambda [desc #:width [w 0] #:height [h 0] #:text-color [color-text #false] #:border-color [color-border #false]]
      (define roomsize 12) ; this is the min size that keeps the resulting pict clear
      (define content (foldr vc-append (blank 0) (map (curryr text null) (string-split (~a desc) (string #\newline)))))
      (cc-superimpose (rounded-rectangle (max w (+ (pict-width content) roomsize)) (max h (+ (pict-height content) roomsize))
                                         #:border-color color-border #:border-width 1)
                      (if (false? color-text) content (colorize content color-text))))))

(require (submod "." digitama))

#;(define-schema digimon
  (define-fact [has press unid % "每一个出版社都有一个唯一代码，此代码是书号的一部分"])
  (define-table digimon as 数码宝贝资料卡
    ([name           : String                 % 日文名称]
     [name/en        : (Option String)        % 英文名称]
     [description    : String                 % 详细资料])))
;(require (submod "." theory))
