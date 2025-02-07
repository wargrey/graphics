#lang typed/racket/base

;;; https://docs.gtk.org/Pango/pango_markup.html

(provide (all-defined-out))

(require "digitama/markup.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-a:big : (-> Any PExpr-Element) (λ [v] `(big () (,(format "~a" v)))))
(define geo-a:small : (-> Any PExpr-Element) (λ [v] `(small () (,(format "~a" v)))))
(define geo-a:sub : (-> Any PExpr-Element) (λ [v] `(sub () (,(format "~a" v)))))
(define geo-a:sup : (-> Any PExpr-Element) (λ [v] `(sup () (,(format "~a" v)))))
(define geo-a:tt : (-> Any PExpr-Element) (λ [v] `(tt () (,(format "~a" v)))))
(define geo-a:s : (-> Any PExpr-Element) (λ [v] `(s () (,(format "~a" v)))))
(define geo-a:u : (-> Any PExpr-Element) (λ [v] `(u () (,(format "~a" v)))))
(define geo-a:b : (-> Any PExpr-Element) (λ [v] `(b () (,(format "~a" v)))))
(define geo-a:i : (-> Any PExpr-Element) (λ [v] `(i () (,(format "~a" v)))))

(define geo-s:big : (-> Any PExpr-Element) (λ [v] `(big () (,(format "~s" v)))))
(define geo-s:small : (-> Any PExpr-Element) (λ [v] `(small () (,(format "~s" v)))))
(define geo-s:sub : (-> Any PExpr-Element) (λ [v] `(sub () (,(format "~s" v)))))
(define geo-s:sup : (-> Any PExpr-Element) (λ [v] `(sup () (,(format "~s" v)))))
(define geo-s:tt : (-> Any PExpr-Element) (λ [v] `(tt () (,(format "~s" v)))))
(define geo-s:s : (-> Any PExpr-Element) (λ [v] `(s () (,(format "~s" v)))))
(define geo-s:u : (-> Any PExpr-Element) (λ [v] `(u () (,(format "~s" v)))))
(define geo-s:b : (-> Any PExpr-Element) (λ [v] `(b () (,(format "~s" v)))))
(define geo-s:i : (-> Any PExpr-Element) (λ [v] `(i () (,(format "~s" v)))))

