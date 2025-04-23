#lang typed/racket/base

;;; https://docs.gtk.org/Pango/pango_markup.html

(provide (all-defined-out))

(require "digitama/markup.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <markup> : (-> PExpr * PExpr-Element) (λ children (pexpr 'markup children)))
(define <sub> : (-> PExpr * PExpr-Element) (λ children (pexpr 'sub children)))
(define <sup> : (-> PExpr * PExpr-Element) (λ children (pexpr 'sup children)))
(define <span> : (-> PExpr-AttList PExpr * PExpr-Element) (λ [attrs . children] (pexpr 'span attrs children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <a:big> : (-> Any PExpr-Element) (λ [v] (pexpr 'big (format "~a" v))))
(define <a:small> : (-> Any PExpr-Element) (λ [v] (pexpr 'small (format "~a" v))))
(define <a:sub> : (-> Any PExpr-Element) (λ [v] (pexpr 'sub (format "~a" v))))
(define <a:sup> : (-> Any PExpr-Element) (λ [v] (pexpr 'sup (format "~a" v))))
(define <a:tt> : (-> Any PExpr-Element) (λ [v] (pexpr 'tt (format "~a" v))))
(define <a:s> : (-> Any PExpr-Element) (λ [v] (pexpr 's (format "~a" v))))
(define <a:u> : (-> Any PExpr-Element) (λ [v] (pexpr 'u (format "~a" v))))
(define <a:b> : (-> Any PExpr-Element) (λ [v] (pexpr 'b (format "~a" v))))
(define <a:i> : (-> Any PExpr-Element) (λ [v] (pexpr 'i (format "~a" v))))

(define <s:big> : (-> Any PExpr-Element) (λ [v] (pexpr 'big (format "~s" v))))
(define <s:small> : (-> Any PExpr-Element) (λ [v] (pexpr 'small (format "~s" v))))
(define <s:sub> : (-> Any PExpr-Element) (λ [v] (pexpr 'sub (format "~s" v))))
(define <s:sup> : (-> Any PExpr-Element) (λ [v] (pexpr 'sup (format "~s" v))))
(define <s:tt> : (-> Any PExpr-Element) (λ [v] (pexpr 'tt (format "~s" v))))
(define <s:s> : (-> Any PExpr-Element) (λ [v] (pexpr 's (format "~s" v))))
(define <s:u> : (-> Any PExpr-Element) (λ [v] (pexpr 'u (format "~s" v))))
(define <s:b> : (-> Any PExpr-Element) (λ [v] (pexpr 'b (format "~s" v))))
(define <s:i>> : (-> Any PExpr-Element) (λ [v] (pexpr 'i (format "~s" v))))
