#lang typed/racket

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

(require "unsafe/font.rkt")

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(define-css-keywords [css-font-generic-family css-font-generic-families] #:as CSS:Font-Family 
  generic-font-family-map #:-> String
  [(sans-serif) (case os [(macosx) "Lucida Grande"] [(windows) "Tahoma"] [else "Sans"])]
  [(serif)      (case os [(macosx) "Times"] [(windows) "Times New Roman"] [else "Serif"])]
  [(monospace)  (case os [(macosx) "Courier"] [(windows) "Courier New"] [else "Monospace"])]
  [(fantasy)    (case os [(macosx) "Helvetica"] [(windows) "Arial"] [else "Helvetica"])]
  [(cursive)    (case os [(macosx) "Apple Chancery, Italic"] [(windows) "Palatino Linotype, Italic"] [else "Chancery"])]
  [(system-ui)  (system-ui 'normal-control-font (case os [(macosx) "Helvetica Neue"] [(windows) "Verdana"] [else "Sans"]))]
  [(emoji)      (case os [else "Symbol"])]
  [(math)       (case os [else "Symbol"])]
  [(fangsong)   (case os [(macosx) "ST FangSong"] [(windows) "FangSong"] [else "Symbol"])])

(define-css-keywords css-font-size-option #:as CSS:Font-Size
  generic-font-size-map #:-> [inheritsize Nonnegative-Flonum] [font-medium Nonnegative-Flonum] Nonnegative-Flonum
  [(xx-large) (* 2/1 font-medium)]
  [(x-large)  (* 3/2 font-medium)]
  [(large)    (* 6/5 font-medium)]
  [(small)    (* 8/9 font-medium)]
  [(x-small)  (* 3/4 font-medium)]
  [(xx-small) (* 3/5 font-medium)]
  [(smaller)  (* 5/6 inheritsize)] ; TODO: find a better function to deal with these two keywords.
  [(larger)   (* 6/5 inheritsize)] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
  [#:else     font-medium])

(define-css-keywords css-font-stretch-option #:+> CSS:Font-Stretch ; order matters
  font-stretch->integer integer->font-stretch #:fallback normal
  [0 ultra-condensed extra-condensed condensed semi-condensed normal
     semi-expanded expanded extra-expanded ultra-expanded])

(define-css-keywords css-font-style-option #:+> CSS:Font-Style ; order matters
  font-style->integer integer->font-style #:fallback normal
  [0 normal oblique italic])

(define-css-keywords css-font-weight-option #:+> CSS:Font-Weight ; order matters
  font-weight->integer integer->font-weight #:-> Integer #:fallback normal
  [thin   100] [ultralight 200] [light    300] [semilight  350] [book 380]
  [normal 400] [medium     500] [semibold 600]
  [bold   700] [ultrabold  800] [heavy    900] [ultraheavy 1000])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-font-face : (-> (Listof (U String Symbol)) (-> Symbol String) (Option String))
  (lambda [value family->face]
    (let select ([families value])
      (and (pair? families)
           (let ([family (car families)])
             (or (and (symbol? family) (family->face family))
                 (and (string? family) (face-filter family))
                 (select (cdr families))))))))

(define face-filter : (-> String (Option String))
  (let ([&faces : (Boxof (Option (HashTable String String))) (box #false)]
        [&all-faces : (Boxof (Option (HashTable String String))) (box #false)])
    (lambda [face]
      (define-values (&face-list all?) (if (regexp-match #rx"," face) (values &all-faces #true) (values &faces #false)))
      (define the-face-set : (HashTable String String)
        (or (unbox &face-list)
            (let ([the-set (for/hash : (HashTable String String)
                             ([face (in-list (get-face-list #:all-variants? all?))])
                             (values (string-downcase face) face))])
              (set-box! &face-list the-set)
              the-set)))
      (hash-ref the-face-set (string-downcase face) (thunk #false)))))

(define memcadr : (-> (Listof CSS:Font-Weight) CSS:Font-Weight CSS:Font-Weight)
  (lambda [srclist v]
    (define tail (memv v srclist))
    (cond [(false? tail) v]
          [else (let ([tail (cdr tail)])
                  (if (pair? tail) (car tail) v))])))
