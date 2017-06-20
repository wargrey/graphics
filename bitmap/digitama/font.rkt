#lang typed/racket/base

(provide (all-defined-out))

(require "draw.rkt")
(require "misc.rkt")

(require "unsafe/font.rkt")

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(define-enumeration* [css-font-generic-family css-font-generic-families] #:as Font-Family 
  font-family->face #:-> String
  [(sans-serif) (case os [(macosx) "Lucida Grande"] [(windows) "Tahoma"] [else "Sans"])]
  [(serif)      (case os [(macosx) "Times"] [(windows) "Times New Roman"] [else "Serif"])]
  [(monospace)  (case os [(macosx) "Courier"] [(windows) "Courier New"] [else "Monospace"])]
  [(fantasy)    (case os [(macosx) "Helvetica"] [(windows) "Arial"] [else "Helvetica"])]
  [(cursive)    (case os [(macosx) "Apple Chancery, Italic"] [(windows) "Palatino Linotype, Italic"] [else "Chancery"])]
  [(system-ui)  (system-ui 'normal-control-font (case os [(macosx) "Helvetica Neue"] [(windows) "Verdana"] [else "Sans"]))]
  [(emoji)      (case os [else "Symbol"])]
  [(math)       (case os [else "Symbol"])]
  [(fangsong)   (case os [(macosx) "ST FangSong"] [(windows) "FangSong"] [else "Symbol"])])

(define-enumeration* css-font-size-option #:as Font-Size
  generic-font-size-filter #:-> [inheritsize Nonnegative-Flonum] [font-medium Nonnegative-Flonum] Nonnegative-Flonum
  [(xx-large) (* 2/1 font-medium)]
  [(x-large)  (* 3/2 font-medium)]
  [(large)    (* 6/5 font-medium)]
  [(medium)   (* 1/1 font-medium)]
  [(small)    (* 8/9 font-medium)]
  [(x-small)  (* 3/4 font-medium)]
  [(xx-small) (* 3/5 font-medium)]
  [(smaller)  (* 5/6 inheritsize)] ; TODO: find a better function to deal with these two keywords.
  [(larger)   (* 6/5 inheritsize)] ; http://style.cleverchimp.com/font_size_intervals/altintervals.html#bbs
  [#:else     inheritsize])

(define-enumeration* css-font-stretch-option #:+> Font-Stretch ; order matters
  font-stretch->integer integer->font-stretch
  [0 ultra-condensed extra-condensed condensed semi-condensed normal
     semi-expanded expanded extra-expanded ultra-expanded])

(define-enumeration* css-font-style-option #:+> Font-Style ; order matters
  font-style->integer integer->font-style
  [0 normal oblique italic])

(define-enumeration* css-font-weight-option #:+> Font-Weight ; order matters
  font-weight->integer integer->font-weight #:-> Integer
  [thin   100] [ultralight 200] [light    300] [semilight  350] [book 380]
  [normal 400] [medium     500] [semibold 600]
  [bold   700] [ultrabold  800] [heavy    900] [ultraheavy 1000])

(define-enumeration* paragraph-wrap-mode #:+> Paragraph-Wrap-Mode ; order matters
  paragraph-wrap-mode->integer integer->paragraph-wrap-mode
  [-1 none word char word-char])

(define-enumeration* paragraph-ellipsize-mode #:+> Paragraph-Ellipsize-Mode ; order matters
  paragraph-ellipsize-mode->integer integer->paragraph-ellipsize-mode
  [0 none start middle end])

(define-enumeration text-decoration-line : Text-Decoration-Line
  line-through underline underdouble undercurl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define os : Symbol (system-type 'os))

(define select-font-face : (-> (Listof (U String Symbol)) (Option String))
  (lambda [value]
    (let select ([families value])
      (and (pair? families)
           (let ([family (car families)])
             (or (if (symbol? family) (font-family->face family) (face-filter family))
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
      (hash-ref the-face-set (string-downcase face) (λ _ #false)))))

(define memcadr : (-> (Listof Font-Weight) Font-Weight Font-Weight)
  (lambda [srclist v]
    (define tail (memv v srclist))
    (cond [(not tail) v]
          [else (let ([tail (cdr tail)])
                  (if (pair? tail) (car tail) v))])))
