#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

(require "unsafe/font.rkt")

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(define-enumeration* [css-font-generic-family css-font-generic-families] #:as Font-Family 
  font-family->face #:-> String
  [(sans-serif) (case os [(macosx) "Lucida Grande"] [(windows) "Microsoft YaHei"] [else "Nimbus Sans"])]
  [(serif)      (case os [(macosx) "Times"] [(windows) "Times New Roman"] [else "DejaVu Serif"])]
  [(monospace)  (case os [(macosx) "Menlo"] [(windows) "KaiTi"] [else "Monospace"])]
  [(fantasy)    (case os [(macosx) "Comic Sans MS"] [(windows) "Comic Sans MS"] [else "Helvetica"])]
  [(cursive)    (case os [(macosx) "Kokonor"] [(windows) "Palatino Linotype, Italic"] [else "Chancery"])]
  [(system-ui)  (system-ui 'normal-control-font (λ [] (case os [(macosx) "Helvetica Neue"] [(windows) "Verdana"] [else "Sans"])))]
  [(emoji)      (case os [(macosx) "GB18030 Bitmap"] [(windows) "Algerian"] [else "Symbol"])]
  [(math)       (case os [(macosx) "Bodoni 72, Book Italic"] [(windows) "Bodoni MT, Italic"] [else "URW Bookman, Italic"])]
  [(fangsong)   (case os [(macosx) "ST FangSong"] [(windows) "FangSong"] [else "FangSong"])])

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
  font-weight->integer integer->font-weight
  #:range
  [thin   100] [ultralight 200] [light    300] [semilight  350] [book 380]
  [normal 400] [medium     500] [semibold 600]
  [bold   700] [ultrabold  800] [heavy    900] [ultraheavy 1000])

(define-enumeration* css-font-variant-option #:+> Font-Variant ; order matters
  font-variant->integer integer->font-variant
  [0 normal small-caps])

(define-enumeration* paragraph-wrap-mode #:+> Paragraph-Wrap-Mode ; order matters
  paragraph-wrap-mode->integer integer->paragraph-wrap-mode
  [-1 none word char word-char])

(define-enumeration* paragraph-ellipsize-mode #:+> Paragraph-Ellipsize-Mode ; order matters
  paragraph-ellipsize-mode->integer integer->paragraph-ellipsize-mode
  [0 none start middle end])

(define-enumeration text-decoration-line : Text-Decoration-Line
  [line-through underline underdouble undercurl])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define os : Symbol (system-type 'os))

(define list-font-families : (-> (Listof String))
  (lambda []
    (let families++ ([fobjects : (Listof Font-Raw-Family) (font_list_families)]
                     [families : (Listof String) null])
      (cond [(null? fobjects) (sort families string<?)]
            [else (families++ (cdr fobjects) (cons (pango_font_family_get_name (car fobjects)) families))]))))

(define list-monospace-font-families : (-> (Listof String))
  (lambda []
    (let families++ ([fobjects : (Listof Font-Raw-Family) (font_list_families)]
                     [families : (Listof String) null])
      (cond [(null? fobjects) (sort families string<?)]
            [else (let ([family (car fobjects)])
                    (families++ (cdr fobjects)
                                (cond [(not (pango_font_family_is_monospace family)) families]
                                      [else (cons (pango_font_family_get_name family) families)])))]))))

(define list-font-faces : (-> (Listof String))
  (lambda []
    (let face++ ([fobjects : (Listof Font-Raw-Family) (font_list_families)]
                 [faces : (Listof String) null])
      (cond [(null? fobjects) (sort faces string<?)]
            [else (face++ (cdr fobjects) (family-faces++ (car fobjects) faces))]))))

(define list-monospace-font-faces : (-> (Listof String))
  (lambda []
    (let face++ ([fobjects : (Listof Font-Raw-Family) (font_list_families)]
                 [faces : (Listof String) null])
      (cond [(null? fobjects) (sort faces string<?)]
            [else (let* ([family (car fobjects)])
                    (face++ (cdr fobjects)
                            (cond [(not (pango_font_family_is_monospace family)) faces]
                                  [else (family-faces++ family faces)])))]))))

(define filter-font-families : (-> (-> String Boolean Boolean) (Listof String))
  (lambda [pred?]
    (let families++ ([fobjects : (Listof Font-Raw-Family) (font_list_families)]
                     [families : (Listof String) null])
      (cond [(null? fobjects) (sort families string<?)]
            [else (let* ([family (car fobjects)]
                         [fname (pango_font_family_get_name family)])
                    (families++ (cdr fobjects)
                                (cond [(pred? fname (pango_font_family_is_monospace family)) (cons fname families)]
                                      [else families])))]))))

(define select-font-face : (-> (Listof (U String Symbol)) (Option String))
  (lambda [value]
    (let select ([families value])
      (and (pair? families)
           (let ([family (car families)])
             (or (if (symbol? family) (font-family->face family) (face-filter family))
                 (select (cdr families))))))))

(define face-filter : (-> String (Option String))
  (let ([&families : (Boxof (Option (HashTable String String))) (box #false)]
        [&faces : (Boxof (Option (HashTable String String))) (box #false)])
    (lambda [face]
      (define-values (&pool ls) (if (regexp-match? #rx"," face) (values &faces list-font-faces) (values &families list-font-families)))
      (define the-face-set : (HashTable String String)
        (or (unbox &pool)
            (let ([the-set (for/hash : (HashTable String String) ([face (in-list (ls))])
                             (values (string-downcase face) face))])
              (set-box! &pool the-set)
              the-set)))
      (hash-ref the-face-set (string-downcase face) (λ _ #false)))))

(define memcadr : (-> (Listof Font-Weight) Font-Weight Font-Weight)
  (lambda [srclist v]
    (define tail (memv v srclist))
    (cond [(not tail) v]
          [else (let ([tail (cdr tail)])
                  (if (pair? tail) (car tail) v))])))
