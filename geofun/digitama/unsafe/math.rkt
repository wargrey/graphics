#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)

  (require ffi/unsafe)
  (require ffi/unsafe/atomic)
  
  (require "pango.rkt")
  (require (submod "font.rkt" unsafe))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (list_math_font_families content)
    (define layout (the-layout))
    (define desc (pango_font_description_new))

    (pango_layout_set_text layout content)

    (let families++ ([fobjects (font_list_families)]
                     [families null])
      (cond [(null? fobjects) (sort families string<?)]
            [else (let* ([name (pango_font_family_get_name (unsafe-car fobjects))])
                    (pango_font_description_set_family desc name)
                    (pango_layout_set_font_description layout desc)
                    (families++ (unsafe-cdr fobjects)
                                (if (= (pango_layout_get_unknown_glyphs_count layout) 0)
                                    (unsafe-cons-list name families)
                                    families)))])))

  (define (list_math_font_faces content)
    (define layout (the-layout))
    
    (pango_layout_set_text layout content)

    (let faces++ ([all-faces (font_list_faces)]
                  [faces null])
      (cond [(null? all-faces) (sort faces string<?)]
            [else (let* ([name (unsafe-car all-faces)]
                         [desc (pango_font_description_from_string name)])
                    (pango_layout_set_font_description layout desc)
                    (faces++ (unsafe-cdr all-faces)
                             (if (= (pango_layout_get_unknown_glyphs_count layout) 0)
                                 (unsafe-cons-list name faces)
                                 faces)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [list_math_font_faces (-> String (Listof String))]
 [list_math_font_families (-> String (Listof String))])
