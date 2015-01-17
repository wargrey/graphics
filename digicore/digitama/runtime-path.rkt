#lang racket/base

(require racket/path)
(require racket/runtime-path)

(provide (except-out (all-defined-out) compiled-syntax-source-directory))

(define-runtime-path compiled-syntax-source-directory ".")

(define-values {digimon-world digimon-kernel digimon-gnome}
  (let* ([dir (path->string (simplify-path compiled-syntax-source-directory))]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/?$" dir)])
    (values (cadr px.split) (caddr px.split) "DigiGnome")))

(unless (member digimon-world (current-library-collection-paths))
  (current-library-collection-paths (cons digimon-world (current-library-collection-paths))))

(void (putenv "digimon-world" digimon-world)
      (putenv "digimon-gnome" digimon-gnome)
      (putenv "digimon-kernel" digimon-kernel))

(define digimon-setenv
  {lambda [digimon]
    (putenv "digimon-zone" (path->string (build-path digimon-world digimon)))
    (for ([pathname (in-list (list "digivice" "digitam" "tamer" "stone"))])
      (putenv (format "digimon-~a" pathname) (path->string (digimon-path pathname #:digimon digimon))))})

(define digimon-path
  {lambda [pathname #:digimon [diginame digimon-gnome]]
    (build-path digimon-world diginame pathname)})

(define path->digimon-libpath
  {lambda [pathname #:submodule [subname #false]]
    (define fname (path->string (find-relative-path digimon-world (simplify-path pathname))))
    (if (symbol? subname) `(submod (lib ,fname) ,subname) `(lib ,fname))})
