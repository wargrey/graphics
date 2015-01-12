#lang racket

(provide (all-defined-out))

(define-values {digimon-world digimon-gnome}
  (let* ([dir (path->string (path-only (syntax-source #'runtimepath)))]
         [px.split (regexp-match #px"(.+)/([^/]+?)/[^/]+?/?$" dir)])
    (values (second px.split) (third px.split))))

(current-library-collection-paths (cons digimon-world (current-library-collection-paths)))

(void (putenv "digimon-world" digimon-world)
      (putenv "digimon-gnome" digimon-gnome))

(define digimon-path
  {lambda [pathname #:digimon [diginame digimon-gnome]]
    (build-path digimon-world diginame pathname)})

(define digimon-setenv
  {lambda [digimon]
    (putenv "digimon-zone" (path->string (build-path digimon-world digimon)))
    (for ([pathname (in-list (list "digivice" "digitam" "tamer" "stone"))])
      (putenv (format "digimon-~a" pathname) (path->string (digimon-path pathname #:digimon digimon))))})
