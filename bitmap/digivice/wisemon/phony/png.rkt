#lang typed/racket/base

(provide (all-defined-out))

(require "../path.rkt")
(require "../../../base.rkt")

(require racket/port)
(require racket/math)

(require digimon/dtrace)
(require digimon/digitama/system)

(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/native)
(require digimon/digivice/wisemon/phony)
(require digimon/digivice/wisemon/spec)
(require digimon/digivice/wisemon/path)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/phony/cc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-graphics-specs : (->* (Info-Ref) (Bytes) Wisemon-Specification)
  (lambda [info-ref [.ext #".png"]]
    (define /dev/null : Output-Port (open-output-nowhere))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([module.rkt (in-list (find-digimon-files graphics-filter (current-directory)))])
      (parameterize ([current-output-port /dev/null])
        (dynamic-require module.rkt #false))

      (define ns (module->namespace module.rkt))
      (for/fold ([g-specs : Wisemon-Specification specs])
                ([sym (in-list (namespace-mapped-symbols ns))])
        (define datum (namespace-variable-value sym #false (λ _ #false) ns))
        (append g-specs
                (cond [(bitmap? datum)
                       (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                             [sym.png (graphics.png module.rkt sym .ext)])
                         (list (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                             (graphics-note module.rkt sym .ext sym.png)
                                             (bitmap-save datum sym.png #:format 'png))))]
                      [(and (list? datum) (andmap bitmap? datum))
                       (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                             [sym.png (graphics.png module.rkt sym .ext)]
                             [ncol (max (exact-floor (sqrt (length datum))) 1)])
                         (list (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                             (graphics-note module.rkt sym .ext sym.png)
                                             (bitmap-save (bitmap-table ncol datum) sym.png #:format 'png))))]
                      [else null]))))))
  
(define make~png : Make-Phony
  (lambda [digimon info-ref]
    (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref)))
    
    (wisemon-make (make-native-library-specs info-ref natives))
    (wisemon-compile (current-directory) digimon info-ref)

    (wisemon-make (make-graphics-specs info-ref) (current-make-real-targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-filter : (-> Path Boolean)
  (lambda [file]
    (and (file-exists? file)
         (module-declared? file #true))))

(define graphics.png : (-> Path Symbol Bytes Path)
  (lambda [module.rkt sym .ext]
    (if (digimon-stone-path? module.rkt)
        (graphics-target-path module.rkt sym .ext)
        (graphics-target-path/compiled module.rkt sym .ext))))

(define graphics-note : (-> Path Symbol Bytes Path Void)
  (lambda [module.rkt sym .ext symbol.png]
    (dtrace-note "~a: save-as: ~a" the-name (path->string symbol.png))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define png-phony-goal : Wisemon-Phony
  (wisemon-make-phony #:name 'png #:phony make~png #:desc "Export module-level images as PNGs"))
