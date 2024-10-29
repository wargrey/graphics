#lang typed/racket/base

(provide (all-defined-out))

(require "../path.rkt")
(require "../../../digitama/unsafe/visual.rkt")

(require bitmap/composite)
(require bitmap/digitama/convert)

(require racket/port)
(require racket/math)
(require racket/file)

(require digimon/dtrace)
(require digimon/digitama/system)

(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/ffi)
(require digimon/digivice/wisemon/phony)
(require digimon/digivice/wisemon/spec)
(require digimon/digivice/wisemon/path)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/phony/cc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-graphics-specs : (-> Info-Ref Symbol String Wisemon-Specification)
  (lambda [info-ref gformat .ext]
    (define /dev/null : Output-Port (open-output-nowhere))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([module.rkt (in-list (find-digimon-files graphics-filter (current-directory)))])
      (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:level 'error #:brief? (not (make-verbose))) specs)])
        (parameterize ([current-output-port /dev/null])
          (dynamic-require module.rkt #false))
      
        (define ns (module->namespace module.rkt))
        (for/fold ([g-specs : Wisemon-Specification specs])
                  ([sym (in-list (namespace-mapped-symbols ns))])
          (define datum (namespace-variable-value sym #false (λ _ #false) ns))
          (append g-specs
                  (cond [(visual-object<%>? datum)
                         (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                               [sym.png (graphics.ext module.rkt sym .ext)])
                           (list (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                               (graphics-note module.rkt sym .ext sym.png)
                                               (graphics-save-as sym.png datum gformat))))]
                        [(and (list? datum) (pair? datum) (andmap bitmap? datum))
                         (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                               [sym.png (graphics.ext module.rkt sym .ext)]
                               [ncol (max (exact-floor (sqrt (length datum))) 1)])
                           (list (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                               (graphics-note module.rkt sym .ext sym.png)
                                               (graphics-save-as sym.png (bitmap-table ncol datum) gformat))))]
                        [else null])))))))
  
(define make~save-as : (-> Symbol String Make-Info-Phony)
  (lambda [gformat .ext]
    (λ [digimon info-ref]
      (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))

      (wisemon-make (make-ffi-library-specs info-ref natives) px.so)
      (wisemon-compile (current-directory) digimon info-ref)
      
      (wisemon-make (make-graphics-specs info-ref gformat .ext) (current-make-real-targets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-filter : (-> Path Boolean)
  (lambda [file]
    (and (file-exists? file)
         (with-handlers ([exn:fail? (λ _ #false)])
           (module-declared? file #true)))))

(define graphics.ext : (-> Path Symbol String Path)
  (lambda [module.rkt sym .ext]
    (if (digimon-stone-path? module.rkt)
        (graphics-target-path module.rkt sym .ext)
        (graphics-target-path/compiled module.rkt sym .ext))))

(define graphics-note : (-> Path Symbol String Path Void)
  (lambda [module.rkt sym .ext symbol.png]
    (dtrace-note "~a: save-as: ~a" the-name (path->string symbol.png))))

(define graphics-save-as : (-> Path Visual-Object<%> Symbol Void)
  (lambda [sym.png vobj gformat]
    (make-parent-directory* sym.png)
    
    (call-with-output-file* sym.png #:exists 'truncate/replace
      (λ [[/dev/stdout : Output-Port]] : Void
        (write-bytes (assert (vobject-convert vobj gformat #"") bytes?) /dev/stdout)
        (void)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define png-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'png #:phony (make~save-as 'png-bytes ".png") #:desc "Export module-level images as PNGs"))

(define svg-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'svg #:phony (make~save-as 'svg-bytes ".svg") #:desc "Export module-level images as SVGs"))

(define pdf-phony-goal : Wisemon-Phony
  (wisemon-make-info-phony #:name 'pdf #:phony (make~save-as 'pdf-bytes ".pdf") #:desc "Export module-level images as PDFs"))
