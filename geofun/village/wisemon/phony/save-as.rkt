#lang typed/racket/base

(provide (all-defined-out))

(require "../path.rkt")
(require "../../../digitama/unsafe/visual.rkt")

(require racket/port)
(require racket/file)
(require racket/path)
(require racket/string)

(require digimon/dtrace)
(require digimon/predicate)
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
        (parameterize ([current-output-port /dev/null]
                       [current-directory (or (path-only module.rkt) (current-directory))])
          (dynamic-require module.rkt #false))
      
        (define ns (module->namespace module.rkt))
        (for/fold ([g-specs : Wisemon-Specification specs])
                  ([sym (in-list (namespace-mapped-symbols ns))])
          (define datum (namespace-variable-value sym #false (λ _ #false) ns))
          (append g-specs
                  (cond [(visual-object<%>? datum)
                         (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                               [sym.png (graphics.ext module.rkt sym .ext #false)])
                           (list (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                               (graphics-note module.rkt sym .ext sym.png)
                                               (graphics-save-as sym.png datum gformat))))]
                        [(and (list? datum) (pair? datum) (andmap visual-object<%>? datum))
                         (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                           (for/list : (Listof Wisemon-Spec) ([subdatum (in-list datum)]
                                                              [idx (in-naturals 1)])
                             (define sym.png (graphics.ext module.rkt sym .ext (number->string idx)))
                             (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                           (graphics-note module.rkt sym .ext sym.png)
                                           (graphics-save-as sym.png subdatum gformat))))]
                        [(and (hash? datum) (immutable? datum))
                         (or (let ([content (hash-values datum)])
                               (cond [(andmap visual-object<%>? content)
                                      (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                                        (for/list : (Listof Wisemon-Spec) ([(key subdatum) (in-hash datum)])
                                          (define sym.png : Path
                                            (graphics.ext module.rkt sym .ext
                                                          (string-replace (format "~a" key) "." "_")))
                                          (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                                        (graphics-note module.rkt sym .ext sym.png)
                                                        (graphics-save-as sym.png (assert subdatum visual-object<%>?) gformat))))]
                                     [(andmap listof-visual-object<%>? content)
                                      (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                                        (apply append
                                               (for/list : (Listof (Listof Wisemon-Spec)) ([key (in-hash-keys datum)]
                                                                                           [subdata (in-list content)])
                                                 (for/list : (Listof Wisemon-Spec) ([subdatum (in-list subdata)]
                                                                                    [idx (in-naturals 1)])
                                                   (define sym.png : Path
                                                     (graphics.ext module.rkt sym .ext
                                                                   (string-replace (format "~a-~a" key idx) "." "_")))
                                                   (wisemon-spec sym.png #:^ (cons module.rkt deps.rkt) #:-
                                                                 (graphics-note module.rkt sym .ext sym.png)
                                                                 (graphics-save-as sym.png subdatum gformat))))))]
                                     [else null]))
                             null)]
                        [else null])))))))
  
(define make~save-as : (-> Symbol String Make-Info-Phony)
  (lambda [gformat .ext]
    (λ [digimon info-ref]
      (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))

      (wisemon-make (make-ffi-library-specs info-ref natives) px.so)
      (wisemon-compile (current-directory) digimon info-ref)
      
      (wisemon-make (make-graphics-specs info-ref gformat .ext) (current-make-real-targets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listof-visual-object<%>? : (-> Any Boolean : (Listof Visual-Object<%>)) (make-listof? visual-object<%>?))

(define graphics-filter : (-> Path Boolean)
  (lambda [file]
    (and (file-exists? file)
         (with-handlers ([exn:fail? (λ _ #false)])
           (module-declared? file #true)))))

(define graphics.ext : (-> Path Symbol String (Option Path-String) Path)
  (lambda [module.rkt sym .ext subid]
    (if (digimon-stone-path? module.rkt)
        (graphics-target-path module.rkt sym .ext subid)
        (graphics-target-path/compiled module.rkt sym .ext subid))))

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
