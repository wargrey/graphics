#lang typed/racket/base

(provide (all-defined-out))

(require "../save.rkt")
(require "../../../digitama/unsafe/visual.rkt")

(require racket/port)
(require racket/path)
(require racket/list)
(require racket/string)

(require digimon/dtrace)
(require digimon/predicate)

(require digimon/digivice/wisemon/parameter)
(require digimon/digivice/wisemon/ffi)
(require digimon/digivice/wisemon/phony)
(require digimon/digivice/wisemon/spec)
(require digimon/digivice/wisemon/path)
(require digimon/digivice/wisemon/racket)
(require digimon/digivice/wisemon/phony/cc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-module-specs : (-> Path Symbol String Wisemon-Specification Wisemon-Specification)
  (lambda [module.rkt gformat .ext specs]
    (define ns (module->namespace module.rkt))
    (for/fold ([g-specs : Wisemon-Specification specs])
              ([id (in-list (namespace-mapped-symbols ns))])
      (define datum (namespace-variable-value id #false (λ _ #false) ns))
      (append g-specs
              (cond [(visual-object<%>? datum)
                     (let ([deps.rkt (racket-smart-dependencies module.rkt)]
                           [sym.ext (graphics.ext module.rkt id .ext #false)])
                       (list (wisemon-spec sym.ext #:^ (cons module.rkt deps.rkt) #:-
                                           (graphics-save-as module.rkt id sym.ext datum gformat))))]
                    [(and (list? datum) (pair? datum) (andmap visual-object<%>? datum))
                     (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                       (for/list : (Listof Wisemon-Spec) ([subdatum (in-list datum)]
                                                          [idx (in-naturals 1)])
                         (define sym.ext (graphics.ext module.rkt id .ext (number->string idx)))
                         (wisemon-spec sym.ext #:^ (cons module.rkt deps.rkt) #:-
                                       (graphics-save-as module.rkt id sym.ext subdatum gformat))))]
                    [(and (hash? datum) (immutable? datum))
                     (or (let ([content (hash-values datum)])
                           (cond [(andmap visual-object<%>? content)
                                  (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                                    (for/list : (Listof Wisemon-Spec) ([(key subdatum) (in-hash datum)])
                                      (define sym.ext : Path
                                        (graphics.ext module.rkt id .ext
                                                      (string-replace (format "~a" key) "." "_")))
                                      (wisemon-spec sym.ext #:^ (cons module.rkt deps.rkt) #:-
                                                    (graphics-save-as module.rkt id sym.ext (assert subdatum visual-object<%>?) gformat))))]
                                 [(andmap listof-visual-object<%>? content)
                                  (let ([deps.rkt (racket-smart-dependencies module.rkt)])
                                    (apply append
                                           (for/list : (Listof (Listof Wisemon-Spec)) ([key (in-hash-keys datum)]
                                                                                       [subdata (in-list content)])
                                             (for/list : (Listof Wisemon-Spec) ([subdatum (in-list subdata)]
                                                                                [idx (in-naturals 1)])
                                               (define sym.ext : Path
                                                 (graphics.ext module.rkt id .ext
                                                               (string-replace (format "~a-~a" key idx) "." "_")))
                                               (wisemon-spec sym.ext #:^ (cons module.rkt deps.rkt) #:-
                                                             (graphics-save-as module.rkt id sym.ext subdatum gformat))))))]
                                 [else null]))
                         null)]
                    [else null])))))

(define make-graphics-specs : (-> (Option Info-Ref) Symbol String (Listof Path) Wisemon-Specification)
  (lambda [info-ref gformat .ext modules]
    (define /dev/null : Output-Port (open-output-nowhere))
    
    (for/fold ([specs : Wisemon-Specification null])
              ([module.rkt (in-list (cond [(pair? modules) modules]
                                          [(not info-ref) (find-digimon-files graphics-filter (current-directory))]
                                          [else null]))])
      (with-handlers ([exn:fail? (λ [[e : exn:fail]] (dtrace-exception e #:level 'error #:brief? (not (make-verbose))) specs)])
        (parameterize ([current-output-port /dev/null]
                       [current-directory (or (path-only module.rkt) (current-directory))])
          (dynamic-require module.rkt #false))
        
        (make-module-specs module.rkt gformat .ext specs)))))
  
(define make~save-as : (-> Symbol String Make-Free-Phony)
  (lambda [gformat .ext]
    (λ [digimon info-ref]
      (unless (not info-ref)
        (define natives (map (inst car Path CC-Launcher-Info) (find-digimon-native-launcher-names info-ref #false)))
        
        (wisemon-make (make-ffi-library-specs info-ref natives) px.so)
        (wisemon-compile (current-directory) digimon info-ref))

      (define-values (modules targets)
        (partition (λ [[p : Path]] (regexp-match? #px"[.]rkt$" p))
                   (current-make-real-targets)))

      (wisemon-make (make-graphics-specs info-ref gformat .ext modules) targets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listof-visual-object<%>? : (-> Any Boolean : (Listof Visual-Object<%>)) (make-listof? visual-object<%>?))

(define graphics-filter : (-> Path Boolean)
  (lambda [file]
    (and (file-exists? file)
         (with-handlers ([exn:fail? (λ _ #false)])
           (module-declared? file #true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define png-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'png #:phony (make~save-as 'png-bytes ".png") #:desc "Export module-level images as PNGs"))

(define png@2x-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'png@2x #:phony (make~save-as 'png@2x-bytes "@2x.png") #:desc "Export module-level images as 2x PNGs"))

(define svg-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'svg #:phony (make~save-as 'svg-bytes ".svg") #:desc "Export module-level images as SVGs"))

(define pdf-phony-goal : Wisemon-Phony
  (wisemon-make-free-phony #:name 'pdf #:phony (make~save-as 'pdf-bytes ".pdf") #:desc "Export module-level images as PDFs"))
