#lang typed/racket/base

(provide (all-defined-out))

(require "../../digitama/unsafe/visual.rkt")

(require racket/file)
(require racket/path)
(require racket/symbol)

(require digimon/dtrace)
(require digimon/digitama/system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics.ext : (->* (Path Symbol String) ((Option Path-String) (Option Path)) Path)
  (lambda [module.rkt sym .ext subid [rootdir #false]]
    (cond [(and rootdir) (graphics-target-path rootdir sym .ext subid)]
          [(digimon-stone-path? module.rkt) (graphics-target-path (or (path-only module.rkt) (current-directory)) sym .ext subid)]
          [else (graphics-target-path/compiled module.rkt sym .ext subid)])))

(define graphics-save-as : (-> Symbol Path Symbol Path Visual-Object<%> Symbol Void)
  (lambda [the-name module.rkt id symbol.ext vobj gformat]
    (make-parent-directory* symbol.ext)
    
    (dtrace-note "~a: ~a: ~a: save-as: ~a" the-name
                 (file-name-from-path module.rkt) id (path->string symbol.ext))
    
    (call-with-output-file* symbol.ext #:exists 'truncate/replace
      (λ [[/dev/stdout : Output-Port]] : Void
        (write-bytes (assert (vobject-convert vobj gformat #"") bytes?) /dev/stdout)
        (void)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-target-path : (-> Path Symbol String (Option Path-String) Path)
  (lambda [rootdir symid .ext subid]
    (build-path rootdir (graphics-subpath #false symid .ext subid))))

(define graphics-target-path/compiled : (-> Path-String Symbol String (Option Path-String) Path)
  (lambda [src.rkt symid .ext subid]
    (build-path (or (path-only src.rkt) (current-directory))
                (car (use-compiled-file-paths)) "graphics"
                (graphics-subpath (path-replace-extension (assert (file-name-from-path src.rkt) path?) #"")
                                  symid .ext subid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define graphics-subpath : (-> (Option Path-String) Symbol String (Option Path-String) Path)
  (lambda [subroot symid .ext subid]
    (define self : Path-String
      (cond [(not subid) (symbol->immutable-string symid)]
            [else (build-path (symbol->immutable-string symid) subid)]))
    
    (path-add-extension (cond [(not subroot) self]
                              [else (build-path subroot self)])
                        .ext)))
