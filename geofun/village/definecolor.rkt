#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/list)

(require digimon/cmdopt)
(require digimon/format)
(require digimon/json)

(require geofun/color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hexadecimal->rgba : (-> Symbol String String (Pairof FlRGBA String))
  (lambda [option str.v str.n]
    (define hex : (Option Number) (string->number (string-trim str.v #px"^((#|0)x?)?") 16))

    (cond [(index? hex) (cons (rgb* hex) str.n)]
          [else (error option "expected hexadecimal, given ~a" str.v)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option cc-flags #: CC-Flags
  #:usage-help "define a color for latex based on existed color values"
  #:args []
  #:once-each
  [[(prefix) pfx "name prefix"]
   [(suffix) sfx "name suffix"]
   [(json) #:=> cmdopt-string->path path #: Path "vscode theme file"]]
  #:multi
  [[(#\h hex) #:=> hexadecimal->rgba hcolor name #: (Pairof FlRGBA String) "hexadecimal color value"]])

(struct vscode-theme
  ([name : String]
   [color : Keyword]
   [style : (Option String)])
  #:transparent
  #:type-name VSCode-Theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main
  (lambda []
    (define-values (options λargv) (parse-cc-flags #:help-output-port (current-output-port)))

    (with-handlers ([exn:fail:user? (λ [[e : exn:fail:user]] (display-cc-flags #:user-error e #:exit 1))])
      (define pfx : (Option String) (cc-flags-prefix options))
      (define sfx : (Option String) (cc-flags-suffix options))
      
      (for-each displayln
        (text-table-rows
         (append (let ([theme.json (cc-flags-json options)])
                   (or (and theme.json
                            (let* ([theme (assert (read-json theme.json) hash?)]
                                   [tclrs (filter-map vscode-theme-setting (assert (hash-ref theme 'tokenColors) list?))])
                              (for/list : (Listof (Listof String)) ([tc (in-list ((inst sort VSCode-Theme String) tclrs string<? #:key vscode-theme-name))])
                                (defcolor-row (rgb* (vscode-theme-color tc)) (vscode-theme-name tc)
                                  pfx sfx (vscode-theme-style tc)))))
                       null))
                 
                 (for/list : (Listof (Listof String)) ([cn (in-list (cc-flags-hex options))])
                   (defcolor-row (car cn) (cdr cn) pfx sfx))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vscode-theme-setting : (-> Any (Option VSCode-Theme))
  (lambda [src]
    (and (hash? src)
         (let* ([name (hash-ref src 'name (λ [] #false))]
                [settings (assert (hash-ref src 'settings hash) hash?)]
                [color (hash-ref settings 'foreground (λ [] #false))]
                [style (hash-ref settings 'fontStyle (λ [] #false))])
           (and (string? name)
                (string? color)
                (vscode-theme (string-replace (string-titlecase (string-replace name #px"\\W" " ")) " " "")
                              (string->keyword (substring color 1))
                              (and (string? style) style)))))))

(define defcolor-row : (->* (FlRGBA String (Option String) (Option String)) ((Option String)) (Listof String))
  (lambda [c name pfx sfx [style #false]]
    (list (format "\\definecolor{~a}{rgb}{~a, ~a, ~a}"
            (apply string-append (filter string? (list pfx name sfx)))
            (rgba-red c) (rgba-green c) (rgba-blue c))
          
          (format "% #~a~a"
            (~r (flcolor->hex c) #:base '(up 16) #:min-width 6 #:pad-string "0")
            (flcolor->byte-list c))

          #;(format "alpha: ~a" (rgba-alpha c))
          (or style ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(main)
