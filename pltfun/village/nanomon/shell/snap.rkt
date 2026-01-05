#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)

(require pltfun/memory)
(require pltfun/digitama/ram/style)
(require geofun/village/wisemon/save)

(require digimon/digivice/nanomon/shell)
(require digimon/digitama/exec)
(require digimon/cmdopt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-shell : Symbol 'snap)
(define the-desc : String "run and take RAM snapshots of a C/C++ program")

(define snap-output-pdf? : (Parameterof Boolean) (make-parameter #false))
(define snap-output-svg? : (Parameterof Boolean) (make-parameter #false))
(define snap-output-png? : (Parameterof Boolean) (make-parameter #false))

(define snap-split-table? : (Parameterof Boolean) (make-parameter #false))
(define snap-open-file? : (Parameterof Boolean) (make-parameter #false))
(define snap-no-segment? : (Parameterof Boolean) (make-parameter #false))
(define snap-no-states? : (Parameterof Boolean) (make-parameter #false))

(define-cmdlet-option snap-flags #: Snap-Flags
  #:program the-shell
  #:args [src.c . argv]

  #:usage-help the-desc
  #:once-each
  [[(entry main)              #:=> cmdopt-string->symbol entry #: Symbol
                              ["use ~1 as the default entry function (default: ~a)" (default-ram-entry)]]
   [(ahead)                   #:=> cmdopt-string->index size #: Index
                              ["take ~1-byte more ahead for each segment (default: ~a)" (default-ram-lookahead-size)]]
   [(behind)                  #:=> cmdopt-string->index size #: Index
                              ["take ~1-byte more behind for each segment (default: ~a)" (default-ram-lookbehind-size)]]
   [(body-limit)              #:=> cmdopt-string->index size #: Index
                              ["restricted the body size upto ~1 bytes for each segment (default: ~a)" (default-ram-body-limit)]]
   [(#\O O2)                  #:=> default-ram-optimize?
                              "run optimized program"]

   [(#\h human-readable)      #:=> default-ram-human-readable?
                              "display values for humen to read"]
   [(no-padding no-pad)       #:=> default-ram-no-padding?
                              "ignore all padding locations"]
   [(padding-limit pad-limit) #:=> cmdopt-string->index size #: Index
                              ["cascade consecutive padding locations when exceeding ~1 bytes (default: ~a)" (default-ram-padding-limit)]]
   [(mask)                    #:=> cmdopt-string->natural F #: Natural
                              ["use ~1 as the mask of address (default: #x~a)" (~r (default-ram-address-mask) #:base '(up 16))]]

   [(rd-radix rd-base)        #:=> cmdopt-string+>radix N #: Positive-Byte
                              ["display raw data in base-~1 (default: ~a)" (default-ram-raw-data-radix)]]
   [(fx-radix fx-base)        #:=> cmdopt-string+>radix N #: Positive-Byte
                              ["display integer values in base-~1 (default: ~a)" (default-ram-fixnum-radix)]]
   [(pad-radix pad-base)      #:=> cmdopt-string+>radix N #: Positive-Byte
                              ["display padding in base-~1 (default: ~a)" (default-ram-padding-radix)]]

   [(segment-gap seg-gap)     #:=> cmdopt-string+>flonum size #: Nonnegative-Flonum
                              ["use ~1 as the gapsize of segments (default: ~a)" (default-ram-segment-gapsize)]]
   [(snapshot-gap snap-gap)   #:=> cmdopt-string+>flonum size #: Nonnegative-Flonum
                              ["use ~1 as the gapsize of snapshots (default: ~a)" (default-ram-snapshot-gapsize)]]

   [(no-segment hide-segment) #:=> snap-no-segment?                "hide segment names"]
   [(no-state hide-state)     #:=> snap-no-states?                 "hide states"]

   [(#\d dest)                #:=> cmdopt-string->path dir #: Path "write output to ~1"]
   [(pdf)                     #:=> snap-output-pdf?                "save snapshots as PDFs (default)"]
   [(svg)                     #:=> snap-output-svg?                "save snapshots as SVGs"]
   [(png)                     #:=> snap-output-png?                "save snapshots as PNGs"]
   [(split)                   #:=> snap-split-table?               "split snapshots into subdirectories"]
   [(show)                    #:=> snap-open-file?                 "open graphics after saving"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell-snap : (-> Path Snap-Flags (Listof String) Any)
  (lambda [src.c options argv]
    (define entry : Symbol (or (snap-flags-entry options) (default-ram-entry)))
    (define destdir : (Option Path) (snap-flags-dest options))
    
    (define snapshots
      (parameterize ([default-ram-padding-limit (or (snap-flags-padding-limit options) (default-ram-padding-limit))]
                     [default-ram-address-mask (or (snap-flags-mask options) (default-ram-address-mask))]
                     [default-ram-raw-data-radix (or (snap-flags-rd-radix options) (default-ram-raw-data-radix))]
                     [default-ram-fixnum-radix (or (snap-flags-fx-radix options) (default-ram-fixnum-radix))]
                     [default-ram-padding-radix (or (snap-flags-pad-radix options) (default-ram-padding-radix))])
        (plt-ram-snapshots #:main entry #:c-argv argv
                           #:lookahead-size (or (snap-flags-ahead options) (default-ram-lookahead-size))
                           #:lookbehind-size (or (snap-flags-behind options) (default-ram-lookbehind-size))
                           #:body-limit (or (snap-flags-body-limit options) (default-ram-body-limit))
                           src.c)))

    (when (snap-split-table?)
      (for ([(segment subsnaps) (in-hash snapshots)])
        (for ([snapshot (in-list subsnaps)]
              [idx (in-naturals 1)])
          (shell-snap-save src.c entry destdir snapshot (string-replace (format "~a-~a" segment idx) "." "_")))))

    (define snapshot-table
      (plt-ram-snapshots->table #:hide-segment-names? (snap-no-segment?)
                                   #:hide-states? (snap-no-states?)
                                   snapshots))
    
    (define maybe-graphics (shell-snap-save src.c entry destdir snapshot-table #false))

    (when (snap-open-file?)
      (define graphics (filter path? maybe-graphics))

      (when (pair? graphics)
        (fg-recon-open-file 'exec (car graphics))))))

(define shell-snap-save : (-> Path Symbol (Option Path) Geo (Option String) (Listof (Option Path)))
  (lambda [src.c entry destdir snapshot subpath]
    (define-values (maybe.pdf maybe.svg maybe.png)
      (if (or (snap-output-pdf?) (snap-output-svg?) (snap-output-svg?))
          (values (and (snap-output-pdf?) (graphics.ext src.c entry ".pdf" subpath destdir))
                  (and (snap-output-svg?) (graphics.ext src.c entry ".svg" subpath destdir))
                  (and (snap-output-png?) (graphics.ext src.c entry ".png" subpath destdir)))
          (values (graphics.ext src.c entry ".pdf" subpath destdir)
                  #false
                  #false)))
    
    (when (and maybe.pdf) (graphics-save-as src.c entry maybe.pdf snapshot 'pdf-bytes))
    (when (and maybe.svg) (graphics-save-as src.c entry maybe.svg snapshot 'svg-bytes))
    (when (and maybe.png) (graphics-save-as src.c entry maybe.png snapshot 'png-bytes))

    (list maybe.pdf maybe.svg maybe.png)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define shell~~snap : (-> (Listof String) Thread Any)
  (lambda [argv env-thread]
    (define-values (options λargv) (parse-snap-flags argv))

    (if (not (snap-flags-help? options))
        (let-values ([(src.c argv) (λargv)])
          (shell-snap (cmdopt-string->path the-shell src.c) options argv))
        (display-snap-flags #:exit #false))
    (thread-send env-thread 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define snap-shell : Nanomon-Shell
  (nanomon-make-shell #:name the-shell
                      #:shell shell~~snap
                      #:desc the-desc))
