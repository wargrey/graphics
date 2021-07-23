#lang typed/racket/base

(require racket/sequence)

(require png/digitama/stdin)

(require digimon/cmdopt)
(require digimon/dtrace)
(require digimon/filesystem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-cmdlet-option pngdump-flags #: PngDump-Flags
  #:program 'pngdump
  #:args [filename]

  #:once-each
  [[(#\b)                                          "hexdump file content in binary mode"]
   [(#\w)   #:=> cmdopt-string+>byte width #: Byte "hexdump file content in ~1 bytes per line"]
   [(#\v)   #:=> png-verbose                       "run with verbose messages"]])

(define png-verbose : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pndump-on-file : (-> Path Void)
  (lambda [src.png]
    (displayln src.png)))

(define pngdump-main : (-> (U (Listof String) (Vectorof String)) Nothing)
  (lambda [argument-list]
    (define-values (options λargv) (parse-pngdump-flags argument-list #:help-output-port (current-output-port)))
    (define src.txt (λargv))
    
    (parameterize ([current-logger /dev/dtrace])
      (exit (let ([tracer (thread (make-png-log-trace))])
              (cond [(directory-exists? src.txt)
                     (let ([ignore-dir? (make-path-match-predicate (cons #px"/(compiled|[.]DS_Store|[.]git.*)/?$" (use-compiled-file-paths)))])
                       (for-each pndump-on-file
                         (sequence->list
                          (in-directory (path->complete-path src.txt)
                                        (λ [[f : Path]] : Boolean (not (ignore-dir? f)))))))]
                    [else (pndump-on-file (path->complete-path src.txt))])
              
              (dtrace-datum-notice eof)
              (thread-wait tracer))))))

(define make-png-log-trace : (-> (-> Void))
  (lambda []
    (make-dtrace-loop (if (png-verbose) 'trace 'info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (pngdump-main (current-command-line-arguments)))
