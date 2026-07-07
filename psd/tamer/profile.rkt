#lang typed/racket/base

(require psd/base)
(require psd/profile)
(require psd/layer)

(require digimon/debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define argv (current-command-line-arguments))

(define rootdir : Path-String
  (or (and (>= (vector-length argv) 1)
           (let ([rootdir (vector-ref argv 0)])
             (cond [(directory-exists? rootdir) rootdir]
                   [(file-exists? rootdir) rootdir]
                   [else #false])))
      (collection-file-path "samples" "psd" "tamer")))

(define (psd-file? [path : Path-String]) (regexp-match? #px"[.]ps[bd]$" path))

(define psds : (Listof PSD)
  (filter psd?
          (for/list : (Listof Any) ([file.psd (if (directory-exists? rootdir) (in-directory rootdir) (in-value rootdir))]
                                    #:when (psd-file? file.psd))
            (with-handlers ([exn:fail? (λ [[e : exn:fail]] (eprintf "~a: ~a~n" (object-name e) (exn-message e)))])
              (time* (read-psd file.psd #:try-@2x? #false #:hdr-expose 3.0))))))
          
(if (regexp-match? #px"DrRacket[.]app" (find-system-path 'exec-file))
    (for/list : (Listof Any) ([tamer.psd (in-list psds)]
                              [idx (in-naturals 1)])
      (newline)
      (psd-profile tamer.psd #:full? #true)

      (list idx (psd-file-name tamer.psd)
            (psd-layers tamer.psd)
            (psd-global-mask tamer.psd)
            tamer.psd
            (psd-image-resources tamer.psd)))
    (for ([tamer.psd (in-list psds)])
      (psd-profile tamer.psd #:full? #false)))
