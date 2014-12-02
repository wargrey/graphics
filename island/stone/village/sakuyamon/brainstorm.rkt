#lang at-exp racket

(require pict)
(require racket/draw)

(define mastering-skills
  (hash '{Science}          (list '{Experimental Psychology} '{Evolutionary Psychology})
        '{Technology}       (list '{Unix} '{Programming Languages})
        '{Engineering}      (list '{Software Engineering} '{Operating and Maintenance})
        '{Mathematics}      (list '{Formal Method} '{Procedural Method} '{Probability})))

(define colors (send the-color-database get-names))
(define group-roots (make-hash))

(define (~pict label)
  (define nodebg (cloud 96 64 (list-ref colors (random (length colors)))))
  (define content (colorize (apply vc-append (map (compose1 text (curry format "~a")) label))
                            (list-ref colors (random (length colors)))))
  (cc-superimpose nodebg (scale-to-fit content (scale nodebg 0.8))))

(define brainstorm-layout
  {λ [parent children start0 end0 offset?]
    (define delta (with-handlers ([exn:fail:contract? (const +NaN.0)]) (/ (- end0 start0) (+ (sequence-length children) (if offset? 1 0)))))
    (define start (+ start0 (if offset? delta 0)))
    (define leaves (cond [(hash? children) (for/list ([{subroot subchildren} (in-hash children)]
                                                      [theta (in-range start +inf.0 delta)])
                                             (define subvalue (~pict subroot))
                                             (define subkey (brainstorm-layout subvalue subchildren (- theta (/ pi 2)) (+ theta (/ pi 2)) #true))
                                             (hash-set! group-roots subkey subvalue) subkey)]
                         [else (map ~pict (if (list? children) children (list children)))]))
    (define-values {pw ph} (values (pict-width parent) (pict-height parent)))
    (define-values {bsfg bsleft bstop bsright bsbottom}
      (for/fold ([bspict parent] [bsx 0] [bsy 0] [bsr pw] [bsb ph]) ([child (in-list leaves)] [theta (in-range start +inf.0 delta)])
        (define-values {cw ch} (values (pict-width child) (pict-height child)))
        (define radius (sqrt (+ (* cw cw) (* ch ch))))
        (define-values {cx cy} (values (* radius (cos theta)) (* radius (sin theta))))
        (values (pin-line (pin-over bspict cx cy child) parent cc-find (hash-ref group-roots child child) cc-find
                          #:under? #true #:line-width 2 #:color (list-ref colors (random (length colors))))
                (min bsx cx) (min bsy cy) (max bsr (+ cx cw)) (max bsb (+ cy ch)))))
    (pin-over (blank (- bsright bsleft) (- bsbottom bstop)) (abs bsleft) (abs bstop) bsfg)})

(define brainstorm (brainstorm-layout (~pict '{Interdiscipline}) mastering-skills 0 (* pi 2) #false))
