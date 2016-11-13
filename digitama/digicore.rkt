#lang at-exp typed/racket

(define boot-time : Fixnum (current-milliseconds))

(provide (all-defined-out) Term-Color vim-colors Racket-Place-Status Racket-Thread-Status)
(provide (all-from-out "sugar.rkt" "format.rkt" "ugly.rkt" "uuid.rkt"))
(provide (all-from-out (submod "openssl.rkt" typed/ffi)))

(require (for-syntax racket/syntax))

@require{sugar.rkt}
@require{format.rkt}
@require{ugly.rkt}
@require{uuid.rkt}

@require[(submod "openssl.rkt" typed/ffi)]

(define-type Racket-Main (-> String * Void))
(define-type Place-Main (-> Place-Channel Void))
(define-type Stack-Hint (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))))
(define-type Help-Table (Listof (U (List Symbol String) (List* Symbol (Listof (List (Listof String) Any (Listof String)))))))

(define-type EvtSelf (Rec Evt (Evtof Evt)))
(define-type Place-EvtExit (Evtof (Pairof Place Integer)))
(define-type Timer-EvtSelf (Rec Timer-Evt (Evtof (Vector Timer-Evt Fixnum Fixnum))))

(define /dev/stdin : Input-Port (current-input-port))
(define /dev/stdout : Output-Port (current-output-port))
(define /dev/stderr : Output-Port (current-error-port))
(define /dev/log : Logger (make-logger 'digital-world #false))
(define /dev/eof : Input-Port (open-input-bytes #"" '/dev/null))
(define /dev/null : Output-Port (open-output-nowhere '/dev/null))

(define /dev/zero : Input-Port
  (make-input-port '/dev/zero
                   (λ [[bs : Bytes]]
                     (bytes-fill! bs #x00)
                     (bytes-length bs))
                   #false
                   void))

(define /dev/urandom : Input-Port
  (make-input-port '/dev/urandom
                   (λ [[bs : Bytes]]
                     (let ([bsize (bytes-length bs)])
                       (bytes-copy! bs 0 (crypto-random-bytes bsize))
                       bsize))
                   #false
                   void))

(define digicore.rkt : Path (#%file))

(define digimon-world : (Parameterof Nothing String)
  (make-parameter (path->string (simple-form-path (build-path digicore.rkt 'up 'up 'up)))
                  (immutable-guard 'digimon-world)))

(define digimon-gnome : (Parameterof Nothing String)
  (make-parameter (path->string (last (drop-right (filter path? (explode-path digicore.rkt)) 2)))
                  (immutable-guard 'digimon-gnome)))

(define digimon-kuzuhamon : (Parameterof Nothing String) (make-parameter "kuzuhamon" (immutable-guard 'digimon-kuzuhamon)))
(define current-digimon : (Parameterof String String) (make-parameter (digimon-gnome)))
(define current-tamer : (Parameterof Nothing String) (make-parameter (or (getenv "USER") (getenv "LOGNAME") #| daemon |# "root")))

(define digimon-system : (Parameterof Nothing Symbol)
  (make-parameter (match (path->string (system-library-subpath #false))
                    ;;; (system-type 'machine) might lead to "forbidden exec /bin/uname" 
                    [(pregexp #px"solaris") 'illumos]
                    [(pregexp #px"linux") 'linux]
                    [_ (system-type 'os)])))

(define digimon-zone : (Parameterof Nothing Path)
  (make-derived-parameter current-digimon
                          (immutable-guard 'digimon-zone)
                          (curry build-path (digimon-world))))

(define/extract-info (digimon-world) :- (make-parameter Parameterof)
  [pkg-institution : (Option String) = #false]
  [pkg-domain : String = "gyoudmon.org"]
  [pkg-idun : String = (current-tamer)])

(define all-digimons : (Listof String)
  (let* ([dirnames : (Listof String) (map path->string (directory-list (digimon-world) #:build? #false))]
         [top-ref : (Option Info-Ref) (get-info/full (digimon-world) #:bootstrap? #true)]
         [candidates : (Listof String) (cond [(false? top-ref) null]
                                             [else (cast (top-ref 'setup-collects (λ _ dirnames)) (Listof String))])])
    (remove-duplicates (filter string? (for/list : (Listof (Option String)) ([digimon (in-list (cons (digimon-gnome) candidates))])
                                         (define info-ref (get-info/full (build-path (digimon-world) digimon) #:bootstrap? #true)) 
                                         (and (procedure? info-ref)
                                              (hash-ref! infobase digimon (thunk info-ref))
                                                digimon))))))

(define #%info : (->* (Symbol) ((Option (-> Any)) #:digimon String) Any)
  (lambda [key [defval #false] #:digimon [digimon (current-digimon)]]
    (define (try-setinfo)
      (define info-ref (get-info/full (build-path (digimon-world) digimon) #:bootstrap? #true))
      (if (procedure? info-ref) info-ref (error '#%info "on info.rkt found for ~a" digimon)))
    (define info-ref : Info-Ref (hash-ref! infobase digimon try-setinfo))
    (info-ref key (or defval (thunk (error '#%info "no info for ~a" key))))))

(define #%digimon : (->* () (String) String)
  (lambda [[digimon (current-digimon)]]
    (define name (#%info 'collection #:digimon digimon))
    (if (string? name) name digimon)))

(void (unless (member (digimon-world) (current-library-collection-paths))
        (current-library-collection-paths (cons (build-path (digimon-world)) (current-library-collection-paths)))
        (print-boolean-long-form #true)
        (current-logger /dev/log)
        
        ;;; Do not change the name of compiled file path, here we only escapes from DrRacket's convention.
        ;;; Since compiler will check the bytecodes in the core collection which have already been compiled into <path:compiled/>.
        (use-compiled-file-paths (list (build-path "compiled")))))

(define-syntax (define-digimon-dirpath stx)
    (syntax-case stx []
      [(_ parent [id ...])
       (with-syntax ([(digimon-id ...)
                      (for/list ([var (in-list (syntax->list #'(id ...)))])
                        (with-syntax ([id var]) (format-id #'id "digimon-~a" (syntax-e #'id))))])
         #'(begin (define digimon-id : (Parameterof Nothing Path)
                    (make-derived-parameter parent (immutable-guard 'digimon-id)
                                            (λ [[dir : Path]] (build-path dir (symbol->string 'id)))))
                  ...))]))

(define-digimon-dirpath digimon-zone [stone digitama digivice tamer village terminus])
(define-digimon-dirpath digimon-stone [tongue icon])

(define path->digimon-modpath : (->* [Path-String] [(Option Symbol)] (U Module-Path (List 'submod Module-Path Symbol)))
  (lambda [modfile [submodule #false]]
    (define modpath : Module-Path
      (let ([modfile (simplify-path modfile)])
        (cond [(false? (regexp-match? #px"\\.rkt$" modfile)) modfile]
              [else `(lib ,(path->string (cast (find-relative-path (digimon-world) modfile) Path)))])))
    (if (symbol? submodule) `(submod ,modpath ,submodule) modpath)))

(define find-digimon-files : (-> (-> Path-String Boolean) Path-String [#:search-compiled? Boolean] (Listof Path-String))
  (lambda [predicate start-path #:search-compiled? [search-compiled? #false]]
    (define px.exclude : Regexp
      (let ([cmpls (for/list : (Listof String) ([p : Path (in-list (use-compiled-file-paths))])
                     (path->string (cast (file-name-from-path p) Path)))])
        (pregexp (if search-compiled? "/\\.git$"
                     (string-join #:before-first "/(\\.git|" #:after-last ")$" (remove-duplicates cmpls) "|")))))
    (filter predicate (sequence->list (in-directory start-path (curry (negate regexp-match?) px.exclude))))))

(define file-readable? : (-> Path-String Boolean)
  (lambda [p]
    (and (file-exists? p)
         (memq 'read (file-or-directory-permissions p))
         #true)))

(define string-null? : (-> Any Boolean : #:+ String)
  (lambda [str]
    (and (string? str)
         (string=? str ""))))

(define object-name/symbol : (-> Any Symbol)
  (lambda [v]
    (define name (object-name v))
    (or (and (symbol? name) name)
        (string->symbol (format "<object-name:~a>" name)))))

(define current-macroseconds : (-> Fixnum)
  (lambda []
    (fl->fx (real->double-flonum (* (current-inexact-milliseconds) 1000)))))

(define timer-evt : (->* (Fixnum) (Fixnum) Timer-EvtSelf)
  (lambda [interval [basetime (current-milliseconds)]]
    (define alarm-time : Fixnum (fx+ basetime interval))
    ((inst wrap-evt Any (Vector Timer-EvtSelf Fixnum Fixnum))
     (alarm-evt alarm-time)
     (λ [alarm] (vector (timer-evt interval alarm-time) interval alarm-time)))))

(define timer-thread : (-> Fixnum (-> Thread Fixnum Any) [#:basetime Fixnum] Thread)
  (lambda [interval on-timer #:basetime [basetime (current-milliseconds)]]
    (define thdsrc : Thread (current-thread))
    (thread (thunk (let wait-dotask-loop ([evt (timer-evt interval basetime)])
                     (match (sync/enable-break evt)
                       [(vector (? evt? next-alarm) (? fixnum? alarm-time))
                        (on-timer thdsrc (fxquotient (fx- alarm-time basetime) interval))
                        (wait-dotask-loop next-alarm)]))))))

(define tcp-server : (-> Index (Input-Port Output-Port Index -> Any)
                         [#:max-allow-wait Natural] [#:localhost (Option String)]
                         [#:timeout (Option Fixnum)] [#:on-error (exn -> Any)] [#:custodian Custodian]
                         (Values (-> Void) Index))
  (lambda [port-hit on-connection #:max-allow-wait [maxwait (processor-count)] #:localhost [ip #false]
           #:timeout [timeout #false] #:on-error [on-error void] #:custodian [server-custodian (make-custodian)]]
    (parameterize ([current-custodian server-custodian])
      (define /dev/tcp : TCP-Listener (tcp-listen port-hit maxwait #true ip))
      (define-values (localhost portno remote rport) (tcp-addresses /dev/tcp #true))
      (define saved-params-incaseof-transferring-continuation : Parameterization (current-parameterization))
      (define (wait-accept-handle-loop) : Void
        (parameterize ([current-custodian (make-custodian server-custodian)])
          (define close-session : (-> Void) (thunk (custodian-shutdown-all (current-custodian))))
          (with-handlers ([exn:fail:network? (λ [[e : exn]] (on-error e) (close-session))])
            (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break /dev/tcp))
            (thread (thunk ((inst dynamic-wind Any)
                            (thunk (unless (false? timeout)
                                     (timer-thread timeout (λ [server times] ; give the task a chance to live longer
                                                             (if (fx= times 1) (break-thread server) (close-session))))))
                            (thunk (call-with-parameterization
                                    saved-params-incaseof-transferring-continuation
                                    (thunk (parameterize ([current-custodian (make-custodian)])
                                             (with-handlers ([exn? on-error])
                                               (on-connection /dev/tcpin /dev/tcpout portno))))))
                            (thunk (close-session)))))))
        (wait-accept-handle-loop))
      (thread wait-accept-handle-loop)
      (values (thunk (custodian-shutdown-all server-custodian)) portno))))

(define continuation-mark->stack-hints : (->* () ((U Continuation-Mark-Set Thread)) (Listof Stack-Hint))
  (lambda [[cm (current-continuation-marks)]]
    ((inst map (Pairof Symbol (Option (Vector (U String Symbol) Integer Integer))) (Pairof (Option Symbol) Any))
     (λ [[stack : (Pairof (Option Symbol) Any)]]
       (define maybe-name (car stack))
       (define maybe-srcinfo (cdr stack))
       (cons (or maybe-name 'λ)
             (and (srcloc? maybe-srcinfo)
                  (let ([src (srcloc-source maybe-srcinfo)]
                        [line (srcloc-line maybe-srcinfo)]
                        [column (srcloc-column maybe-srcinfo)])
                    (vector (if (symbol? src) src (~a src))
                            (or line -1)
                            (or column -1))))))
     (cond [(continuation-mark-set? cm) (continuation-mark-set->context cm)]
           [else (continuation-mark-set->context (continuation-marks cm))]))))

(define-type Prefab-Message msg:log)
(struct msg:log ([level : Log-Level] [brief : String] [details : Any] [topic : Symbol])
  #:prefab #:constructor-name make-prefab-message)

(define dtrace-send : (-> Any Symbol String Any Void)
  (lambda [topic level message urgent]
    (define log-level : Log-Level (case level [(debug info warning error fatal) level] [else 'debug]))
    (cond [(logger? topic) (log-message topic log-level message urgent)]
          [(symbol? topic) (log-message (current-logger) log-level topic message urgent)]
          [else (log-message (current-logger) log-level (object-name/symbol topic) message urgent)])))

(define-values (dtrace-debug dtrace-info dtrace-warning dtrace-error dtrace-fatal)
  (let ([dtrace (lambda [[level : Symbol]] : (->* (String) (#:topic Any #:urgent Any) #:rest Any Void)
                  (lambda [#:topic [topic (current-logger)] #:urgent [urgent (current-continuation-marks)] msgfmt . messages]
                    (dtrace-send topic level (if (null? messages) msgfmt (apply format msgfmt messages)) urgent)))])
    (values (dtrace 'debug) (dtrace 'info) (dtrace 'warning) (dtrace 'error) (dtrace 'fatal))))

(define dtrace-message : (-> Prefab-Message [#:logger Logger] [#:alter-topic (Option Symbol)] [#:detail-only? Boolean] Void)
  (lambda [info #:logger [logger (current-logger)] #:alter-topic [topic #false] #:detail-only? [detail-only? #false]]
    (log-message logger (msg:log-level info) (or topic (msg:log-topic info))
                 (msg:log-brief info) (if detail-only? (msg:log-details info) info))))

(define exn->prefab-message : (-> exn [#:level Log-Level] [#:exn->detail (-> exn Any)] Prefab-Message)
  (lambda [e #:level [level 'error] #:exn->detail [exn->detail (λ [[e : exn]] (continuation-mark->stack-hints (exn-continuation-marks e)))]]
    (make-prefab-message level
                         (exn-message e)
                         (exn->detail e)
                         (object-name/symbol e))))

(define the-synced-place-channel : (Parameterof (Option Place-Channel)) (make-parameter #false))
(define place-channel-evt : (-> Place-Channel [#:hint (Parameterof (Option Place-Channel))] (Evtof Any))
  (lambda [source-evt #:hint [hint the-synced-place-channel]]
    (hint #false)
    (wrap-evt source-evt ; do not work with guard evt since the maker may not be invoked
              (λ [datum] (hint source-evt)
                (cond [(not (place-message? datum)) datum]
                      [else (let ([stream : Any (place-message-stream datum)])
                              (match/handlers (if (bytes? stream) (with-input-from-bytes stream read) (box stream))
                                [(? exn:fail:read? e) (exn->prefab-message e #:level 'fatal #:exn->detail (λ _ stream))]))])))))

(define place-channel-send : (-> Place-Channel Any Void)
  (lambda [dest datum]
    (match datum
      [(? place-message-allowed?) (place-channel-put dest datum)]
      [(? exn?) (place-channel-put dest (exn->prefab-message datum))]
      [(box (and (not (? bytes? v)) (? place-message-allowed? v))) (place-channel-put dest (place-message v))]
      [_ (place-channel-put dest (place-message (with-output-to-bytes (thunk (write datum)))))])))

(define place-channel-recv : (-> Place-Channel [#:timeout Nonnegative-Real] [#:hint (Parameterof (Option Place-Channel))] Any)
  (lambda [channel #:timeout [s +inf.0] #:hint [hint the-synced-place-channel]]
    ; Note: the `hint` can also be used to determine whether it is timeout or receiving #false
    (sync/timeout/enable-break s (place-channel-evt channel #:hint hint))))

(define place-channel-send/recv : (-> Place-Channel Any [#:timeout Nonnegative-Real] [#:hint (Parameterof (Option Place-Channel))] Any)
  (lambda [channel datum #:timeout [s +inf.0] #:hint [hint the-synced-place-channel]]
    (place-channel-send channel datum)
    (place-channel-recv channel #:timeout s #:hint hint)))

(define place-status : (-> Place (U 'running Integer))
  (lambda [p]
    (match (sync/timeout 0 (place-dead-evt p))
      [(? false?) 'running]
      [_ (place-wait p)])))

(define place-wait-evt : (-> Place Place-EvtExit)
  (lambda [p]
    (wrap-evt (place-dead-evt p)
              (λ _ (cons p (place-wait p))))))

(define thread-mailbox-evt : (-> (Evtof Any))
  (lambda []
    (wrap-evt (thread-receive-evt)
              (λ _ (thread-receive)))))

(define call-as-normal-termination : (-> (-> Any) [#:atinit (-> Any)] [#:atexit (-> Any)] Void)
  (lambda [#:atinit [atinit/0 void] main/0 #:atexit [atexit/0 void]]
    (define exit-racket : (-> Any AnyValues) (exit-handler))
    (define service-exit : (-> Any (Option Byte))
      (match-lambda
        ['FATAL 95]
        ['ECONFIG 96]
        ['ENOSERVICE 99]
        ['EPERM 100]
        [_ #false]))

    (define (terminate [status : Any]) : Any
      (parameterize ([exit-handler exit-racket])
        (cond [(exact-nonnegative-integer? status) (exit (min status 255))]
              [(service-exit status) => exit]
              [else (exit 0)])))
    
    (parameterize ([exit-handler terminate])
      (exit (with-handlers ([exn? (lambda [[e : exn]] (and (eprintf "~a~n" (exn-message e)) 'FATAL))]
                            [void (lambda [e] (and (eprintf "(uncaught-exception-handler) => ~a~n" e) 'FATAL))])
              (dynamic-wind (thunk (with-handlers ([exn? (lambda [[e : exn]] (atexit/0) (raise e))])
                                     (atinit/0)))
                            (thunk (main/0))
                            (thunk (atexit/0))))))))

(define vector-set-place-statistics! : (-> Racket-Place-Status Void)
  (lambda [stat]
    (vector-set-performance-stats! stat)
    (vector-set! stat 10 (+ (vector-ref stat 10) (current-memory-use)))))

(define vector-set-thread-statistics! : (-> Racket-Thread-Status Thread Void)
  (lambda [stat thd]
    (vector-set-performance-stats! stat thd)))

(define make-peek-port : (->* (Input-Port) ((Boxof Natural) Symbol) Input-Port)
  (lambda [/dev/srcin [iobox ((inst box Natural) 0)] [name '/dev/tmpeek]]
    (make-input-port name
                     (λ [[s : Bytes]] : (U EOF Exact-Positive-Integer)
                       (define peeked : Natural (unbox iobox))
                       (define r (peek-bytes! s peeked /dev/srcin))
                       (set-box! iobox (+ peeked (if (number? r) r 1))) r)
                     #false
                     void)))

(define car.eval : (->* (Any) (Namespace) Any)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (thunk (eval sexp ns))
                      (λ result (car result)))))

(define void.eval : (->* (Any) (Namespace) Void)
  (lambda [sexp [ns (current-namespace)]]
    (call-with-values (thunk (eval sexp ns)) void)))

(define echof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (printf "~a" (if (terminal-port? (current-output-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))

(define eechof : (-> String [#:fgcolor Term-Color] [#:bgcolor Term-Color] [#:attributes (Listof Symbol)] Any * Void)
  (lambda [msgfmt #:fgcolor [fg #false] #:bgcolor [bg #false] #:attributes [attrs null] . vals]
    (define rawmsg (apply format msgfmt vals))
    (eprintf "~a" (if (terminal-port? (current-error-port)) (term-colorize fg bg attrs rawmsg) rawmsg))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama typed/racket
  (provide (all-defined-out))

  (require "sugar.rkt")
  
  (define-type Term-Color (Option (U String Symbol Byte)))
  
  (define-type Racket-Place-Status (Vector Fixnum Fixnum Fixnum Natural Natural Natural Natural Natural Fixnum Fixnum Natural Natural))
  (define-type Racket-Thread-Status (Vector Boolean Boolean Boolean Natural))
  (define-type Vector-Set-Performance-Stats! (case-> [Racket-Place-Status -> Void]
                                                     [Racket-Thread-Status Thread -> Void]))
  
  (require/typed/provide racket [vector-set-performance-stats! Vector-Set-Performance-Stats!])

  (struct place-message ([stream : Any]) #:prefab)
  (define infobase : (HashTable String Info-Ref) (make-hash))
  
  (define immutable-guard : (-> Symbol (Path-String -> Nothing))
    (lambda [pname]
      (λ [pval] (error pname "Immutable Parameter: ~a" pval))))
  
  (define vim-colors : (HashTable String Byte)
    #hash(("black" . 0) ("darkgray" . 8) ("darkgrey" . 8) ("lightgray" . 7) ("lightgrey" . 7) ("gray" . 7) ("grey" . 7) ("white" . 15)
                        ("darkred" . 1) ("darkgreen" . 2) ("darkyellow" . 3) ("darkblue" . 4) ("brown" . 5) ("darkmagenta" . 5) ("darkcyan" . 6)
                        ("red" . 9) ("lightred" . 9) ("green" . 10) ("lightgreen" . 10) ("yellow" . 11) ("lightyellow" . 11)
                        ("blue" . 12) ("lightblue" . 12) ("magenta" . 13) ("lightmagenta" . 13) ("cyan" . 14) ("lightcyan" . 14)))
  
  (define term-colorize : (-> Term-Color Term-Color (Listof Symbol) String String)
    (lambda [fg bg attrs content]
      (define color-code : (-> String [#:bgcolor? Boolean] String)
        (lambda [color #:bgcolor? [bg? #false]]
          (format "~a8;5;~a" (if bg? 4 3) (if (regexp-match? #px"\\d+" color) color (hash-ref vim-colors color)))))
      (regexp-replace #px"^(\\s*)(.+?)(\\s*)$" content
                      (format "\\1\033[~a;~a;~am\\2\033[0m\\3"
                              (string-replace (for/fold : String ([effects ""]) ([attr : Symbol (in-list attrs)])
                                                (case (string-downcase (format "~a" attr))
                                                  [{"bold" "bright"} (string-append effects ";1")]
                                                  [{"dim"} (string-append effects ";2")]
                                                  [{"underline" "undercurl"} (string-append effects ";4")]
                                                  [{"blink"} (string-append effects ";5")]
                                                  [{"reverse" "inverse"} (string-append effects ";7")]
                                                  [{"hidden" "password"} (string-append effects ";8")]
                                                  [else (error 'tarminal-colorize "Unsupported Terminal Attribute: ~a" attr)]))
                                              "^;" "" #:all? #false)
                              (if (false? fg) 39 (color-code (string-downcase (format "~a" fg))))
                              (if (false? bg) 49 (color-code (string-downcase (format "~a" bg)) #:bgcolor? #true)))))))

(require (submod "." digitama))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* test racket
  (require (submod ".."))
  
  (for ([color (in-list '(grey red green blue yellow magenta cyan))])
    (define-values [darkcolor lightcolor] (values (format "dark~a" color) (format "light~a" color)))
    (echof "»»» 8/16 colors test:")
    (echof #:fgcolor color " ~a" color)
    (echof #:fgcolor darkcolor " ~a" darkcolor)
    (echof #:fgcolor lightcolor " ~a" lightcolor)
    (echof #:bgcolor color " ~a" color)
    (echof #:bgcolor darkcolor " ~a" darkcolor)
    (echof #:bgcolor lightcolor " ~a~n" lightcolor)
    (for ([effect (in-list '(bright dim underline blink reverse password))])
      (echof #:fgcolor darkcolor #:attributes (list effect) "dark:~a " effect)
      (echof #:fgcolor lightcolor #:attributes (list effect) "light:~a " effect))
    (newline))
  
  (echof "»»» 256 colors test:~n")
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:fgcolor (sub1 color) caption)
    (when (zero? (remainder color 32))
      (newline)))
  
  (for ([color (in-range 1 257)])
    (define caption (~a (sub1 color) #:width 4 #:align 'right))
    (echof #:bgcolor (sub1 color) caption)
    (when (zero? (remainder color 32))
      (newline))))
