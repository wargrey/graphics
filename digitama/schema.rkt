#lang at-exp typed/racket

(provide (all-defined-out))

@require{digicore.rkt}

(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-type (UUID dataspec) String)
(define-type Schema-Message msg:schema)

(define schema-tables : (HashTable Symbol Struct-TypeTop) (make-hasheq))

(define-type/enum schema-maniplations : Schema-Maniplation read write verify select delete merge index unknown)
(struct msg:schema msg:log ([maniplation : Schema-Maniplation] [table : Symbol] [uuid : String]) #:prefab)


(define make-schema-message : (->* (Schema-Maniplation Struct-TypeTop String Any)
                                   (#:level (Option Log-Level) String) ;;; `#:rest Type` is broken in 6.6
                                   Schema-Message)
  (lambda [maniplation struct:table uuid urgent #:level [level #false] [message ""]]
    (cond [(exn:schema? urgent) (exn:schema->schema-message urgent #:level level)]
          [(exn? urgent) (exn:schema->schema-message (exn->exn:schema urgent maniplation struct:table uuid) #:level level)]
          [else (let ([table : Symbol (object-name/symbol struct:table)])
                  (msg:schema (or level 'info) message urgent table maniplation table uuid))])))

(define-type Schema-Record schema-record)
(struct schema-record ([uuid : (UUID String)] [ctime : Fixnum] [mtime : Fixnum] [deleted? : Boolean] [mac : (Option String)])
  #:prefab ;#:type-name Schema ; this break the type checking if it is inherited 
  #:constructor-name abstract-schema-record)

(struct exn:schema exn:fail ([struct:table : Struct-TypeTop] [uuid : (UUID String)]))
(struct exn:schema:read exn:schema ([reason : (U EOF Schema-Record exn False)]))
(struct exn:schema:write exn:schema ([reason : exn]))
(struct exn:schema:index exn:schema ())
(struct exn:schema:verify exn:schema ())
(struct exn:schema:merge exn:schema ())
(struct exn:schema:select exn:schema ())
(struct exn:schema:delete exn:schema ())

(struct exn:schema:constraint exn:schema:merge ([constraint : (Listof Any)] [given : (HashTable Symbol Any)]))
(struct exn:schema:constraint:unique exn:schema:constraint ([key-type : (U 'Natural 'Surrogate)]))

(define raise-schema-constraint-error : (-> Symbol Struct-TypeTop (UUID String) (Listof Any) (Listof (Pairof Symbol Any)) Nothing)
  (lambda [source struct:table uuid constraints given]
    (throw [exn:schema:constraint struct:table uuid (reverse constraints) (make-hash given)]
           "~a: constraint violation~n  table: ~a~n  constraint: @~a~n  given: ~a~n" source struct:table
           (string-join ((inst map String Any) ~s (reverse constraints))
                        (format "~n~a@" (make-string 14 #\space)))
           (string-join ((inst map String (Pairof Symbol Any)) (λ [kv] (format "(~a . ~s)" (car kv) (cdr kv))) (reverse given))
                        (format "~n~a " (make-string 8 #\space))))))

(define raise-schema-unique-constraint-error : (-> Symbol Struct-TypeTop (UUID String) (Listof (Pairof Symbol Any))
                                                   [#:type (U 'Natural 'Surrogate)]
                                                   Nothing)
  (lambda [source struct:table uuid given #:type [key-type 'Natural]]
    (define entry : (HashTable Symbol Any) (make-hash given))
    (throw [exn:schema:constraint:unique struct:table uuid `(UNIQUE ,(hash-keys entry)) entry key-type]
           "~a: constraint violation~n  table: ~a~n  constraint: @Unique{~a}~n  given: {~a}~n"
           source (object-name struct:table)
           (string-join (map symbol->string (hash-keys entry)) ", ")
           (string-join (hash-map entry (λ [k v] (format "~a: ~s" k v)))
                        (format "~n~a " (make-string 9 #\space))))))

(define exn->exn:schema : (-> exn Schema-Maniplation Struct-TypeTop String exn:schema)
  (lambda [e maniplation struct:table uuid]
    (define brief : String (exn-message e))
    (define cmarks : Continuation-Mark-Set (exn-continuation-marks e))
    (case maniplation
      [(read) (exn:schema:read brief cmarks struct:table uuid e)]
      [(write) (exn:schema:write brief cmarks struct:table uuid e)]
      [(select) (exn:schema:select brief cmarks struct:table uuid)]
      [(delete) (exn:schema:delete brief cmarks struct:table uuid)]
      [(merge) (exn:schema:merge brief cmarks struct:table uuid)]
      [(verify) (exn:schema:verify brief cmarks struct:table uuid)]
      [(index) (exn:schema:index brief cmarks struct:table uuid)]
      [else (exn:schema brief cmarks struct:table uuid)])))

(define exn:schema->schema-message : (-> exn:schema [#:level (Option Log-Level)] Schema-Message)
  (lambda [e #:level [level #false]]
    (msg:schema (or level (match e [(struct* exn:schema:constraint:unique ([key-type 'Natural])) 'warning] [_ 'error]))
                (exn-message e)
                (continuation-mark->stack-hints (exn-continuation-marks e))
                (object-name/symbol e)
                (cond [(exn:schema:read? e) 'read]
                      [(exn:schema:write? e) 'write]
                      [(exn:schema:select? e) 'select]
                      [(exn:schema:delete? e) 'delete]
                      [(exn:schema:merge? e) 'merge]
                      [(exn:schema:verify? e) 'verify]
                      [(exn:schema:index? e) 'index]
                      [else 'unknown])
                (object-name/symbol (exn:schema-struct:table e))
                (exn:schema-uuid e))))

(define schema-name->struct:schema : (-> Symbol Struct-TypeTop)
  (let ([unknown-schema-tables : (HashTable Symbol Struct-TypeTop) (make-hasheq)])
    (lambda [record-name]
      (hash-ref schema-tables record-name
                (thunk (hash-ref! unknown-schema-tables record-name
                                  (thunk (let-values ([(struct:unknown make-unknown unknown? unknown-ref unknown-set!)
                                                       (make-struct-type record-name struct:schema-record 0 0
                                                                         'manually-serialization null 'prefab
                                                                         #false null (and 'prefab-has-not-guard #false)
                                                                         #false)])
                                           struct:unknown))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define schema-digest : (-> Schema-Record String)
  (let* ([evp-hashes : (Vectorof EVP_MD*) (vector sha224 sha256 sha384 sha512)]
         [hash-count : Index (vector-length evp-hashes)])
    (lambda [shadow]
      (define message : Bytes (string->bytes/utf-8 (~s shadow)))
      (define evp-hash : EVP_MD* (vector-ref evp-hashes (remainder (bytes-length message) hash-count)))
      (define salt-for-fun : Bytes
        (let ([seed : Integer (* (schema-record-mtime shadow) (if (schema-record-deleted? shadow) -1 1))]
              [size->bytes : (-> Integer Bytes) (λ [size] (integer->integer-bytes size 4 #false #true))])
          (define buffer : Bytes (make-bytes (quotient (+ (integer-length seed) 7) 8)))
          (define size : Index (bytes-length buffer))
          (for ([idx : Integer (in-range size)])
            (bytes-set! buffer idx (bitwise-and (arithmetic-shift seed (* (- size idx 1) -8)) #xFF)))
          (cond [(and (positive? seed) (= (bytes-ref buffer 0) #b10000000))
                 (bytes-append (size->bytes (add1 size)) (bytes #x00) buffer)]
                [(and (negative? seed) (false? (bitwise-bit-set? (bytes-ref buffer 0) 7)))
                 (bytes-append (size->bytes (add1 size)) (bytes #xFF) buffer)]
                [else (bytes-append (size->bytes size) buffer)])))
      (bytes->hex-string (HMAC evp-hash salt-for-fun message)))))

(define schema-file/rename-old-file : (-> Path-String Bytes (Listof Bytes) Path-String)
  (lambda [path-hint .rstn old-exts]
    (define path.rstn : Path-String (path-replace-extension (simple-form-path path-hint) .rstn))
    (with-handlers ([exn:fail:filesystem? void])
      (for ([.ext : Bytes (in-list old-exts)])
        (define path.ext : Path-String (path-replace-extension path-hint .ext))
        (when (file-exists? path.ext)
          (rename-file-or-directory path.ext path.rstn (and 'exists-ok? #false)))))
    path.rstn))

(define schema-write-to-file/unsafe : (-> Schema-Record Path-String Bytes (Listof Bytes) Void)
  (lambda [occurrence path-hint .rstn old-exts]
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (let ([dirname : (Option Path) (path-only path-hint)])
                (and dirname (unless (directory-exists? dirname) (make-directory* dirname)))))
       (thunk (parameterize ([current-output-port (let ([s.dat (schema-file/rename-old-file path-hint .rstn old-exts)])
                                                    (open-output-file s.dat #:exists 'truncate/replace))])
                (write occurrence)
                (newline)))
       (thunk (custodian-shutdown-all (current-custodian)))))))

(define schema-read-from-file/unsafe : (-> Path-String Bytes (Listof Bytes) Any)
  (lambda [path-hint .rstn old-exts]
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (let ([dirname : (Option Path) (path-only path-hint)])
                (when (and dirname (null? (directory-list dirname)))
                  (with-handlers ([exn:fail:filesystem? void])
                    (delete-directory dirname)))))
       (thunk (read (open-input-file (schema-file/rename-old-file path-hint .rstn old-exts))))
       (thunk (custodian-shutdown-all (current-custodian)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-table stx)
  (syntax-parse stx #:datum-literals [as :]
    [(_ table as Table ([field : DataType info ...] ...) (~optional (~seq #:check constraint:expr) #:defaults ([constraint #'true])))
     (with-syntax* ([struct:table (format-id #'table "struct:~a" (syntax-e #'table))]
                    [unsafe-table (format-id #'table "unsafe-~a" (syntax-e #'table))]
                    [create-table (format-id #'table "create-~a" (syntax-e #'table))]
                    [update-table (format-id #'table "update-~a" (syntax-e #'table))]
                    [delete-table (format-id #'table "delete-~a" (syntax-e #'table))]
                    [select-table (format-id #'table "select-~a" (syntax-e #'table))]
                    [digest-table (format-id #'table "digest-~a" (syntax-e #'table))]
                    [write-table (format-id #'table "write-~a" (syntax-e #'table))]
                    [read-table (format-id #'table "read-~a" (syntax-e #'table))]
                    [check-fields (datum->syntax #'table (gensym 'schema))]
                    [table? (format-id #'table "~a?" (syntax-e #'table))]
                    [([:field table-field old-value] ...)
                     (for/list ([field (in-list (syntax->list #'(field ...)))])
                       (list (datum->syntax field (string->keyword (symbol->string (syntax-e field))))
                             (datum->syntax field (format-id #'table "~a-~a" (syntax-e #'table) (syntax-e field)))
                             (datum->syntax field (format-id #'table "old-~a" (syntax-e field)))))]
                    [([defval ...] ...)
                     (for/list ([field-info (in-list (syntax->list #'([DataType info ...] ...)))])
                       (syntax-parse field-info #:datum-literals [% = Option]
                         [(DataType % comments ... = defval (~optional (~seq #:check constraint:expr))) #'(defval)]
                         [((Option T) % comments ...) #'(#false)]
                         [(DataType % comments ...) #'()]))]
                    [(field-constraint ...)
                     (for/list ([field-info (in-list (syntax->list #'([info ...] ...)))])
                       (syntax-case field-info [:]
                         [(_ ... #:check s-exp) #'s-exp]
                         [(_ ...) #'#true]))]
                    [([args ...] [args! ...])
                     (for/fold ([syns (list null null)])
                               ([:fld (in-list (syntax->list #'(:field ...)))]
                                [arg (in-list (syntax->list #'([field : DataType defval ...] ...)))]
                                [arg! (in-list (syntax->list #'([field : (U DataType Void) (void)] ...)))])
                       (list (cons :fld (cons arg (car syns)))
                             (cons :fld (cons arg! (cadr syns)))))])
       #'(begin (define-type Table table)
                (struct table schema-record ([field : DataType] ...) #:prefab #:constructor-name unsafe-table)
                (hash-set! schema-tables (object-name/symbol struct:table) struct:table)
                
                (define-syntax (check-fields stx)
                  (syntax-case stx []
                    [(_ src uuid field ...)
                     #'(let ([failures (list (false? field-constraint) ...)]
                             [table-failure (false? constraint)])
                         (when (or (memq #true failures) table-failure)
                           (define maybe-fields (remove-duplicates (filter symbol? (flatten 'constraint))))
                           (define-values (givens checks)
                             (for/fold ([fields : (Listof (Pairof Symbol Any)) null] [checks : (Listof Any) null])
                                       ([target (in-list (list 'field ...))]
                                        [given (in-list (list field ...))]
                                        [check (in-list (list 'field-constraint ...))]
                                        [failure (in-list failures)])
                               (cond [failure (values (cons (cons target given) fields) (cons check checks))]
                                     [(and table-failure (memq target maybe-fields)) (values (cons (cons target given) fields) checks)]
                                     [else (values fields checks)])))
                           (raise-schema-constraint-error src struct:table uuid
                                                          (if table-failure (cons 'constraint checks) checks)
                                                          givens)))]))
                
                (define (create-table #:unsafe? [unsafe? : Boolean #false] #:UUID [uuid : (UUID String) ""] args ...) : Table
                  (when (not unsafe?) (check-fields 'create-table "" field ...))
                  (define now : Fixnum (current-macroseconds))
                  (unsafe-table (if (string-null? uuid) (uuid:timestamp) uuid) now now #false #false field ...))
                
                (define (update-table [occurrence : Table] #:pretend-creation? [create? : Boolean #false] args! ...) : Table
                  ; Maybe: (void) will never be an valid value that will be inserted into database.
                  (let ([field (if (void? field) (table-field occurrence) field)] ...)
                    (check-fields 'update-table (schema-record-uuid occurrence) field ...)
                    (define now : Fixnum (current-macroseconds))
                    (unsafe-table (if create? (uuid:timestamp) (schema-record-uuid occurrence))
                                  (if create? now (schema-record-ctime occurrence))
                                  now #false #false field ...)))
                
                (define (delete-table [occurrence : Table]) : Table
                  (unsafe-table (schema-record-uuid occurrence) (schema-record-ctime occurrence) (current-macroseconds)
                                #true #false (table-field occurrence) ...))
                
                (define (digest-table [occurrence : Table] #:verify? [verify? : Boolean #false]) : Table
                  (define oldsum : (Option String) (schema-record-mac occurrence))
                  (when (and verify? (false? oldsum))
                    (throw [exn:schema:verify struct:table (schema-record-uuid occurrence)] "occurrence has not digested"))
                  (define shadow : Table (struct-copy table occurrence [mac #:parent schema-record #false]))
                  (define digest : String (schema-digest shadow))
                  (cond [(and (string? oldsum) (string=? oldsum digest)) occurrence]
                        [(not verify?) (struct-copy table occurrence [mac #:parent schema-record digest])]
                        [else (throw [exn:schema:verify struct:table (schema-record-uuid occurrence)] "digest mismatch")]))
                
                (define (write-table [occurrence : Table] [out : (U Output-Port Path-String) (current-output-port)]
                                     #:suffix [.rstn : Bytes #".rstn"] #:old-suffixes [old-exts : (Listof Bytes) (list #".rkt")]) : Void
                  (define digested-one : Table (digest-table occurrence #:verify? #false))
                  (cond [(output-port? out) (write digested-one out)]
                        [else (schema-write-to-file/unsafe digested-one out .rstn old-exts)]))
                
                (define (read-table [in : (U Input-Port Path-String) /dev/zero]
                                    #:suffix [.rstn : Bytes #".rstn"] #:old-suffixes [old-exts : (Listof Bytes) (list #".rktl")]) : Table
                  (define-values (head ~mistype) (values (~a "read-" 'table) "~a: unexpected record type: ~a"))
                  (define peeked : (Boxof Natural) (box 0))
                  (match/handlers (cond [(not (input-port? in)) (schema-read-from-file/unsafe in .rstn old-exts)]
                                        [(read (make-peek-port in peeked)) => (λ [v] (when (table? v) (read-bytes (unbox peeked) in)) v)])
                    [(? table? occurrence) (digest-table occurrence #:verify? #true)]
                    [(? schema-record? record) (throw [exn:schema:read struct:table "" record] ~mistype head (object-name record))]
                    [(? eof-object?) (throw [exn:schema:read struct:table "" eof] "~a: unexpected end of stream" head)]
                    [(? exn? e) (throw [exn:schema:read struct:table "" e] "~a: ~a" head (exn-message e))]
                    [_ (throw [exn:schema:read struct:table "" #false] "~a: unrecognized stream" head)]))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define-table p as P
    ([number     : String                 % 出版社编号 #:check (regexp-match? #px"^\\d+$" number)]
     [names      : (Option String)        % 出版社名称]
     [address    : (Option String)        % 出版社地址]
     [url        : (Option String)        % 出版社官网]
     [about      : (Option String)        % 出版社简介]))
  
  (define-table s as S
    ([titles     : (Listof String)        % 丛书标题 '("བོད་ཀྱི་བཅུ་ཕྲག་རིག་མཛོད་ཆེན་མོ" "藏族十明文化传世经典丛书")]
     [type       : Symbol                 % 丛书类型]))
  
  (define-values (i o) (make-pipe))
  (write-p (create-p #:number "123") o)
  (write-s (create-s #:titles (list "བོད་ཀྱི་བཅུ་ཕྲག་རིག་མཛོད་ཆེན་མོ") #:type 'Tibetan) o)
  
  (with-handlers ([exn? displayln]) (read-s i))
  (read-p i)
  (read-s i)
  
  (close-output-port o)
  (read i))
