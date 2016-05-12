#lang at-exp typed/racket

(provide (all-defined-out))

@require{digicore.rkt}

(require (for-syntax racket/list))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-type (UUID dataspec) String)

(define-type Schema-Record schema-record)

(struct schema-record ([uuid : String] [ctime : Fixnum] [mtime : Fixnum] [deleted? : Boolean] [mac : (Option String)])
  #:prefab ;#:type-name Schema ; this break the type checking if it is inherited 
  #:constructor-name abstract-schema-record)

(struct exn:schema exn:fail ())

(struct exn:schema:record exn:schema ([table : Struct-TypeTop] [maniplation : Symbol] [uuid : String]))
(struct exn:schema:record:mac exn:schema:record ())

(struct exn:schema:constraint exn:schema ([table : Struct-TypeTop] [constraint : (Listof Any)] [given : (HashTable Symbol Any)]))
(struct exn:schema:constraint:unique exn:schema:constraint ([key-type : (U 'Natural 'Surrogate)]))

(define raise-schema-constraint-error : (-> Symbol Struct-TypeTop (Listof Any) (Listof (Pairof Symbol Any)) Nothing)
  (lambda [source table constraints given]
    (throw [exn:schema:constraint table (reverse constraints) (make-hash given)]
           "~a: constraint violation~n  table: ~a~n  constraint: @~a~n  given: ~a" source table
           (string-join ((inst map String Any) ~s (reverse constraints))
                        (format "~n~a@" (make-string 14 #\space)))
           (string-join ((inst map String (Pairof Symbol Any)) (λ [kv] (format "(~a . ~s)" (car kv) (cdr kv))) (reverse given))
                        (format "~n~a " (make-string 8 #\space))))))

(define raise-schema-unique-constraint-error : (-> Symbol Struct-TypeTop (Listof (Pairof Symbol Any)) [#:type (U 'Natural 'Surrogate)] Nothing)
  (lambda [source table given #:type [key-type 'Natural]]
    (define entry : (HashTable Symbol Any) (make-hash given))
    (throw [exn:schema:constraint:unique table `(UNIQUE ,(hash-keys entry)) entry key-type]
           "~a: constraint violation~n  table: ~a~n  constraint: @Unique{~a}~n  given: {~a}"
           source (object-name table)
           (string-join (map symbol->string (hash-keys entry)) ", ")
           (string-join (hash-map entry (λ [k v] (format "~a: ~s" k v)))
                        (format "~n~a " (make-string 9 #\space))))))

(struct msg:schema msg ([table : Symbol] [manipaltion : Symbol] [uuid : String])
  #:prefab #:type-name Schema-Message)

(define make-schema-message : (->* (Struct-TypeTop Symbol String Any) (#:level Log-Level #:topic (Option Symbol) String) #:rest Any Schema-Message)
  (lambda [table maniplation uuid urgent #:level [level 'info] #:topic [topic #false] [message ""] . argl]
    (define table-name : Symbol (object-name/symbol table))
    (msg:schema (or topic table-name) level (if (null? argl) message (apply format message argl)) urgent
                table-name maniplation uuid)))

(define exn:schema->schema-message : (-> exn [#:level (Option Log-Level)] Prefab-Message)
  (lambda [e #:level [level #false]]
    (cond [(not (exn:schema:record? e)) (exn->prefab-message e)]
          [else (msg:schema (object-name/symbol e)
                            (or level (match e [(exn:schema:constraint:unique _ _ _ _ _ 'Natural) 'warning] [_ 'error]))
                            (exn-message e)
                            (continuation-mark->stack-hints (exn-continuation-marks e))
                            (object-name/symbol (exn:schema:record-table e))
                            (exn:schema:record-maniplation e)
                            (exn:schema:record-uuid e))])))

(define-syntax (define-table stx)
  (syntax-parse stx #:datum-literals [as :]
    [(_ table as Table ([field : DataType info ...] ...) (~optional (~seq #:check constraint:expr) #:defaults ([constraint #'true])))
     (with-syntax* ([check-fields (datum->syntax #'table (gensym 'schema))]
                    [struct:table (format-id #'table "struct:~a" (syntax-e #'table))]
                    [unsafe-table (format-id #'table "unsafe-~a" (syntax-e #'table))]
                    [create-table (format-id #'table "create-~a" (syntax-e #'table))]
                    [update-table (format-id #'table "update-~a" (syntax-e #'table))]
                    [delete-table (format-id #'table "delete-~a" (syntax-e #'table))]
                    [select-table (format-id #'table "select-~a" (syntax-e #'table))]
                    [digest-table (format-id #'table "digest-~a" (syntax-e #'table))]
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
       #'(begin (struct table schema-record ([field : DataType] ...) #:prefab #:type-name Table #:constructor-name unsafe-table)
                
                (define-syntax (check-fields stx)
                  (syntax-case stx []
                    [(_ id field ...)
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
                           (raise-schema-constraint-error id struct:table (if table-failure (cons 'constraint checks) checks) givens)))]))
                
                (define (create-table #:unsafe? [unsafe? : Boolean #false] #:UUID [uuid : (Option (UUID String)) #false] args ...) : Table
                  (when (not unsafe?) (check-fields 'create-table field ...))
                  (define now : Fixnum (current-macroseconds))
                  (unsafe-table (or uuid (uuid:timestamp)) now now #false #false field ...))
  
                (define (update-table [occurrence : Table] #:update-uuid? [update-uuid? : Boolean #false] args! ...) : Table
                  ; Maybe: (void) will never be an valid value that will be inserted into database.
                  (let ([field (if (void? field) (table-field occurrence) field)] ...)
                    (check-fields 'update-table field ...)
                    (unsafe-table (if update-uuid? (uuid:timestamp) (schema-record-uuid occurrence))
                                  (schema-record-ctime occurrence) (current-macroseconds)
                                  #false #false field ...)))

                (define (delete-table [occurrence : Table]) : Table
                  (unsafe-table (schema-record-uuid occurrence) (schema-record-ctime occurrence) (current-macroseconds)
                                #true #false (table-field occurrence) ...))

                (define (digest-table [occurrence : Table] [hash : EVP_MD* sha256] #:verify? [verify? : Boolean #false]) : Table
                  (define salt : Bytes
                    (let ([seed : Integer (* (schema-record-mtime occurrence) (if (schema-record-deleted? occurrence) -1 1))]
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
                  (define shadow : Table (struct-copy table occurrence [mac #:parent schema-record #false]))
                  (define oldsum : (Option String) (schema-record-mac occurrence))
                  (define checksum : String (bytes->hex-string (HMAC hash salt (string->bytes/utf-8 (~s shadow)))))
                  (cond [(and (string? oldsum) (string=? oldsum checksum)) occurrence]
                        [(or (false? oldsum) (not verify?)) (struct-copy table occurrence [mac #:parent schema-record checksum])]
                        [else (throw [exn:schema:record:mac struct:table 'verify (schema-record-uuid occurrence)] "MAC Mismatch!")]))))]))
