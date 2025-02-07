#lang typed/racket/base

(provide (all-defined-out))
(provide diaflowlet-node-construct diaflowlet-arrow-identify)

(require racket/symbol)
(require racket/list)

(require digimon/metrics)
(require digimon/sequence)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/markup)
(require geofun/digitama/dc/composite)

(require "flowchart.rkt")
(require "digitama/flowchart/flowlet.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflowlet-block-width : (Parameterof Nonnegative-Flonum) (make-parameter 150.0))
(define default-diaflowlet-block-height : (Parameterof Nonnegative-Flonum) (make-parameter (* (default-diaflowlet-block-width) 0.50)))

(define default-diaflowlet-node-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 'xx-large)))
(define default-diaflowlet-edge-font : (Parameterof Font) (make-parameter (desc-font #:family 'math #:size 'x-large)))
(define default-diaflowlet-stroke-thickness : (Parameterof Nonnegative-Flonum) (make-parameter 1.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flowlet-read
  (lambda [#:id [id : (Option Symbol) #false]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real (default-diaflowlet-block-width)]
           #:block-height [block-height : Real (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real -0.81] #:grid-height [grid-height : Real -0.81]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0] #:file-position [file-position : Real 0.45]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:node-font [node-font : (Option Font) (default-diaflowlet-node-font)]
           #:edge-font [edge-font : (Option Font) (default-diaflowlet-edge-font)]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier diaflowlet-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) diaflowlet-node-construct]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:input-desc [alt-in : (Option DC-Markup-Text) #false]
           #:output-desc [alt-out : (Option (-> Any (U Void DC-Markup-Text))) #false]
           #:reader [f : (-> Input-Port Any) read] #:peek-size [peek-size : Index 8]
           [in : Input-Port] [repeats : Index 1]] : Dia:Flow
    (define-values (funcs func-descs) (diaflowlet-funcion-rename (list->n:vector (list f) (+ repeats 1)) diaflow-delim-format))
    (define base-width  : Nonnegative-Flonum (~length block-width  ((default-diaflow-block-width))))
    (define base-height : Nonnegative-Flonum (~length block-height ((default-diaflow-block-height))))
    (define in-desc (diaflowlet-input-desc in alt-in))
    (define offset (real->double-flonum file-position))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-block-width  (* base-height 0.80)]
                   [default-diaflow-storage-block-height (* base-height 0.618)]
                   [default-diaflow-storage-font (or node-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or edge-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-node-label-string diaflowlet-node-label-string]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define path (dia-initial-path #false grid-width grid-height +0.5 0.0+0.0i '#:home))

      (if (or downward?)
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (diaflowlet-function->anchor r)])
                  (with-gomamon! path
                    (move-down ystep f-anchor (if (> idx 0) (diaflowlet-output-desc (r in) alt-out) in-desc))
                    (jump-left (* xstep offset))
                    (jump-up (* ystep (if (> idx 0) 0.5 0.618)) (diaflowlet-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! path
                  (move-down ystep '$ (diaflowlet-output-desc (r in) alt-out))
                  (jump-left (* xstep offset))
                  (jump-up (* ystep 0.382) (diaflowlet-file-anchor in peek-size)))))
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (diaflowlet-function->anchor r)])
                  (gomamon-move-right! path xstep f-anchor (if (> idx 0) (diaflowlet-output-desc (r in) alt-out) in-desc))
                  (with-gomamon! path
                    (jump-down (* ystep offset))
                    (jump-left (* xstep (if (> idx 0) 0.5 0.618)) (diaflowlet-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! path
                  (move-right xstep '$ (diaflowlet-output-desc (r in) alt-out))
                  (jump-down (* ystep offset))
                  (jump-left (* xstep 0.382) (diaflowlet-file-anchor in peek-size))))))
            
      (define flowlet
        (dia-path-flow path
                       #:id id #:path-operator path-op #:flow-operator flow-op 
                       #:border bdr #:background bg #:margin margin #:padding padding
                       #:λblock block-detect #:λarrow arrow-detect
                       #:λnode make-node #:λnode-label make-node-label
                       #:λedge make-edge #:λedge-label make-edge-label))
      
      (assert flowlet dia:flow?))))

(define #:forall (In Out) dia-flowlet-function
  (lambda [#:id [id : (Option Symbol) #false]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real (default-diaflowlet-block-width)]
           #:block-height [block-height : Real (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real -0.81] #:grid-height [grid-height : Real -0.81]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:node-font [node-font : (Option Font) (default-diaflowlet-node-font)]
           #:edge-font [edge-font : (Option Font) (default-diaflowlet-edge-font)]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier diaflowlet-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) diaflowlet-node-construct]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:input-desc [alt-in : (U False DC-Markup-Text (-> In (U Void DC-Markup-Text))) #false]
           #:output-desc [alt-out : (U False DC-Markup-Text (-> Out (U Void DC-Markup-Text))) #false]
           [f : (U (-> In Out) Symbol String)] [in : In]] : Dia:Flow
    (parameterize ([default-diaflow-block-width  (~length block-width  ((default-diaflow-block-width)))]
                   [default-diaflow-block-height (~length block-height ((default-diaflow-block-height)))]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or node-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or edge-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define path (dia-initial-path #false grid-width grid-height +0.5 0.0+0.0i '#:home))
      
      (if (or downward?)
          (with-gomamon! path
            (move-down ystep (diaflowlet-function->anchor f) (diaflowlet-input-desc in alt-in))
            (move-down ystep '$ (diaflowlet-output-desc alt-out f in)))
          (with-gomamon! path
            (move-right xstep (diaflowlet-function->anchor f) (diaflowlet-input-desc in alt-in))
            (move-right xstep '$ (diaflowlet-output-desc alt-out f in))))
      
      (define flowlet
        (dia-path-flow path
                       #:id id #:path-operator path-op #:flow-operator flow-op 
                       #:border bdr #:background bg #:margin margin #:padding padding
                       #:λblock block-detect #:λarrow arrow-detect
                       #:λnode make-node #:λnode-label make-node-label
                       #:λedge make-edge #:λedge-label make-edge-label))
      
      (assert flowlet dia:flow?))))

(define #:forall (In) dia-flowlet-join
  (lambda [#:id [id : (Option Symbol) #false]
           #:and? [and? : Boolean #true]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real (default-diaflowlet-block-width)]
           #:block-height [block-height : Real (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real -0.65] #:grid-height [grid-height : Real -0.65]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:node-font [node-font : (Option Font) (default-diaflowlet-node-font)]
           #:edge-font [edge-font : (Option Font) (default-diaflowlet-edge-font)]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier diaflowlet-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) diaflowlet-node-construct]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:input-desc [alt-in : (Listof (Option DC-Markup-Text)) null]
           #:output-desc [alt-out : (Listof (Option DC-Markup-Text)) null]
           [f : (U (-> In Any) (Pairof (-> In Any) (Listof (-> In Any))))] [ins : (Pairof In (Listof In))]] : Dia:Flow
    (define size : Index (length ins))
    (define fs (if (pair? f) (list->n:vector f size) (make-vector size f)))
    (define in-descs (list->n:vector alt-in size #false))
    (define out-descs (list->n:vector alt-out size #false))
    (define-values (funcs func-descs) (diaflowlet-funcion-rename fs))
    (define base-width  (~length block-width  ((default-diaflow-block-width))))
    (define base-height (~length block-height ((default-diaflow-block-height))))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or node-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or edge-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-selection-block-height (* base-height 0.25)]
                   [default-diaflow-junction-block-height (* base-height 0.25)]
                   [default-diaflow-node-label-string func-descs]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define path (dia-initial-path #false grid-width grid-height +0.5 0.0+0.0i '#:home))
      (define symbol-anchor (if and? '=* '-+))
      
      (if (or downward?)
          (let ([xjoin (* (sub1 size) xstep 0.5)])
            (gomamon-jump-to! path (make-rectangular xjoin (* ystep 2.0)) symbol-anchor)
            (gomamon-move-down! path (* ystep 0.5) '$)
            (for ([f (in-list funcs)]
                  [in (in-list ins)]
                  [idx (in-naturals)])
              (with-gomamon! path
                (jump-to (make-rectangular (* idx xstep) 0.0) (diaflowlet-start->anchor idx))
                (move-down ystep (diaflowlet-function->anchor f) (diaflowlet-input-desc in (vector-ref in-descs idx)))
                (L-step symbol-anchor (diaflowlet-output-desc (vector-ref out-descs idx) f in)))))
          (let ([yjoin (* (sub1 size) ystep 0.5)])
            (gomamon-jump-to! path (make-rectangular (* xstep 2.0) yjoin) symbol-anchor)
            (gomamon-move-right! path (* xstep 0.5) '$)
            (for ([f (in-list funcs)]
                  [in (in-list ins)]
                  [idx (in-naturals)])
              (with-gomamon! path
                (jump-to (make-rectangular 0.0 (* idx ystep)) (diaflowlet-start->anchor idx))
                (move-right xstep (diaflowlet-function->anchor f) (diaflowlet-input-desc in (vector-ref in-descs idx)))
                (T-step symbol-anchor (diaflowlet-output-desc (vector-ref out-descs idx) f in))))))
          
      (define flowlet
        (dia-path-flow path
                       #:id id #:path-operator path-op #:flow-operator flow-op 
                       #:border bdr #:background bg #:margin margin #:padding padding
                       #:λblock block-detect #:λarrow arrow-detect
                       #:λnode make-node #:λnode-label make-node-label
                       #:λedge make-edge #:λedge-label make-edge-label))
      
      (assert flowlet dia:flow?))))

(define #:forall (In Out) dia-flowlet-fork
  (lambda [#:id [id : (Option Symbol) #false]
           #:or? [or? : Boolean #true]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real (default-diaflowlet-block-width)]
           #:block-height [block-height : Real (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real -0.65] #:grid-height [grid-height : Real -0.65]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:node-font [node-font : (Option Font) (default-diaflowlet-node-font)]
           #:edge-font [edge-font : (Option Font) (default-diaflowlet-edge-font)]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier diaflowlet-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) diaflowlet-node-construct]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:input-desc [alt-in : (Option DC-Markup-Text) #false]
           #:output-desc [alt-out : (Listof (U False DC-Markup-Text (-> Out (U Void DC-Markup-Text)))) null]
           [f : (-> In Out)] [ins : (Pairof In (Listof In))]] : Dia:Flow
    (define outs : (Listof (Listof Out)) ((inst group-by Out Out) values (map f ins)))
    (define size : Index (length outs))
    (define out-descs (list->n:vector alt-out size #false))
    (define base-width  (~length block-width  ((default-diaflow-block-width))))
    (define base-height (~length block-height ((default-diaflow-block-height))))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or node-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or edge-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-selection-block-height (* base-height 0.25)]
                   [default-diaflow-junction-block-height (* base-height 0.25)]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define path (dia-initial-path #false grid-width grid-height +0.5 0.0+0.0i '#:home))
      (define symbol-anchor (if or? '-+ '=*))
      (define in-ratio (if downward? 1.0 0.85))
      (define din-ratio (* in-ratio 2.0))
      (define out-ratio (- 2.5 din-ratio))
      
      (if (or downward?)
          (let ([xfork (* (sub1 size) xstep 0.5)])
            (gomamon-jump-right! path xfork '^)
            (gomamon-move-down! path (* xstep in-ratio) (diaflowlet-function->anchor f) alt-in)
            (gomamon-move-down! path (* xstep in-ratio) symbol-anchor)
            (for ([out (in-list outs)]
                  [idx (in-naturals)])
              (with-gomamon! path
                (jump-to symbol-anchor)
                (move-to (make-rectangular (* xstep idx) (* ystep din-ratio)))
                (move-down (* ystep out-ratio) (diaflowlet-terminal->anchor idx)
                           (diaflowlet-output-desc (vector-ref out-descs idx) (inst car Out) out)))))
          (let ([yfork (* (sub1 size) ystep 0.5)])
            (gomamon-jump-down! path yfork '^)
            (gomamon-move-right! path (* xstep in-ratio) (diaflowlet-function->anchor f) alt-in)
            (gomamon-move-right! path (* xstep in-ratio) symbol-anchor)
            (for ([out (in-list outs)]
                  [idx (in-naturals)])
              (with-gomamon! path
                (jump-to symbol-anchor)
                (move-to (make-rectangular (* xstep din-ratio) (* idx ystep)))
                (move-right (* xstep out-ratio) (diaflowlet-terminal->anchor idx)
                            (diaflowlet-output-desc (vector-ref out-descs idx) (inst car Out) out))))))
          
      (define flowlet
        (dia-path-flow path
                       #:id id #:path-operator path-op #:flow-operator flow-op 
                       #:border bdr #:background bg #:margin margin #:padding padding
                       #:λblock block-detect #:λarrow arrow-detect
                       #:λnode make-node #:λnode-label make-node-label
                       #:λedge make-edge #:λedge-label make-edge-label))
      
      (assert flowlet dia:flow?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flowlet-assignment
  (lambda [#:id [id : (Option Symbol) #false]
           #:block-width [block-width : Real (default-diaflowlet-block-width)]
           #:block-height [block-height : Real (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real -1.00] #:grid-height [grid-height : Real -0.85]
           #:xstep [xstep : Real 1.00] #:ystep [ystep : Real 0.618]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:node-font [node-font : (Option Font) (default-diaflowlet-node-font)]
           #:edge-font [edge-font : (Option Font) (default-diaflowlet-edge-font)]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier diaflowlet-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) diaflowlet-node-construct]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:read-desc [read-desc : Any "Read"] #:write-desc [write-desc : Any "Write"]
           [variable : Symbol] [f : Symbol] [out-desc : Any]] : Dia:Flow
    (define base-width  : Nonnegative-Flonum (~length block-width  ((default-diaflow-block-width))))
    (define base-height : Nonnegative-Flonum (~length block-height ((default-diaflow-block-height))))

    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-storage-block-width  base-height]
                   [default-diaflow-storage-block-height (* base-height 0.618)]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or edge-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or edge-font ((default-diaflow-storage-arrow-font)))])
      (define v : Symbol (string->symbol (format "/proc/~a" variable)))
      (define path (dia-initial-path #false grid-width grid-height 0.5 0.0+0.0i v))
    
      (with-gomamon! path
        (move-right xstep f (cons read-desc (symbol->immutable-string variable)))
        (move-right (* xstep 0.618) #false out-desc)
        (move-down ystep)
        (T-step v #false write-desc))
      
      (define flowlet
        (dia-path-flow path
                       #:id id #:path-operator path-op #:flow-operator flow-op 
                       #:border bdr #:background bg #:margin margin #:padding padding
                       #:λblock block-detect #:λarrow arrow-detect
                       #:λnode make-node #:λnode-label make-node-label
                       #:λedge make-edge #:λedge-label make-edge-label))
      
      (assert flowlet dia:flow?))))
