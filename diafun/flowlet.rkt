#lang typed/racket/base

(provide (all-defined-out))
(provide diaflowlet-block-construct diaflowlet-arrow-identify)

(require racket/symbol)
(require racket/list)

(require digimon/metrics)
(require digimon/sequence)

(require "flowchart.rkt")
(require "digitama/track/dc.rkt")
(require "digitama/block/style.rkt")
(require "digitama/flowchart/flowlet.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflowlet-block-width : (Parameterof Nonnegative-Flonum) (make-parameter 150.0))
(define default-diaflowlet-block-height : (Parameterof Nonnegative-Flonum) (make-parameter (* (default-diaflowlet-block-width) 0.50)))

(define default-diaflowlet-block-font : (Parameterof Font) (make-parameter (desc-font #:family 'math     #:size 'xx-large)))
(define default-diaflowlet-path-font : (Parameterof Font) (make-parameter (desc-font #:family 'math      #:size 'x-large)))
(define default-diaflowlet-file-font : (Parameterof Font) (make-parameter (desc-font #:family 'monospace #:size 'xx-large)))
(define default-diaflowlet-stroke-thickness : (Parameterof Nonnegative-Flonum) (make-parameter 1.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flowlet-read
  (lambda [#:id [id : (Option Symbol) #false]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(81 %)] #:grid-height [grid-height : Real+% '(81 %)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0] #:file-position [file-position : Real 0.45]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:file-font [file-font : (Option Font) (default-diaflowlet-file-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:input-desc [alt-in : (Option DC-Markup-Text) #false]
           #:output-desc [alt-out : (Option (-> Any (U Void DC-Markup-Text))) #false]
           #:reader [f : (-> Input-Port Any) read] #:peek-size [peek-size : Index 8]
           [in : Input-Port] [repeats : Index 1]] : Dia:Flow
    (define funcs (diaflowlet-function-rename (list->n:vector (list f) (+ repeats 1))))
    (define base-width  : Nonnegative-Flonum (~length block-width  ((default-diaflow-block-width))))
    (define base-height : Nonnegative-Flonum (~length block-height ((default-diaflow-block-height))))
    (define in-desc (diaflowlet-input-desc in alt-in))
    (define offset (real->double-flonum file-position))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-block-width  (* base-height 0.80)]
                   [default-diaflow-storage-block-height (* base-height 0.618)]
                   [default-diaflow-storage-font (or file-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-storage-arrow-label-rotate? rotation?]
                   [default-dia-block-text-alignment 'left]
                   [default-dia-block-text-trim? #false])
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:home base-width))

      (if (or downward?)
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (diaflowlet-function->anchor r)])
                  (with-gomamon! self
                    (move-down ystep f-anchor (if (> idx 0) (diaflowlet-output-desc (r in) alt-out) in-desc))
                    (jump-left (* xstep offset))
                    (jump-up (* ystep (if (> idx 0) 0.5 0.618)) (diaflowlet-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! self
                  (move-down ystep '$ (diaflowlet-output-desc (r in) alt-out))
                  (jump-left (* xstep offset))
                  (jump-up (* ystep 0.382) (diaflowlet-file-anchor in peek-size)))))
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (diaflowlet-function->anchor r)])
                  (gomamon-move-right! self xstep f-anchor (if (> idx 0) (diaflowlet-output-desc (r in) alt-out) in-desc))
                  (with-gomamon! self
                    (jump-down (* ystep offset))
                    (jump-left (* xstep (if (> idx 0) 0.5 0.618)) (diaflowlet-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! self
                  (move-right xstep '$ (diaflowlet-output-desc (r in) alt-out))
                  (jump-down (* ystep offset))
                  (jump-left (* xstep 0.382) (diaflowlet-file-anchor in peek-size))))))
            
      (define flowlet
        (dia-track-flow self
                        #:id id #:border bdr #:background bg #:margin margin #:padding padding
                        #:block-detect block-detect #:track-detect track-detect
                        #:λblock make-block #:λbrief make-brief #:block-desc diaflowlet-block-describe
                        #:λpath make-path #:λlabel make-label))
      
      (assert flowlet dia:flow?))))

(define #:forall (In Out) dia-flowlet-function
  (lambda [#:id [id : (Option Symbol) #false]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(81 %)] #:grid-height [grid-height : Real+% '(81 %)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:input-desc [alt-in : (U False DC-Markup-Text (-> In (U Void DC-Markup-Text))) #false]
           #:output-desc [alt-out : (U False DC-Markup-Text (-> Out (U Void DC-Markup-Text))) #false]
           [f : (U (-> In Out) Symbol String)] [in : In]] : Dia:Flow
    (define base-width  (~length block-width  ((default-diaflow-block-width))))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height (~length block-height ((default-diaflow-block-height)))]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or block-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:home base-width))
      
      (if (or downward?)
          (with-gomamon! self
            (move-down ystep (diaflowlet-function->anchor f) (diaflowlet-input-desc in alt-in))
            (move-down ystep '$ (diaflowlet-output-desc alt-out f in)))
          (with-gomamon! self
            (move-right xstep (diaflowlet-function->anchor f) (diaflowlet-input-desc in alt-in))
            (move-right xstep '$ (diaflowlet-output-desc alt-out f in))))
      
      (define flowlet
        (dia-track-flow self
                        #:id id #:border bdr #:background bg #:margin margin #:padding padding
                        #:block-detect block-detect #:track-detect track-detect
                        #:λblock make-block #:λbrief make-brief
                        #:λpath make-path #:λlabel make-label))
      
      (assert flowlet dia:flow?))))

(define #:forall (T) dia-flowlet-functions
  (lambda [#:id [id : (Option Symbol) #false]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(81 %)] #:grid-height [grid-height : Real+% '(81 %)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:input-desc [alt-in : (Option DC-Markup-Text) #false]
           #:output-desc [alt-out : (Option (-> Any (U Void DC-Markup-Text))) #false]
           [in : T] [f : (-> T T)] . [fs : (-> T T) *]] : Dia:Flow
    (define funcs (diaflowlet-function-rename (cons f fs)))
    (define base-width  : Nonnegative-Flonum (~length block-width  ((default-diaflow-block-width))))
    (define in-desc (diaflowlet-input-desc in alt-in))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height (~length block-height ((default-diaflow-block-height)))]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or block-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:home base-width))

      (if (or downward?)
          (let ([out (for/fold ([v : T in])
                               ([f (in-list funcs)]
                                [idx (in-naturals)])
                       (gomamon-move-down! self ystep (diaflowlet-function->anchor f) (if (> idx 0) (diaflowlet-output-desc v alt-out) in-desc))
                       (f v))])
            (gomamon-move-down! self ystep '$ (diaflowlet-output-desc out alt-out)))
          (let ([out (for/fold ([v : T in])
                               ([f (in-list funcs)]
                                [idx (in-naturals)])
                       (gomamon-move-right! self xstep (diaflowlet-function->anchor f) (if (> idx 0) (diaflowlet-output-desc v alt-out) in-desc))
                       (f v))])
            (gomamon-move-right! self xstep '$ (diaflowlet-output-desc out alt-out))))
            
      (dia-track-flow self
                      #:id id #:border bdr #:background bg #:margin margin #:padding padding
                      #:block-detect block-detect #:track-detect track-detect
                      #:λblock make-block #:λbrief make-brief #:block-desc diaflowlet-block-describe
                      #:λpath make-path #:λlabel make-label))))

(define #:forall (In) dia-flowlet-join
  (lambda [#:id [id : (Option Symbol) #false]
           #:and? [and? : Boolean #true]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(65 %)] #:grid-height [grid-height : Real+% '(65 %)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:input-desc [alt-in : (Listof (Option DC-Markup-Text)) null]
           #:output-desc [alt-out : (Listof (U False DC-Markup-Text (-> Any (U Void DC-Markup-Text)))) null]
           [f : (U (-> In Any) (Pairof (-> In Any) (Listof (-> In Any))))] [ins : (Pairof In (Listof In))]] : Dia:Flow
    (define size : Index (length ins))
    (define fs (if (pair? f) (list->n:vector f size) (make-vector size f)))
    (define in-descs (list->n:vector alt-in size #false))
    (define out-descs (list->n:vector alt-out size #false))
    (define funcs (diaflowlet-function-rename fs))
    (define base-width  (~length block-width  ((default-diaflow-block-width))))
    (define base-height (~length block-height ((default-diaflow-block-height))))
    
    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or block-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-selection-block-height (* base-height 0.25)]
                   [default-diaflow-junction-block-height (* base-height 0.25)]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:home base-width))
      (define symbol-anchor (if and? '=* '-+))
      
      (if (or downward?)
          (let ([xjoin (* (sub1 size) xstep 0.5)])
            (gomamon-jump-to! self (make-rectangular xjoin (* ystep 2.0)) symbol-anchor)
            (gomamon-move-down! self (* ystep 0.5) '$)
            (for ([f (in-list funcs)]
                  [in (in-list ins)]
                  [idx (in-naturals)])
              (with-gomamon! self
                (jump-to (make-rectangular (* idx xstep) 0.0) (diaflowlet-start->anchor idx))
                (move-down ystep (diaflowlet-function->anchor f) (diaflowlet-input-desc in (vector-ref in-descs idx)))
                (L-step symbol-anchor (diaflowlet-output-desc (vector-ref out-descs idx) f in)))))
          (let ([yjoin (* (sub1 size) ystep 0.5)])
            (gomamon-jump-to! self (make-rectangular (* xstep 2.0) yjoin) symbol-anchor)
            (gomamon-move-right! self (* xstep 0.5) '$)
            (for ([f (in-list funcs)]
                  [in (in-list ins)]
                  [idx (in-naturals)])
              (with-gomamon! self
                (jump-to (make-rectangular 0.0 (* idx ystep)) (diaflowlet-start->anchor idx))
                (move-right xstep (diaflowlet-function->anchor f) (diaflowlet-input-desc in (vector-ref in-descs idx)))
                (T-step symbol-anchor (diaflowlet-output-desc (vector-ref out-descs idx) f in))))))
          
      (dia-track-flow self
                      #:id id #:border bdr #:background bg #:margin margin #:padding padding
                      #:block-detect block-detect #:track-detect track-detect
                      #:λblock make-block #:λbrief make-brief #:block-desc diaflowlet-block-describe
                      #:λpath make-path #:λlabel make-label))))

(define #:forall (In Out) dia-flowlet-fork
  (lambda [#:id [id : (Option Symbol) #false]
           #:or? [or? : Boolean #true]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(65 %)] #:grid-height [grid-height : Real+% '(65 %)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
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
                   [default-diaflow-storage-font (or block-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))]
                   [default-diaflow-selection-block-height (* base-height 0.25)]
                   [default-diaflow-junction-block-height (* base-height 0.25)]
                   [default-diaflow-storage-arrow-label-rotate? rotation?])
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:home base-width))
      (define symbol-anchor (if or? '-+ '=*))
      (define in-ratio (if downward? 1.0 0.85))
      (define din-ratio (* in-ratio 2.0))
      (define out-ratio (- 2.5 din-ratio))
      
      (if (or downward?)
          (let ([xfork (* (sub1 size) xstep 0.5)])
            (gomamon-jump-right! self xfork '^)
            (gomamon-move-down! self (* xstep in-ratio) (diaflowlet-function->anchor f) alt-in)
            (gomamon-move-down! self (* xstep in-ratio) symbol-anchor)
            (for ([out (in-list outs)]
                  [idx (in-naturals)])
              (with-gomamon! self
                (jump-to symbol-anchor)
                (move-to (make-rectangular (* xstep idx) (* ystep din-ratio)))
                (move-down (* ystep out-ratio) (diaflowlet-terminal->anchor idx)
                           (diaflowlet-output-desc (vector-ref out-descs idx) (inst car Out) out)))))
          (let ([yfork (* (sub1 size) ystep 0.5)])
            (gomamon-jump-down! self yfork '^)
            (gomamon-move-right! self (* xstep in-ratio) (diaflowlet-function->anchor f) alt-in)
            (gomamon-move-right! self (* xstep in-ratio) symbol-anchor)
            (for ([out (in-list outs)]
                  [idx (in-naturals)])
              (with-gomamon! self
                (jump-to symbol-anchor)
                (move-to (make-rectangular (* xstep din-ratio) (* idx ystep)))
                (move-right (* xstep out-ratio) (diaflowlet-terminal->anchor idx)
                            (diaflowlet-output-desc (vector-ref out-descs idx) (inst car Out) out))))))
          
      (dia-track-flow self
                      #:id id #:border bdr #:background bg #:margin margin #:padding padding
                      #:block-detect block-detect #:track-detect track-detect
                      #:λblock make-block #:λbrief make-brief
                      #:λpath make-path #:λlabel make-label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flowlet-assignment
  (lambda [#:id [id : (Option Symbol) #false]
           #:block-width [block-width : Real+% (default-diaflowlet-block-width)]
           #:block-height [block-height : Real+% (default-diaflowlet-block-height)]
           #:grid-width [grid-width : Real+% '(100 %)] #:grid-height [grid-height : Real+% '(85 %)]
           #:xstep [xstep : Real 1.00] #:ystep [ystep : Real 0.618]
           #:stroke-width [stroke-width : (Option Real) (default-diaflowlet-stroke-thickness)]
           #:block-font [block-font : (Option Font) (default-diaflowlet-block-font)]
           #:path-font [path-font : (Option Font) (default-diaflowlet-path-font)]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint 'White]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier diaflowlet-arrow-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) diaflowlet-block-construct]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:read-desc [read-desc : Any "Read"] #:write-desc [write-desc : Any "Write"]
           [variable : Symbol] [f : Symbol] [out-desc : Any]] : Dia:Flow
    (define base-width  : Nonnegative-Flonum (~length block-width  ((default-diaflow-block-width))))
    (define base-height : Nonnegative-Flonum (~length block-height ((default-diaflow-block-height))))

    (parameterize ([default-diaflow-block-width  base-width]
                   [default-diaflow-block-height base-height]
                   [default-diaflow-storage-block-width  base-height]
                   [default-diaflow-storage-block-height (* base-height 0.618)]
                   [default-diaflow-process-stroke-width (if stroke-width (~length stroke-width) ((default-diaflow-process-stroke-width)))]
                   [default-diaflow-storage-font (or path-font ((default-diaflow-storage-font)))]
                   [default-diaflow-storage-arrow-font (or path-font ((default-diaflow-storage-arrow-font)))])
      (define v : Symbol (string->symbol (format "/proc/~a" variable)))
      (define self (dia-initial-track #false grid-width grid-height 0.5 0.0+0.0i v base-width))
    
      (with-gomamon! self
        (move-right xstep f (cons read-desc (symbol->immutable-string variable)))
        (move-right (* xstep 0.618) #false out-desc)
        (move-down ystep)
        (T-step v #false write-desc))
      
      (dia-track-flow self
                      #:id id #:border bdr #:background bg #:margin margin #:padding padding
                      #:block-detect block-detect #:track-detect track-detect
                      #:λblock make-block #:λbrief make-brief
                      #:λpath make-path #:λlabel make-label))))
