#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "flow/self.rkt"))
(provide (all-from-out "flow/style.rkt"))

(require digimon/struct)

(require racket/symbol)
(require racket/list)

(require digimon/measure)
(require digimon/sequence)

(require diafun/flowchart)
(require diafun/digitama/track/dc)
(require diafun/digitama/track/interface)
(require diafun/digitama/block/interface)
(require diafun/digitama/block/style)

(require diafun/digitama/flowchart/self)
(require diafun/digitama/flowchart/identifier)

(require "flow/self.rkt")
(require "flow/style.rkt")
(require "flow/trick.rkt")
(require "flow/customize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-read
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
           #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
           #:block-scale [block-scale : Nonnegative-Real 1.00]
           #:downward? [downward? : Boolean #false]
           #:rotate-label? [rotation? : Boolean (not downward?)]
           #:grid-width [grid-width : Length+% (~% 100)]
           #:grid-height [grid-height : Length+% (~% 81)]
           #:xstep [xstep : Real 1.0]
           #:ystep [ystep : Real 1.0]
           #:file-position [file-position : Real 0.42]
           #:input-desc [alt-in : Geo-Option-Rich-Text #false]
           #:output-desc [alt-out : (Option (-> Any (U Void Geo-Rich-Text))) #false]
           #:reader [f : (-> Input-Port Any) read] #:peek-size [peek-size : Index 8]
           [in : Input-Port] [repeats : Index 1]] : Dia:FlowChart
    (parameterize ([default-flow-file-style (make-flow-file-style #:width (~% 40.0) #:height (~% 61.8) #:padding (~% 8))]
                   [default-flow~storage~style (make-flow~storage~style #:label-rotate? rotation?)])
      (define-values (base-width base-height) (dia-block-reference-size block-factory))
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:>>
                                      (* base-width (real->double-flonum block-scale))))
      
      (define funcs (plt-flow-function-rename (list->n:vector (list f) (+ repeats 1))))
      (define in-desc (plt-flow-input-desc in alt-in))
      (define offset (real->double-flonum file-position))
      
      (if (or downward?)
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (plt-flow-function->anchor r)])
                  (with-gomamon! self
                    (move-down ystep f-anchor (if (> idx 0) (plt-flow-output-desc (r in) alt-out) in-desc))
                    (jump-left (* xstep offset))
                    (jump-up (* ystep (if (> idx 0) 0.5 0.618)) (plt-flow-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! self
                  (move-down ystep '<< (plt-flow-output-desc (r in) alt-out))
                  (jump-left (* xstep offset))
                  (jump-up (* ystep 0.382) (plt-flow-file-anchor in peek-size)))))
          (for ([idx (in-range (+ repeats 1))]
                [r (in-list funcs)])
            (if (< idx repeats)
                (let ([f-anchor (plt-flow-function->anchor r)])
                  (gomamon-move-right! self xstep f-anchor (if (> idx 0) (plt-flow-output-desc (r in) alt-out) in-desc))
                  (with-gomamon! self
                    (jump-down (* ystep offset))
                    (jump-left (* xstep (if (> idx 0) 0.5 0.618)) (plt-flow-file-anchor in peek-size))
                    (jump-to f-anchor)))
                (with-gomamon! self
                  (move-right xstep '<< (plt-flow-output-desc (r in) alt-out))
                  (jump-down (* ystep offset))
                  (jump-left (* xstep 0.382) (plt-flow-file-anchor in peek-size))))))
    
      ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
       #:id id #:frame frame #:block-desc plt-flow-block-describe
       #:track-factory track-factory #:block-factory block-factory
       #:block-scale block-scale 
       self))))

(define #:forall (In Out) plt-flow-function
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : (U Geo-Box Maybe-Fill-Paint) #false]
           #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
           #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
           #:block-scale [block-scale : Nonnegative-Real 1.00]
           #:downward? [downward? : Boolean #false]
           #:rotate-label? [rotation? : Boolean (not downward?)]
           #:grid-width [grid-width : Length+% (~% 100)]
           #:grid-height [grid-height : Length+% (~% 81)]
           #:xstep [xstep : Real 1.0]
           #:ystep [ystep : Real 1.0]
           #:input-desc [alt-in : (U Geo-Option-Rich-Text (-> In (U Void Geo-Rich-Text))) #false]
           #:output-desc [alt-out : (U Geo-Option-Rich-Text (-> Out (U Void Geo-Rich-Text))) #false]
           [f : (U (-> In Out) Symbol String)] [in : In]] : Dia:FlowChart
    (parameterize ([default-flow~storage~style (make-flow~storage~style #:label-rotate? rotation?)])
      (define-values (base-width base-height) (dia-block-reference-size block-factory))
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:>>
                                      (* base-width (real->double-flonum block-scale))))
      
      (if (or downward?)
          (with-gomamon! self
            (move-down ystep (plt-flow-function->anchor f) (plt-flow-input-desc in alt-in))
            (move-down ystep '<< (plt-flow-output-desc alt-out f in)))
          (with-gomamon! self
            (move-right xstep (plt-flow-function->anchor f) (plt-flow-input-desc in alt-in))
            (move-right xstep '<< (plt-flow-output-desc alt-out f in))))
      
      ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
       #:id id #:frame frame #:block-desc plt-flow-block-describe
       #:track-factory track-factory #:block-factory block-factory
       #:block-scale block-scale 
       self))))

(define #:forall (T) plt-flow-functions
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : (U Geo-Box Maybe-Fill-Paint) #false]
           #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
           #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
           #:block-scale [block-scale : Nonnegative-Real 1.00]
           #:downward? [downward? : Boolean #false]
           #:rotate-label? [rotation? : Boolean (not downward?)]
           #:grid-width [grid-width : Length+% (~% 100)]
           #:grid-height [grid-height : Length+% (~% 81)]
           #:xstep [xstep : Real 1.0]
           #:ystep [ystep : Real 1.0]
           #:input-desc [alt-in : Geo-Option-Rich-Text #false]
           #:output-desc [alt-out : (Option (-> Any (U Void Geo-Rich-Text))) #false]
           [in : T] [f : (-> T T)] . [fs : (-> T T) *]] : Dia:FlowChart
    (parameterize ([default-flow~storage~style (make-flow~storage~style #:label-rotate? rotation?)])
      (define-values (base-width base-height) (dia-block-reference-size block-factory))
      (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:>>
                                      (* base-width (real->double-flonum block-scale))))

      (define in-desc (plt-flow-input-desc in alt-in))
      (define funcs (plt-flow-function-rename (cons f fs)))
      
      (if (or downward?)
          (let ([out (for/fold ([v : T in])
                               ([f (in-list funcs)]
                                [idx (in-naturals)])
                       (gomamon-move-down! self ystep (plt-flow-function->anchor f) (if (> idx 0) (plt-flow-output-desc v alt-out) in-desc))
                       (f v))])
            (gomamon-move-down! self ystep '<< (plt-flow-output-desc out alt-out)))
          (let ([out (for/fold ([v : T in])
                               ([f (in-list funcs)]
                                [idx (in-naturals)])
                       (gomamon-move-right! self xstep (plt-flow-function->anchor f) (if (> idx 0) (plt-flow-output-desc v alt-out) in-desc))
                       (f v))])
            (gomamon-move-right! self xstep '<< (plt-flow-output-desc out alt-out))))
      
      ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
       #:id id #:frame frame #:block-desc plt-flow-block-describe
       #:track-factory track-factory #:block-factory block-factory
       #:block-scale block-scale 
       self))))

(define #:forall (In) plt-flow-join
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : (U Geo-Box Maybe-Fill-Paint) #false]
           #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
           #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
           #:block-scale [block-scale : Nonnegative-Real 1.00]
           #:and? [and? : Boolean #true]
           #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
           #:grid-width [grid-width : Length+% (~% 85)]
           #:grid-height [grid-height : Length+% (~% 61.8)]
           #:xstep [xstep : Real 1.0] #:ystep [ystep : Real 1.0]
           #:input-desc [alt-in : (Listof Geo-Option-Rich-Text) null]
           #:output-desc [alt-out : (Listof (U Geo-Option-Rich-Text (-> Any (U Void Geo-Rich-Text)))) null]
           [f : (U (-> In Any) (Pairof (-> In Any) (Listof (-> In Any))))] [ins : (Pairof In (Listof In))]] : Dia:FlowChart
      (parameterize ([default-flow-junction-style (make-flow-junction-style #:height (~% 32))]
                     [default-flow-selection-style (make-flow-selection-style #:height (~% 32))]
                     [default-flow~storage~style (make-flow~storage~style #:label-rotate? rotation?)])
        (define-values (base-width base-height) (dia-block-reference-size block-factory))
        (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:>>
                                        (* base-width (real->double-flonum block-scale))))

        (define size : Index (length ins))
        (define fs (if (pair? f) (list->n:vector f size) (make-vector size f)))
        (define in-descs (list->n:vector alt-in size #false))
        (define out-descs (list->n:vector alt-out size #false))
        (define funcs (plt-flow-function-rename fs))
        
        (define symbol-anchor (if and? '=* '-+))
      
        (if (or downward?)
            (let ([xjoin (* (sub1 size) xstep 0.5)])
              (gomamon-jump-to! self (make-rectangular xjoin (* ystep 2.0)) symbol-anchor)
              (gomamon-move-down! self (* ystep 0.5) '<<)
              (for ([f (in-list funcs)]
                    [in (in-list ins)]
                    [idx (in-naturals)])
                (with-gomamon! self
                  (jump-to (make-rectangular (* idx xstep) 0.0) (plt-flow-start->anchor idx))
                  (move-down ystep (plt-flow-function->anchor f) (plt-flow-input-desc in (vector-ref in-descs idx)))
                  (L-step symbol-anchor (plt-flow-output-desc (vector-ref out-descs idx) f in)))))
            (let ([yjoin (* (sub1 size) ystep 0.5)])
              (gomamon-jump-to! self (make-rectangular (* xstep 2.0) yjoin) symbol-anchor)
              (gomamon-move-right! self (* xstep 0.5) '<<)
              (for ([f (in-list funcs)]
                    [in (in-list ins)]
                    [idx (in-naturals)])
                (with-gomamon! self
                  (jump-to (make-rectangular 0.0 (* idx ystep)) (plt-flow-start->anchor idx))
                  (move-right xstep (plt-flow-function->anchor f) (plt-flow-input-desc in (vector-ref in-descs idx)))
                  (T-step symbol-anchor (plt-flow-output-desc (vector-ref out-descs idx) f in))))))
          
        ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
       #:id id #:frame frame #:block-desc plt-flow-block-describe
       #:track-factory track-factory #:block-factory block-factory
       #:block-scale block-scale 
       self))))

(define #:forall (In Out) plt-flow-fork
    (lambda [#:id [id : (Option Symbol) #false]
             #:frame [frame : (U Geo-Box Maybe-Fill-Paint) #false]
             #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
             #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
             #:block-scale [block-scale : Nonnegative-Real 1.00]
             #:or? [or? : Boolean #true]
             #:downward? [downward? : Boolean #false] #:rotate-label? [rotation? : Boolean (not downward?)]
             #:grid-width [grid-width : Length+% (~% 85)]
             #:grid-height [grid-height : Length+% (~% 61.8)]
             #:xstep [xstep : Real 1.0]
             #:ystep [ystep : Real 1.0]
             #:input-desc [alt-in : Geo-Option-Rich-Text #false]
             #:output-desc [alt-out : (Listof (U Geo-Option-Rich-Text (-> Out (U Void Geo-Rich-Text)))) null]
             [f : (-> In Out)] [ins : (Pairof In (Listof In))]] : Dia:FlowChart
      (parameterize ([default-flow-junction-style (make-flow-junction-style #:height (~% 32))]
                     [default-flow-selection-style (make-flow-selection-style #:height (~% 32))]
                     [default-flow~storage~style (make-flow~storage~style #:label-rotate? rotation?)])
        (define-values (base-width base-height) (dia-block-reference-size block-factory))
        (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i '#:>>
                                        (* base-width (real->double-flonum block-scale))))

        (define outs : (Listof (Listof Out)) ((inst group-by Out Out) values (map f ins)))
        (define size : Index (length outs))
        (define out-descs (list->n:vector alt-out size #false))

        (define symbol-anchor (if or? '-+ '=*))
        (define in-ratio (if downward? 1.0 0.85))
        (define din-ratio (* in-ratio 2.0))
        (define out-ratio (- 2.5 din-ratio))
      
        (if (or downward?)
            (let ([xfork (* (sub1 size) xstep 0.5)])
              (gomamon-jump-right! self xfork '^)
              (gomamon-move-down! self (* xstep in-ratio) (plt-flow-function->anchor f) alt-in)
              (gomamon-move-down! self (* xstep in-ratio) symbol-anchor)
              (for ([out (in-list outs)]
                    [idx (in-naturals)])
                (with-gomamon! self
                  (jump-to symbol-anchor)
                  (move-to (make-rectangular (* xstep idx) (* ystep din-ratio)))
                  (move-down (* ystep out-ratio) (plt-flow-terminal->anchor idx)
                             (plt-flow-output-desc (vector-ref out-descs idx) (inst car Out) out)))))
            (let ([yfork (* (sub1 size) ystep 0.5)])
              (gomamon-jump-down! self yfork '^)
              (gomamon-move-right! self (* xstep in-ratio) (plt-flow-function->anchor f) alt-in)
              (gomamon-move-right! self (* xstep in-ratio) symbol-anchor)
              (for ([out (in-list outs)]
                    [idx (in-naturals)])
                (with-gomamon! self
                  (jump-to symbol-anchor)
                  (move-to (make-rectangular (* xstep din-ratio) (* idx ystep)))
                  (move-right (* xstep out-ratio) (plt-flow-terminal->anchor idx)
                              (plt-flow-output-desc (vector-ref out-descs idx) (inst car Out) out))))))
          
        ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
         #:id id #:frame frame #:block-desc plt-flow-block-describe
         #:track-factory track-factory #:block-factory block-factory
         #:block-scale block-scale 
         self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-assignment
    (lambda [#:id [id : (Option Symbol) #false]
             #:frame [frame : (U Geo-Box Maybe-Fill-Paint) #false]
             #:track-factory [track-factory : Plt-Flow-Track-Factory (default-plt-flow-track-factory)]
             #:block-factory [block-factory : Plt-Flow-Block-Factory (default-plt-flow-block-factory)]
             #:block-scale [block-scale : Nonnegative-Real 1.00]
             #:grid-width [grid-width : Length+% (~% 100)]
             #:grid-height [grid-height : Length+% (~% 50)]
             #:xstep [xstep : Real 1.00]
             #:ystep [ystep : Real 1.00]
             #:read-desc [read-desc : Any "Read"]
             #:write-desc [write-desc : Any "Write"]
             [variable : Symbol] [f : Symbol] [out-desc : Any]] : Dia:FlowChart
      (parameterize ([default-flow-storage-style (make-flow-storage-style #:width (~% 50.0) #:height (~% 61.8) #:font plt-flow-preset-block-font)])
        (define v : Symbol (string->symbol (format "/proc/~a" variable)))
        (define-values (base-width base-height) (dia-block-reference-size block-factory))
        (define self (dia-initial-track #false grid-width grid-height +0.5 0.0+0.0i v
                                        (* base-width (real->double-flonum block-scale))))
        
        (with-gomamon! self
          (move-right xstep (plt-flow-function->anchor f) (cons read-desc (symbol->immutable-string variable)))
          (move-right (* xstep 0.618) #false out-desc)
          (move-down ystep)
          (T-step v #false write-desc))
      
        ((inst dia-track-flow* Flow-Track-Style Flow-Block-Style Flow-Block-Metadata)
         #:id id #:frame frame #:block-desc plt-flow-block-describe
         #:track-factory track-factory #:block-factory block-factory
         #:block-scale block-scale 
         self))))
