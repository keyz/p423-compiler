;; P423 / P523
;; Week 9 grammars
;;
;; Passes:
;;   verify-scheme              l-01 -> l-01
;;   uncover-locals             l-01 -> l-20
;;   remoove-let                l-20 -> l-21

;;   verify-uil                 l-22 -> l-22
;;   remove-complex-opera*      l-22 -> l-23
;;   flatten-set!               l-23 -> l-24
;;   impose-calling-conventions l-24 -> l-25
;;   uncover-frame-conflict     l-25 -> l-27
;;   pre-assign-frame           l-27 -> l-28
;;   assign-new-frame           l-28 -> l-29
;;     finalize-frame-locations  l-29 -> l-30
;;     select-instructions       l-30 -> l-30
;;     uncover-register-conflict l-30 -> l-32
;;     assign-registers          l-32 -> l-33
;;     everybody-home?           l-33 -> bool
;;     assign-frame              l-33 -> l-29
;;   discard-call-live          l-33 -> l-35
;;   finalize-locations         l-35 -> l-36
;;   expose-frame-var           l-36 -> l-37
;;   expose-basic-blocks        l-37 -> l-39
;;   flatten-program            l-39 -> l-41
;;   generate-x86-64            l-41 -> ()

(p423-grammars

  (l01-verify-scheme
    (start Prog)
    (Prog
      (letrec ((Label (lambda (UVar *) Value)) *) Value))
    (Pred
      (let ([UVar Value]*) Pred)
      (true)
      (false)
      (if Pred Pred Pred)
      (begin Effect * Pred)
      (PredPrim Value *))
    (Effect
      (let ([UVar Value]*) Effect)
      (nop)
      (if Pred Effect Effect)
      (begin Effect * Effect)
      (EffectPrim Value *)
      (Value Value *))
    (Value
      (quote Immediate)
      (let ([UVar Value]*) Value)
      (if Pred Value Value)
      (begin Effect * Value)
      (ValPrim Value *)
      (Value Value *)
      UVar Label)
    ; (Immediate fixnum () #t #f) ;; BUILTIN!
    )

  (l19-specify-representation
   (%remove Prog Pred Effect Value)  ;; Remove ALL.  Start fresh.
   (%add
;    (start Prog)
    (Prog
      (letrec ((Label (lambda (UVar *) Tail)) *) Tail))
    (Tail
      (let ([UVar Value]*) Tail)
      (if Pred Tail Tail)
      (begin Effect * Tail)      
      (alloc Value)
      (mref  Value Value)
      (Binop Value Value)
      (Value Value *)
      Triv)
    (Pred
      (let ([UVar Value]*) Pred)
      (true)
      (false)
      (if Pred Pred Pred)
      (begin Effect * Pred)
      (Relop Value Value))
    (Effect
      (let ([UVar Value]*) Effect)
      (nop)
      (mset! Value Value Value)
      (if Pred Effect Effect)
      (begin Effect * Effect)
      (Value Value *))
    (Value
      (let ([UVar Value]*) Value)
      (if Pred Value Value)
      (begin Effect * Value)
      (alloc Value)
      (mref  Value Value)
      (Binop Value Value)
      (Value Value *)
      Triv)
    (Triv
      UVar
      Integer
      Label)))

  (l20-uncover-locals
   (%remove Prog)
   (%add 
    (Prog (letrec ((Label (lambda (UVar *) Body)) *) Body))
    (Body (locals (UVar *) Tail))))

  (l21-remove-let
   (%remove 
    (Tail let)
    (Pred let)
    (Effect let)
    (Value let))
   (%add (Effect (set! UVar Value))))

  (l22-verify-uil)

 ;; Replace Value with Triv in arguments of procedure calls and primitive application.
 (l23-remove-complex-opera
   (%remove
     (Tail Binop Value alloc mref)
     (Pred Relop)
     (Value alloc mref Binop Value)
     (Effect Value mset!))
   (%add
     (Tail
       (alloc Triv)
       (mref Triv Triv)
       (Binop Triv Triv)
       (Triv Triv *))
     (Pred (Relop Triv Triv))
     (Value (alloc Triv)
	    (mref Triv Triv)
	    (Binop Triv Triv)
	    (Triv Triv *))
     (Effect 
      (mset! Triv Triv Triv)
      (Triv Triv *))))

 ;; Remove Value, set! rhs may only be Triv or Binop.
 ;; We could treat mref's as Binops and save a bit in the grammar, but
 ;; then we wouldn't be able to verify that they are removed later.
 (l24-flatten-set
   (%remove
     Value
     (Effect set! Triv))
   (%add
     (Effect
       (set! UVar (alloc Triv))
       (set! UVar (mref Triv Triv))
       (set! UVar Triv)
       (set! UVar (Binop Triv Triv))
       (set! UVar (Triv Triv *))
       (Triv Triv *))))

 ;; alloc/mref will only occur on the RHS of set! after this pass:
 (l25-impose-calling-conventions
   (%remove
     (Prog letrec)
     (Tail Triv Binop alloc mref)
     (Effect set! Triv)
     (Triv UVar)
     (Body locals))
   (%add
     (Prog (letrec ((Label (lambda () Body)) *) Body))
     (Body (locals (UVar *) 
	     (new-frames (Frame *)
		Tail)))
     ;; Operands can include new frame vars (NFVs) which are
     ;; technically UVars even though they are used as locations:
     (Tail (Triv Var *))  ;; Note 'Var' not 'Loc'.
     (Effect
       (set! Var Triv)
       (set! Var (Binop Triv Triv))
       (set! Var (alloc Triv))
       (set! Var (mref Triv Triv))
       (return-point Label Tail))
     (Loc
       Reg
       FVar)
     (Var
       UVar
       Loc)
     (Triv Var)
     (Frame (UVar *))))

 ;; After this point, alloc is gone and we reference the
 ;; allocation-pointer-register directly.
 (l26-expose-allocation-pointer
   (%remove (Effect set!))
   (%add 
    (Effect (set! Var Triv)
	    (set! Var (Binop Triv Triv))
	    ;; Remove alloc!
	    (set! Var (mref Triv Triv))
	    )))

 (l27-uncover-frame-conflict
    (%remove 
      (Body locals))
    (%add
      (Body
        (locals (UVar *)
          (new-frames (Frame *)
	    (spills (UVar *)
	       (frame-conflict ((UVar Var *) *)
                 (call-live (UFVar *) Tail))))))
      (UFVar UVar FVar)))

(l28-pre-assign-frame
    (%remove 
      (Body locals))
    (%add
      (Body
        ;; Eliminate 'spills' only:
        (locals (UVar *)
          (new-frames (Frame *)
	    (locate ((UVar FVar) *)
	      (frame-conflict ((UVar Var *) *)
		(call-live (UFVar *) Tail)))))
        )))

;; This is an important grammar.  Its the one that is used at the top
;; and end of every iteration of the register-allocation loop.
(l29-assign-new-frame
    (%remove 
      (UFVar UVar FVar) ; No need for this after this point [Z]
      (Body locals)
      Frame)
    (%add
      (Body
	;; Add ulocals, remove new-frames, call-live:
        (locals (UVar *)
                (ulocals (UVar *)
                         (locate ((UVar FVar) *) 
                                 (frame-conflict ((UVar Var *) *)
                                 Tail))))
	;; Add "finished" locate form, as this grammar is used in iterative register allocation.
	(locate ((UVar Loc) *) Tail)
	)))

;; Resolve NFVs into frame vars.
(l30-finalize-frame-locations
    (%remove (Tail Triv))
    (%add (Tail (Triv Loc *))))

;; Adds register-conflict to the deeply nested Body forms.
(l32-uncover-register-conflict
  (%remove
    (Body locals))
  (%add
    ;; Ignore conflicts with frame vars:
    (Conflict Reg UVar)
    (Body
      (locals (UVar *)
              (ulocals (UVar *)
                       (locate ((UVar FVar) *) 
                               (frame-conflict ((UVar Var *) *)
                                               (register-conflict ((UVar Conflict *) *)
                                                                  Tail))))))))
;; Adds the 'spill' form.
(l33-assign-registers
  (%remove
    (Body locals)
    (Conflict))
  (%add
    (Body
      (locals (UVar *)
              (ulocals (UVar *)
                       (spills (UVar *)
                               (locate ((UVar FVar) *) 
                                       (frame-conflict ((UVar Var *) *) 
                                                       Tail))))))))


; assign-frame: This is the same as l29-assign-new-frame


(l35-discard-call-live
  (%remove
    (Body locals)
    (Tail Triv))
  (%add
    (Tail (Triv))))

(l36-finalize-locations
  (%remove
    (Body locate)
    UVar
    Var)
  (%rename
    (Body -> Tail)
    (Var -> Loc)))

(l37-expose-frame-var
  (%remove UFVar Loc)
  (%add (Loc Reg Disp)))

(l38-expose-memory-operands
   (%remove (Tail mref) 
	    (Effect mset! set!))
   (%add 
    (Loc Ind)
    (Effect
     (set! Loc Triv)
     (set! Loc (Binop Triv Triv)))))

(l39-expose-basic-blocks
  (%remove
    (Tail if)
    Pred
    (Effect nop if begin return-point))
  (%add
    (Tail
      (if (Relop Triv Triv) (Label) (Label)))))

(l41-flatten-program
  (%remove
    Prog
    Tail)
  (%rename
    (Effect -> Statement))
  (%add
    (Prog
      (code Statement * Statement))
    (Statement
      (if (Relop Triv Triv) (jump Label))
      (if (not (Relop Triv Triv)) (jump Label))
      (jump Triv)
      Label)))
)
