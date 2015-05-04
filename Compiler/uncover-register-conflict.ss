#|

;; union -> hunion

A8 - Mar 6, 2015

pass: uncover-register-conflict

this pass records conflicts between each variable and the variables and registers with which it conflicts.

For a complete body, i.e., one with just a locate wrapper, this pass should simply reproduce the body form in the output. For an incomplete body, it should process Tail expressions and insert the register-conflict form as in the preceding version. 
                               
Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            
              (ulocals (UVar *)
                (locate ((UVar FVar) *) 
                  (frame-conflict ((UVar Var *) *)
                    Tail))))
          | (locate ((UVar Loc) *) Tail)                
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Loc *)                                
Pred    ::= (true)
          | (false)
          | (if Pred Pred Pred)
          | (begin Effect * Pred)
          | (Relop Triv Triv)
Effect  ::= (nop)
          | (set! Var Triv)                            
          | (set! Var (Binop Triv Triv))               
          | (set! Var (mref Triv Triv))                 ;; a8 new
          | (return-point Label Tail)                  
          | (mset! Triv Triv Triv)                      ;; a8 new
          | (if Pred Effect Effect)
          | (begin Effect * Effect)
Triv    ::= Var                                        
          | Integer | Label
Loc     ::= Reg | FVar                                 
Var     ::= UVar | Loc

Output Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locals (UVar *)                            ;;      mod
               (ulocals (UVar *)
                 (locate ((UVar FVar) *) 
                   (frame-conflict ((UVar Var *) *)
                     (register-conflict ((UVar Conflict *) *)
                       Tail)))))
           | (locate ((UVar Loc) *) Tail)                
Tail     ::= (if Pred Tail Tail)
           | (begin Effect * Tail)
           | (Triv Loc *)                                
Pred     ::= (true)
           | (false)
           | (if Pred Pred Pred)
           | (begin Effect * Pred)
           | (Relop Triv Triv)
Effect   ::= (nop)
           | (set! Var Triv)                            
           | (set! Var (Binop Triv Triv))               
           | (set! Var (mref Triv Triv))                 ;; a8 new
           | (return-point Label Tail)                  
           | (mset! Triv Triv Triv)                      ;; a8 new
           | (if Pred Effect Effect)
           | (begin Effect * Effect)
Triv     ::= Var                                        
           | Integer | Label
Loc      ::= Reg | FVar                                 
Var      ::= UVar | Loc
Conflict ::= Reg | UVar                                  ;;      mod

|#

(library (Compiler uncover-register-conflict)
  (export uncover-register-conflict)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler utils))

  (define-who uncover-register-conflict

    (define warn-if-dead-at-assignment (make-parameter #f))
    
    (define add-conflicts!
      (lambda (ct lhs live*)
	(define add-conflict!
	  (lambda (var1 var2)
	    (let ([a (assq var1 ct)])
	      (set-cdr! a (set-cons var2 (cdr a))))))
	(when (uvar? lhs)
	  (for-each
	   (lambda (live) (add-conflict! lhs live))
	   live*))
	(for-each
	 (lambda (live) (when (uvar? live) (add-conflict! live lhs)))
	 live*)))

    (define (Body fixed?)
      (lambda (x)

	(define Triv
	  (lambda (x)
	    (if (or (uvar? x) (fixed? x)) `(,x) '())))

	(define Effect*
	  (lambda (x live* ct)
	    (match x
	      [() live*]
	      [(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
	      [,x (error who "invalid Effect* list ~s" x)])))

	(define Effect
	  (lambda (x live* ct)
	    (match x
              ;; a8 new
              [(mset! ,[Triv -> base-t] ,[Triv -> offset-t] ,[Triv -> t])
               (hunion base-t offset-t t live*)]
              ;; a8 new
              [(set! ,var (mref ,[Triv -> base-t] ,[Triv -> offset-t]))
               (add-conflicts! ct var (hunion base-t offset-t live*))
               (hunion base-t offset-t (remq var live*))]
	      [(nop) live*]
	      [(if ,test ,[c-live*] ,[a-live*])
	       (Pred test c-live* a-live* ct)]
	      [(begin ,ef* ... ,[live*])
	       (Effect* ef* live* ct)]
	      [(set! ,lhs ,rhs)
	       (guard (or (uvar? lhs) (fixed? lhs)) (not (memq lhs live*)))
	       (when (warn-if-dead-at-assignment)
	         (warning who "~s is not live at assignment ~s" lhs
			  `(set! ,lhs ,rhs)))
	       (Effect `(set! ,lhs ,rhs) (cons lhs live*) ct)]
	      [(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
	       (let ([live* (hdifference live* `(,lhs))])
		 (when (or (uvar? lhs) (fixed? lhs))
		   (add-conflicts! ct lhs live*))
		 (hunion x-live* y-live* live*))]
	      [(set! ,lhs ,var)
	       (let ([live* (hdifference live* `(,lhs))])
		 (when (or (uvar? lhs) (fixed? lhs))
		   (add-conflicts! ct lhs (remq var live*)))
		 (hunion (Triv var) live*))]
	      [(return-point ,label ,tail) ;; new
	       (Tail tail ct)]
	      [,x (error who "invalid Effect list ~s" x)])))

	(define Pred
	  (lambda (x t-live* f-live* ct)
	    (match x
	      [(true) t-live*]
	      [(false) f-live*]
	      [(if ,test ,[c-live*] ,[a-live*])
	       (Pred test c-live* a-live* ct)]
	      [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
	      [(,relop ,[Triv -> x-live*] ,[Triv -> y-live*])
	       (hunion t-live* f-live* x-live* y-live*)]
	      [,x (error who "invalid Pred ~s" x)])))

	(define Tail
	  (lambda (x ct)
	    (match x
	      [(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
	      [(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
	      [(,[Triv -> target-live*] ,live* ...)
	       (hunion target-live*
		      (filter
		       (lambda (x) (or (fixed? x) (uvar? x)))
		       live*))]
	      [,x (error who "invalid Tail ~s" x)])))

	(match x
	  [(locals (,local* ...)
             (ulocals (,ulocal* ...)
               (locate (,home* ...)
                 (frame-conflict ,fct ,tail))))
	   (let ([ct (map (lambda (x) (cons x '()))
			  (append local* ulocal*))])
	     (let ([uvar* (filter uvar? (Tail tail ct))])
	       (unless (null? uvar*)
		 (error who "found variables ~s live on entry" uvar*)))
	     `(locals (,local* ...) 
                (ulocals (,ulocal* ...)
                  (locate (,home* ...)
                    (frame-conflict ,fct
                      (register-conflict ,ct ,tail))))))]
          [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)] ;; oh.
	  [,x (errorf who "invalid Body ~s" x)])))

    (lambda (x)
      (match x
	[(letrec ([,label* (lambda () ,[(Body register?) -> body*])] ...) ,[(Body register?) -> body])
	 `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
	[,x (error who "invalid Program ~s" x)])))

  )
