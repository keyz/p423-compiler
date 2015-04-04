#|

A8 - Mar 6, 2015

pass: uncover-frame-conflict

This pass records for each variable the set of other variables and frame locations (frame variables) with which it conflicts. This pass and uncover-register-conflict are similar. The only difference with respect to the handling of body expressions is that where uncover-register-conflict looks for registers with register?, uncover-frame-conflict looks for frame variables using frame-var?.

Frame conflicts are recorded in a new frame-conflict form wrapped around the body.

Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                           
	      (new-frames (Frame *)
	        Tail))
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Var *)  ;; Note 'Var' not 'Loc'.     
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
Frame   ::= (Uvar *)  


Output Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            ;;      mod
              (new-frames (Frame *)
	        (spills (UVar *)
	          (frame-conflict ((UVar Var *) *)
                    (call-live (UFVar *) Tail)))))
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Var *)  ;; Note 'Var' not 'Loc'.     
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
Frame   ::= (Uvar *)
UFVar   ::= UVar | FVar                                 ;;      mod

|#

(library (Compiler uncover-frame-conflict)
  (export uncover-frame-conflict)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils))

  (define-who uncover-frame-conflict

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

	(define call-live* '())

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
	      [(return-point ,label ,tail)
	       (begin
		 (set! call-live* (hunion call-live* live*))
		 (hunion (Tail tail ct) live*))]
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
	      [(if ,test ,[c-live*] ,[a-live*])
               (Pred test c-live* a-live* ct)]
	      [(,[Triv -> target-live*] ,live* ...)
	       (hunion target-live*
		      (filter
		       (lambda (x) (or (fixed? x) (uvar? x)))
		       live*))]
	      [,x (error who "invalid Tail ~s" x)])))

	(match x
	  [(locals (,uvar* ...) (new-frames (,frame* ...) ,tail))
	   (let ([ct (map (lambda (x) (cons x '())) uvar*)])
	     (let ([uvar* (filter uvar? (Tail tail ct))])
	       (unless (null? uvar*)
		 (error who "found variables ~s live on entry" uvar*)))
	     (let* ([s-uvar* (filter uvar? call-live*)]
		    [n-locs* (hdifference uvar* s-uvar*)]) ;; remember to difference!
	       `(locals (,n-locs* ...)
                  (new-frames (,frame* ...)
                    (spills ,s-uvar*
                      (frame-conflict ,ct
                        (call-live (,call-live* ...) ,tail)))))))]
	  [,x (error who "invalid Body ~s" x)]))
      )

    (lambda (x)
      (match x
	[(letrec ([,label* (lambda () ,[(Body frame-var?) -> body*])] ...) ,[(Body frame-var?) -> body])
	 `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
	[,x (error who "invalid Program ~s" x)])))
  
  )
