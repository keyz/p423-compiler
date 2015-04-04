#|

A8 - Mar 6, 2015

pass: assign-new-frame

For each body, this pass determines the size of the body's frame, based on the locations of the variables and frame variables in the call-live list. It then selects homes for each of the new-frame variables listed in the new-frames form and rewrites the code of the body to place the callee's frame just above the body's frame.

The size of the frame is, simply, one more than the maximum index of the frame locations of the call-live variables or frame variables. The index of a frame variable can be determined directly via the helpers.ss procedure frame-var->index, while determining the index of a variable involves first finding, via the locate form, the frame variable to which it has been assigned.

Once the frame size n has been determined, each outgoing sequence of new-frame variables is assigned to the frame locations fv_n, fv_n+1, etc. These are recorded in the locate form along with the assignments already made there by the preceding pass. In addition, each return-point form is rewritten to increment and decrement the frame pointer register by the number of bytes nb represented by the n items in the procedure's frame.

Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            
              (new-frames (Frame *)
	        (locate ((UVar FVar) *)
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
UFVar   ::= UVar | FVar                                 

Output Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            ;;      mod
              (ulocals (UVar *)
                (locate ((UVar FVar) *) 
                  (frame-conflict ((UVar Var *) *)
                    Tail))))
          | (locate ((UVar Loc) *) Tail)                ;;      mod
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

|#

(library (Compiler assign-new-frame)
  (export assign-new-frame)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils))

  (define-who assign-new-frame

    (define (flatten orig-sexp)
      (let loop ([sexp orig-sexp] [acc '()])
	(cond [(null? sexp) acc]
	      [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
	      [else (cons sexp acc)])))

    (define (get-size var* home*)
      (let ([num* (map (lambda (x)
			 (if (frame-var? x) 
			     (frame-var->index x) ;; cool
			     (frame-var->index (cadr (assq x home*))))) ;; lookup
		       var*)])
	(if (null? num*)
	    0
	    (add1 (apply max num*)))))

    (define (get-frames size)
      (lambda (var*)
	(let loop ([idx size] [var* var*] [acc '()])
	  (let ([fvar (index->frame-var idx)])
	    (cond
	     [(null? var*) acc]
	     [else (loop (add1 idx)
			 (cdr var*)
			 (cons `(,(car var*) ,fvar) acc))])))))
    
    (define Effect
      (lambda (size)
	(lambda (x)
	  (match x
            [(set! ,var (mref ,t1 ,t2)) ;; a8 new
             `(set! ,var (mref ,t1 ,t2))]
            [(mset! ,t1 ,t2 ,t3) ;; a8 new
             `(mset! ,t1 ,t2 ,t3)]
	    [(nop) '(nop)]
	    [(if ,[(Pred size) -> test] ,[conseq] ,[altern])
	     `(if ,test ,conseq ,altern)]
	    [(begin ,[ef*] ... ,[ef])
	     `(begin ,ef* ... ,ef)]
	    [(set! ,var ,triv)
	     `(set! ,var ,triv)]
	    [(return-point ,label ,tail)
	     (make-begin
	      `((set! ,frame-pointer-register
		      (+ ,frame-pointer-register
			 ,(ash size word-shift)))
		(return-point ,label ,tail)
		(set! ,frame-pointer-register
		      (- ,frame-pointer-register
			 ,(ash size word-shift)))))]
	    [,x (errorf who "invalid Effect ~s" x)]))))
    
    (define Pred
      (lambda (size)
	(lambda (x)
	  (match x
	    [(true) '(true)]
	    [(false) '(false)]
	    [(if ,[test] ,[conseq] ,[altern])
	     `(if ,test ,conseq ,altern)]
	    [(begin ,[(Effect size) -> ef*] ... ,[p])
	     `(begin ,ef* ... ,p)]
	    [(,relop ,x ,y)
	     `(,relop ,x ,y)]
	    [,x (errorf who "invalid Pred ~s" x)]))))
    
    (define Tail
      (lambda (size)
	(lambda (x)
	  (match x
	    [(if ,[(Pred size) -> test] ,[conseq] ,[altern])
	     `(if ,test ,conseq ,altern)]
	    [(begin ,[(Effect size) -> ef*] ... ,[tail])
	     `(begin ,ef* ... ,tail)]
	    [(,triv ,loc* ...)
	     `(,triv ,loc* ...)]
	    [,x (errorf who "invalid Tail ~s" x)]))))
    
    (define Body
      (lambda (x)
	(match x
	  [(locals (,local* ...)
             (new-frames (,frame* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct
                   (call-live (,call-live* ...) ,tail)))))
	   (let* ([size (get-size call-live* home*)]
		  [n-frames (map (get-frames size) frame*)]
		  [n-locs (hdifference local* (flatten frame*))])
	     `(locals (,n-locs ...)
                (ulocals ()
                  (locate (,home* ... ,n-frames ... ...)
                    (frame-conflict
                      ,ct
                      ,((Tail size) tail))))))]
	  [,x (errorf who "invalid Body ~s" x)])))
    
    (lambda (x)
      (match x
	[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
	 `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
	[,x (errorf who "invalid Program ~s" x)])))

  )
