#|

A8 - Mar 6, 2015

pass: pre-assign-frame

This pass finds frame homes for the variables listed in the spills list. It differs from assign-frame only in the structure of the input and output Body forms. Like assign-frame, it eliminates the spills form but leaves behind the other forms. It also adds the locate form, which is not present in its input.

Input Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            
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
UFVar   ::= UVar | FVar                                 

Output Grammar:
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            ;;      mod
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

|#

(library (Compiler pre-assign-frame)
  (export pre-assign-frame)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

  (define-who pre-assign-frame
    
    (define find-used
      (lambda (conflict* home*)
        (cond
         [(null? conflict*) '()]
         [(frame-var? (car conflict*))
          (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
         [(assq (car conflict*) home*) =>
          (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
         [else (find-used (cdr conflict*) home*)])))
    
    (define find-frame-var
      (lambda (used*)
        (let f ([index 0])
          (let ([fv (index->frame-var index)])
            (if (memq fv used*) (f (+ index 1)) fv)))))
    
    (define find-homes
      (lambda (var* ct home*)
        (if (null? var*)
            home*
            (let ([var (car var*)] [var* (cdr var*)])
              (let ([conflict* (cdr (assq var ct))])
                (let ([home (find-frame-var (find-used conflict* home*))])
                  (find-homes var* ct `((,var ,home) . ,home*))))))))
    
    (define Body
      (lambda (body)
        (match body          
          [(locals (,local* ...)
             (new-frames (,frame* ...)
               (spills (,spill* ...)
                 (frame-conflict ,ct
                   (call-live (,call-live* ...)
                     ,tail)))))
           (let ([home* (find-homes spill* ct '())])
             `(locals (,local* ...)
                (new-frames (,frame* ...)
                  (locate (,home* ...)
                    (frame-conflict ,ct
                      (call-live (,call-live* ...)
                        ,tail))))))]
          [,body (error who "invalid Body ~s" body)])))
    
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,x (error who "invalid Program ~s" x)])))

  )
