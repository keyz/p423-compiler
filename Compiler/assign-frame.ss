#|

A8 - Mar 6, 2015

pass: assign-frame

Input Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locals (UVar *)                            
               (ulocals (UVar *)
                 (spills (UVar *)
                   (locate ((UVar FVar) *) 
                     (frame-conflict ((UVar Var *) *) 
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


Output Grammar (same as `assign-new-frame`):
Program ::= (letrec ((Label (lambda () Body)) *) Body) 
Body    ::= (locals (UVar *)                            ;;      mod
              (ulocals (UVar *)
                (locate ((UVar FVar) *) 
                  (frame-conflict ((UVar Var *) *)
                    Tail))))
          | (locate ((UVar Loc) *) Tail)
Tail    ::= (if Pred Tail Tail)
          | (begin Effect * Tail)
          | (Triv Var *)  ;; Note 'Var' not 'Loc'.      ;;      mod
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


(library (Compiler assign-frame)
  (export assign-frame)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who assign-frame
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
             (ulocals (,ulocal* ...)
               (spills (,spill* ...)
                 (locate (,home* ...)
                   (frame-conflict ,ct ,tail)))))
           (let ([home* (find-homes spill* ct home*)])
             `(locals (,local* ...)
                (ulocals (,ulocal* ...)
                  (locate (,home* ...)
                    (frame-conflict ,ct ,tail)))))]
          [(locate (,home* ...) ,body) `(locate (,home* ...) ,body)]
          [,body (error who "invalid Body ~s" body)])))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,x (error who "invalid Program ~s" x)])))

  )
