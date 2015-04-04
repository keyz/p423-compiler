#|

A8 - Mar 6, 2015

pass: assign-registers

Input Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locals (UVar *)                            
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
Conflict ::= Reg | UVar                                  

Output Grammar:
Program  ::= (letrec ((Label (lambda () Body)) *) Body) 
Body     ::= (locals (UVar *)                            ;;      mod
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

|#

(library (Compiler assign-registers)
  (export assign-registers)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler utils))

  (define-who assign-registers

    (define find-used
      (lambda (conflict* home*)
        (cond
         [(null? conflict*) '()]
         [(register? (car conflict*))
          (set-cons (car conflict*) (find-used (cdr conflict*) home*))]
         [(assq (car conflict*) home*) =>
          (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*) home*)))]
         [else (find-used (cdr conflict*) home*)])))

    (define select-register
      (lambda (var conflict* home*)
        (let ([used* (find-used conflict* home*)])
          (let ([available* (hdifference registers used*)])
            (and (not (null? available*)) (car available*))))))

    (define rem-conflicts!
      (lambda (ct var conflict*)
        (for-each
         (lambda (x)
           (when (uvar? x)
             (let ([a (assq x ct)])
               (set-cdr! a (remq var (cdr a))))))
         conflict*)))

    (define find-homes
      (lambda (var* ct)
        (define k (length registers))

        (define low-degree?
          (lambda (var)
            (< (length (cdr (assq var ct))) k)))

        (let f ([var* var*])
          (if (null? var*)
              '()  ; empty homes to begin with
              (let ([var (or (find low-degree? var*) (car var*))])
                (let ([conflict* (cdr (assq var ct))] [var* (remq var var*)])
                  (rem-conflicts! ct var conflict*)
                  (let ([home* (f var*)])
                    (let ([reg (select-register var conflict* home*)])
                      (if reg
                          (cons `[,var ,reg] home*)
                          home*)))))))))

    (define (Body x)
      (match x
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate (,frame-home* ...)
               (frame-conflict ,fv-ct
                 (register-conflict ,ct ,tail)))))
         ;; putting local* before ulocal* allows find-homes to choose the
         ;; first element of the list when all variables are high degree and
         ;; be guaranteed a spillable variable if one is left.  if find-homes
         ;; wants to be more clever about choosing a high-degree victim, it
         ;; will have to be told which variables are spillable.
         (let ([uvar* (append local* ulocal*)])
           (let ([home* (find-homes uvar* ct)])
             (let ([spill* (hdifference uvar* (map car home*))])
               (cond
                [(null? spill*) `(locate (,frame-home* ... ,home* ...) ,tail)]
                [(null? (intersection ulocal* spill*))
                 (let ([local* (hdifference local* spill*)])
                   `(locals (,local* ...)
                      (ulocals (,ulocal* ...)
                        (spills (,spill* ...)
                          (locate (,frame-home* ...)
                            (frame-conflict ,fv-ct ,tail))))))]
                [else
                 (error who "unspillable variables (~s) have been spilled"
                        (hdifference spill* local*))]))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)]))

    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,x (error who "invalid Program ~s" x)])))

  )
