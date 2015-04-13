#|
A12 - Apr 09, 2015

pass: convert-closures

This pass converts lambda expressions with free variables into lambda expressions without free variables and makes explicit the representation of each procedure as a closure that encapsulates a pointer to the procedure's code and the values of its free variables:

before:
(letrec ([name.n
          (lambda (x ...)
            (free (fx ...)
              lambda-body))]
         ...)
  letrec-body)

after:
(letrec ([name$n
          (lambda (cp x ...)
            (bind-free (cp fx ...)
              lambda-body))]
         ...)
  (closures ([name.n name$n fx ...] ...)
    letrec-body))

As a summary,
1. `cp` will be added as the first parameter to each `lambda` exp
2. replacing the variables that the input-language letrec expression binds with labels
3. introducing a closures form within each letrec expression that creates a closure for each lambda expression, implicitly storing within the closure a pointer to the code (obtained via the code's new label) and the values of its free variables; and
4. converting each procedure call to pass along the closure as an explicit additional argument.

Input:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
            | (letrec ((UVar (lambda (UVar *) (free (UVar *) Expr))) *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar 
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr]*) Expr)
            | (letrec ((Label (lambda (UVar *)
                                (bind-free (UVar *) Expr))) *)
                (closures ((UVar Label UVar *) *) Expr))       ;; new
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
            | Label                                            ;; new
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler convert-closures)
  (export convert-closures)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who convert-closures

    (define (immediate? imm)
      (or (memq imm '(#t #f ()))
          (and (integer? imm)
               (exact? imm)
               (fixnum-range? imm))))

    ;; for each uvar: generate a corresponding label
    (define (&label x)
      (values x (unique-label x)))

    ;; for each list of free vars -- i.e., (a.1 b.2 c.3):
    ;; return the list itself and a new cp
    (define (&cp x)
      (values x (unique-name 'cp)))
    
    (define (Expr expr)
      (match expr
        [(quote ,i) (guard (immediate? i))
         `(quote ,i)]
        [(let ([,uvar* ,[exp*]] ...) ,[exp])
         `(let ([,uvar* ,exp*] ...) ,exp)]
        [(letrec
             ([,[&label -> uvar* label*]
               (lambda (,arg* ...)
                 (free ,[&cp -> freev* cp*] ,[exp*]))] ...) ,[exp])
         `(letrec
              ([,label*
                (lambda (,cp* ,arg* ...)
                  (bind-free (,cp* ,freev* ...) ,exp*))] ...)
            (closures ([,uvar* ,label* ,freev* ...] ...) ,exp))]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[exp*] ... ,[exp])
         `(begin ,exp* ... ,exp)]
        [(,prim ,[exp*] ...) (guard (prim? prim))
         `(,prim ,exp* ...)]
        [(,[rator] ,[rand*] ...)
         (cond
          [(uvar? rator) `(,rator ,rator ,rand* ...)]
          [else (let ([tmp (unique-name 'tmp)])
                  `(let ([,tmp ,rator])
                     (,tmp ,tmp ,rand* ...)))])]
        [,uvar (guard (uvar? uvar)) uvar]
        [,el (errorf who "Invalid Expr ~s" el)]))

    ;; gets rid of duplicate suffixes. since `convert-closures` is the first pass to use `unique-*`, it should be responsible for setting the global counter properly.
    (define set-global-name-count!
      (lambda (ls)
        (define (flatten orig-sexp) ;; DFS
          (let loop ([sexp orig-sexp] [acc '()])
            (cond
             [(null? sexp) acc]
             [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
             [else (cons sexp acc)])))
        (define (find-max-count ls)
          (let loop ([ls (flatten ls)] [count 0])
            (cond
             [(null? ls) count]
             [(uvar? (car ls))
              (let ([n (string->number (extract-suffix (car ls)))])
                (if (> n count)
                    (loop (cdr ls) n)
                    (loop (cdr ls) count)))]
             [else (loop (cdr ls) count)])))

        (unique-name-count (find-max-count ls))))
    
    (lambda (prog)
      (set-global-name-count! prog)
      (Expr prog)))
  )
