#|

A14 - Apr 18, 2014

pass: convert-complex-datum

This pass converts quoted pairs and vectors into code that builds them using cons and make-vector.

e.g.,
'(1 (2 (3 4 5))) ->
(cons '1 (cons (cons '2 (cons (cons '3 (cons '4 (cons '5 '()))) '())) '()))

'#(32 (33 33) 34) ->
(let ([tmp.1 (make-vector '3)])
  (begin
    (vector-set! tmp.1 '0 '32)
    (vector-set! tmp.1 '1 (cons '33 (cons '33 '())))
    (vector-set! tmp.1 '2 '34)
    tmp.1))

Moreover, it binds a new variable to the result of building the structure and reference the new variable in place of the quoted constant, such that the pair is created only once as desired.

e.g.,
(let ([f.1 (lambda () '(1 . 2))])
  (eq? (f.1) (f.1))) -> 
(let ([tmp.2 (cons '1 '2)])
  (let ([f.1 (lambda () tmp.2)])
    (eq? (f.1) (f.1))))

Input:
Prog      ::= Expr
Expr      ::= (quote Datum)
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Expr] *) Body)
            | (lambda (UVar *) Body)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Body      ::= Expr
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

Output:
Prog      ::= Expr
Expr      ::= (quote Immediate)                  ;; mod
            | (let ([UVar Expr] *) Body)
            | (letrec ([UVar Expr] *) Body)
            | (lambda (UVar *) Body)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (set! UVar Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar
Body      ::= Expr
Datum     ::= Immediate | (Datum *) | #(Datum *)
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler convert-complex-datum)
  (export convert-complex-datum)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common))

  (define-who convert-complex-datum

    (define lets '())
    
    (define (gen-vector target max ls)
      (let loop ([n 0] [ls ls] [acc '()])
        (cond
         [(= n max) (reverse (cons target acc))]
         [else (loop (add1 n)
                     (cdr ls)
                     (cons `(vector-set! ,target (quote ,n) ,(car ls)) acc))])))
    
    (define (Datum d)
      (match d
        [,i (guard (immediate? i)) `(quote ,i)]
        [(,[a] . ,[d]) `(cons ,a ,d)]
        [#(,[v*] ...)
         (let ([tmp (unique-name 'tmp)]
               [len (length v*)]
               [ls `(,v* ...)])
           `(let ([,tmp (make-vector (quote ,len))])
              (begin ,@(gen-vector tmp len ls))))]))

    (define (Expr expr)
      (match expr
        [(quote ,d) (guard (immediate? d))
         `(quote ,d)]
        [(quote ,d) ;; (guard (or (pair? d) (vector? d)))
         (let ([tmp (unique-name 'tmp)])
           (set! lets (cons `[,tmp ,(Datum d)] lets))
           tmp)]
        [(let ([,uvar* ,[exp*]] ...) ,[exp])
         `(let ([,uvar* ,exp*] ...) ,exp)]
        [(letrec ([,uvar* ,[exp*]] ...) ,[exp])
         `(letrec ([,uvar* ,exp*] ...) ,exp)]
        [(lambda (,uvar* ...) ,[body])
         `(lambda (,uvar* ...) ,body)]
        [(if ,[test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[exp*] ... ,[exp])
         `(begin ,exp* ... ,exp)]
        [(set! ,uvar ,[exp])
         `(set! ,uvar ,exp)]
        [(,prim ,[exp*] ...) (guard (prim? prim))
         `(,prim ,exp* ...)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,el (errorf who "Invalid Expr ~s" el)]))
    
    (lambda (prog)
      (set! lets '())
      (let ([result (Expr prog)])
        `(let ,lets ,result))))
  )
