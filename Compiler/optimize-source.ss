#|

A15 - May 3, 2015

Challenge B: optimize-source

Implemented a simplified version of:
- Constant folding
- Dead-code elimination

Input & Output: 
Prog      ::= Expr
Expr      ::= (quote Immediate)
            | (let ([UVar Expr] *) Expr)
            | (letrec ((Label (lambda (UVar *) Expr)) *) Expr)
            | (if Expr Expr Expr)
            | (begin Expr * Expr)
            | (ValPrim Expr *)
            | (EffectPrim Expr *)
            | (PredPrim Expr *)
            | (Expr Expr *)
            | UVar 
            | Label
Immediate ::= fixnum | () | #t | #f

|#

(library (Compiler optimize-source)
  (export optimize-source)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler utils)
    (Compiler common)
    (Compiler pmunit))

  (define-who optimize-source

    (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (errorf who "integer ~s is out of fixnum range" x)))))

    (define (valid-num? x)
      (and (and (integer? x) (exact? x))
           (or (fixnum-range? x)
               (errorf who "integer ~s is out of fixnum range" x))))

    (define (trivial-binding? binding)
      (match binding
        [(,lhs ,rhs)
         (cond
          [(constant? rhs) `(,lhs . ,rhs)]
          [(symbol? rhs) `(,lhs . ,rhs)]
          [else '()])]))

    (define (replace-vars exp env)
      (match exp
        [() '()]
        [,x (if (assq x env)
                (cdr (assq x env))
                x)]
        [(,[a] . ,[d]) `(,a . ,d)]))
    
    (define (datum? x)
      (or (constant? x)
          (if (pair? x)
              (and (datum? (car x)) (datum? (cdr x)))
              (and (vector? x) (andmap datum? (vector->list x))))))
    
    (define foldable-prims
      `((+ . ,valid-num?) (- . ,valid-num?) (* . ,valid-num?)
        (<= . ,valid-num?) (< . ,valid-num?) (= . ,valid-num?) (>= . ,valid-num?) (> . ,valid-num?)
        (boolean? . ,boolean?) (fixnum? . ,fixnum?) (null? . ,null?) (pair? . ,pair?) (vector? . ,vector?)))

    (define (Expr x)
      ;; an env lookup is the right way to do it. however, given limited time, this is a simplified approach to do the Constant folding and Dead-code elimination part.
      (match x
        [(quote ,i) (guard (immediate? i))
         `(quote ,i)]
        [(let ([,uvar* ,[expr*]] ...) ,[expr])
         `(let ([,uvar* ,expr*] ...) ,expr)]
        [(letrec ([,label* (lambda (,uvar* ...) ,[expr*])] ...) ,[expr])
         `(letrec ([,label* (lambda (,uvar* ...) ,expr*)] ...) ,expr)]
        [(if ,[test] ,[then] ,[else])
         (cond
          [(equal? test ''#t) then]
          [(equal? test ''#f) else]
          [else `(if ,test ,then ,else)])]
        [(begin ,[expr*] ... ,[expr])
         `(begin ,expr* ... ,expr)]
        [(,prim ,[expr*] ...) (guard (prim? prim))
         (let ([check (assq prim foldable-prims)])
           (if check
               (if (andmap (lambda (x)
                             (if (pair? x) ;; safety check
                                 ((cdr check) (cadr x)) ;; cuz everything is quoted here
                                 #f))
                           expr*)
                   `(quote ,(eval `(,prim ,expr* ...)))
                   `(,prim ,expr* ...))
               `(,prim ,expr* ...)))]        
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,label (guard (label? label)) label]
        [,el (errorf who "Invalid Expr ~s" el)]))

    (lambda (prog)
      (Expr prog))

    ;; (lambda (prog) prog)

    ;; (define (Expr env)
    ;;   ;; env contains values for the copy-propagated values
    ;;   ;; returns 3 values: the new expression, a list of the variables referenced in the new expression, and a flag indicating whether the new expression is useless if its value isn't needed
    ;;   (lambda (x)
    ;;     (match x
    ;;       [(quote ,i) (guard (immediate? i))
    ;;        (values `(quote ,i) '() #t)]
    ;;       [(let ([,uvar* ,expr*] ...) ,expr)
    ;;        (let* ([this-env (map trivial-binding? `([,uvar* ,expr*] ...))]
    ;;               [new-env (append this-env env)]
    ;;               ))]
    ;;       [(letrec ([,label* (lambda (,uvar* ...) ,[expr*])] ...) ,[expr])
    ;;        ]
    ;;       [(if ,[test] ,[then] ,[else])
    ;;        ]
    ;;       [(begin ,[expr*] ... ,[expr])
    ;;        ]
    ;;       [(,prim ,[expr*] ...) (guard (prim? prim))
    ;;        (let ([check (assq prim foldable-prims)])
    ;;          (if check
    ;;              (if (andmap (cdr check) expr*) ;; cool, fold it
    ;;                  (values (eval `(,prim ,expr* ...))
    ;;                          '()
    ;;                          #t)
    ;;                  (values `(,prim ,expr* ...)
    ;;                          (filter symbol? expr*)
    ;;                          #t))
    ;;              (values `(,prim ,expr* ...)
    ;;                      (filter symbol? expr*)
    ;;                      #f))) ;; side effects
    ;;        ]
    ;;       [(,[rator] ,[rand*] ...)
    ;;        ]
    ;;       [,uvar (guard (uvar? uvar))
    ;;              ]
    ;;       [,label (guard (label? label))
    ;;               ]
    ;;       [,el (errorf who "Invalid Expr ~s" el)])))


    )
  )
