#|

A12 - Apr 12, 2015
modified `common.ss` to support closure & first-class procedures:
- primitives += make-procedure procedure-code procedure-ref procedure-set! procedure?
- value-prim? += make-procedure procedure-code procedure-ref
- pred-prim? += procedure?
- effect-prim? += procedure-set!

In detail:
- procedure? \sim pair? or vector?
- make-procedure \sim make-vector,
    - size arg should be a constant
- procedure-code \sim vector-length,
- procedure-ref \sim vector-ref
    - index arg should be a constant
- procedure-set! \sim vector-set!.
    - index arg should be a constant


A10 - Mar 29, 2015

pass: specify-representation

The goal of this pass is to convert all Scheme datatypes to their ptr equivalents so they are represented as integers in the output language, and at the same time, translate calls to Scheme primitives into calls to UIL primitives.

The following items summarize what must be done on this pass:
- Convert quoted fixnums and other immediates into their unquoted ptr equivalents, using the values of the appropriate helpers.ss variables.
- Adjust one multiplication operand, at compile time if possible, otherwise at run time, using the helpers.ss variable shift-fixnum.
- Convert calls to cons and make-vector into calls to alloc. Take advantage of constant lengths in make-vector, where possible.
- Convert calls to car, cdr, vector-length, and vector-ref into calls to mref. Take advantage of constant indices in vector-ref, where possible.
- Convert calls to set-car!, set-cdr!, and vector-set! into calls to mset!. Take advantage of constant indices in vector-set!, where possible.
- Expand the type predicates boolean?, fixnum?, pair?, and vector? into calls to logand and =, inserting the appropriate values of the symbolic mask and tag constants.
- Convert calls to eq? into calls to =. Treat calls to null? as calls to eq? with the empty list as one argument, hence convert them as well into calls to =.

Prog      ::= (letrec ((Label (lambda (UVar *) Value)) *) Value)
Pred      ::= (let ([UVar Value] *) Pred)
            | (true)
            | (false)
            | (if Pred Pred Pred)
            | (begin Effect * Pred)
            | (PredPrim Value *)
Effect    ::= (let ([UVar Value] *) Effect)
            | (nop)
            | (if Pred Effect Effect)
            | (begin Effect * Effect)
            | (EffectPrim Value *)
            | (Value Value *)
Value     ::= (quote Immediate)
            | (let ([UVar Value] *) Value)
            | (if Pred Value Value)
            | (begin Effect * Value)
            | (ValPrim Value *)
            | (Value Value *)
            | UVar 
            | Label
Immediate ::= fixnum | () | #t | #f

fixnum: an exact integer in a machine-dependent range

valid primitive functions and their argument counts:

ValPrim:
(+ . 2)
(- . 2)
(* . 2)
(car . 1)
(cdr . 1)
(cons . 2)
(make-vector . 1)
(vector-length . 1)
(vector-ref . 2)
(void . 0)
(make-procedure . 2)
(procedure-code . 1)
(procedure-ref . 2)

PredPrim:
(< . 2)
(<= . 2)
(= . 2)
(>= . 2)
(> . 2)
(boolean? . 1)
(eq? . 2)
(fixnum? . 1)
(null? . 1)
(pair? . 1)
(vector? . 1)
(procedure? . 1)

EffectPrim:
(set-car! . 2)
(set-cdr! . 2)
(vector-set! . 3)
(procedure-set! . 3)

|#


(library (Compiler specify-representation)
  (export specify-representation)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Compiler common)
    (Compiler utils))

  (define-who specify-representation

    (define (Immediate immediate)
      (match immediate
        [#t $true]
        [#f $false]
        [() $nil]
        [,num (guard (fixnum? num)) (ash num shift-fixnum)]
        [,else (errorf who "invalid Immediate ~s" else)]))

    (define (Effect-Prim p args)
      (match `(,p . ,args)
        [(set-car! ,ls ,a)
         `(mset! ,ls ,(- disp-car tag-pair) ,a)]
        [(set-cdr! ,ls ,d)
         `(mset! ,ls ,(- disp-cdr tag-pair) ,d)]
        [(vector-set! ,vec ,i ,x)
         (if (integer? i)
             `(mset! ,vec ,(+ (- disp-vector-data tag-vector) i) ,x) ;; compile-time
             `(mset! ,vec (+ ,(- disp-vector-data tag-vector) ,i) ,x))] ;; run-time
        [(procedure-set! ,proc ,i ,val) ;; a12 new
         (if (integer? i)
             `(mset! ,proc ,(+ (- disp-procedure-data tag-procedure) i) ,val)
             (errorf who "procedure-set! ~s: index arg should be a constant" `(procedure-set! ,proc ,i ,val)))]
        [,else (errorf who "invalid EffectPrim ~s" else)]))

    (define (Effect effect)
      (match effect
        [(let ([,uvar* ,[Value -> val*]] ...) ,[ef])
         `(let ([,uvar* ,val*] ...) ,ef)]
        [(nop) '(nop)]
        [(if ,[Pred -> test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[ef*] ... ,[ef])
         (make-begin `(,ef* ... ,ef))]
        [(,ef-prim ,[Value -> v*] ...) (guard (effect-prim? ef-prim))
         (Effect-Prim ef-prim v*)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
         `(,rator ,rand* ...)]
        [,else (errorf who "invalid Effect ~s" else)]))

    (define (Pred-Prim p args)
      (match `(,p . ,args)
        [(,relop ,x ,y) (guard (memq relop '(< <= = >= >)))
         `(,relop ,x ,y)]
        [(boolean? ,b)
         `(= (logand ,b ,mask-boolean) ,tag-boolean)]
        [(eq? ,x ,y)
         `(= ,x ,y)]
        [(fixnum? ,n)
         `(= (logand ,n ,mask-fixnum) ,tag-fixnum)]
        [(null? ,ls) ;; 
         `(= ,ls ,$nil)]
        [(pair? ,p)
         `(= (logand ,p ,mask-pair) ,tag-pair)]
        [(vector? ,v)
         `(= (logand ,v ,mask-vector) ,tag-vector)]
        [(procedure? ,p) ;; a12 new
         `(= (logand ,p ,mask-procedure) ,tag-procedure)]
        [,else (errorf who "invalid PredPrim ~s" else)]))
    
    (define (Pred pred)
      (match pred
        [(let ([,uvar* ,[Value -> val*]] ...) ,[p])
         `(let ([,uvar* ,val*] ...) ,p)]
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[test] ,[then] ,[else]) `(if ,test ,then ,else)]
        [(begin ,[Effect -> ef*] ... ,[p])
         (make-begin `(,ef* ... ,p))]
        [(,pred-prim ,[Value -> v*] ...) (guard (pred-prim? pred-prim))
         (Pred-Prim pred-prim v*)]
        [,else (errorf who "invalid Pred ~s" else)]))

    (define (Value-Prim p args)
      (match `(,p . ,args)
        [(+ ,x ,y) `(+ ,x ,y)]
        [(- ,x ,y) `(- ,x ,y)]
        [(* ,x ,y) ;; quote has already been eliminated
         (cond
          [(integer? x) `(* ,(sra x shift-fixnum) ,y)] ;; run-time
          [(integer? y) `(* ,x ,(sra y shift-fixnum))] ;; run-time
          [else `(* ,x (sra ,y ,shift-fixnum))])] ;; compile-time
        [(car ,ls)
         `(mref ,ls ,(- disp-car tag-pair))]
        [(cdr ,ls)
         `(mref ,ls ,(- disp-cdr tag-pair))]
        [(cons ,a ,d)
         (let ([tmp-a (unique-name 'a)]
               [tmp-d (unique-name 'd)]
               [tmp-ls (unique-name 'ls)])
           `(let ([,tmp-a ,a]
                  [,tmp-d ,d])
              (let ([,tmp-ls (+ (alloc ,size-pair) ,tag-pair)])
                (begin
                  (mset! ,tmp-ls ,(- disp-car tag-pair) ,tmp-a)
                  (mset! ,tmp-ls ,(- disp-cdr tag-pair) ,tmp-d)
                  ,tmp-ls))))]
        [(make-vector ,len)
         (let ([tmp (unique-name 'vec)])
           (if (integer? len)
               `(let ([,tmp (+ (alloc ,(+ disp-vector-data len)) ,tag-vector)]) ;; compile-time
                  (begin
                    (mset! ,tmp ,(- disp-vector-length tag-vector) ,len) 
                    ,tmp))
               `(let ([,tmp (+ (alloc (+ ,disp-vector-data ,len)) ,tag-vector)]) ;; run-time
                  (begin
                    (mset! ,tmp ,(- disp-vector-length tag-vector) ,len)
                    ,tmp))))]
        [(make-procedure ,code ,size) ;; a12 new
         (let ([tmp (unique-name 'proc)])
           (if (integer? size)
               `(let ([,tmp (+ (alloc ,(+ disp-procedure-data size)) ,tag-procedure)])
                  (begin
                    (mset! ,tmp ,(- disp-procedure-code tag-procedure) ,code)
                    ,tmp))
               (errorf who "make-procedure ~s: size arg should be a constant" `(make-procedure ,code ,size))))]
        [(vector-length ,vec)
         `(mref ,vec ,(- disp-vector-length tag-vector))]
        [(procedure-code ,p) ;; a12 new
         `(mref ,p ,(- disp-procedure-code tag-procedure))]
        [(vector-ref ,vec ,i)
         (if (integer? i) 
             `(mref ,vec ,(+ (- disp-vector-data tag-vector) i)) ;; compile-time
             `(mref ,vec (+ ,(- disp-vector-data tag-vector) ,i)))] ;; run-time
        [(procedure-ref ,proc ,i) ;; a12 new
         (if (integer? i)
             `(mref ,proc ,(+ (- disp-procedure-data tag-procedure) i))
             (errorf who "procedure-ref ~s: index arg should be a constant" `(procedure-ref ,proc ,i)))]
        [(void) $void]
        [,else (errorf who "invalid ValuePrim ~s" else)]))

    
    (define (Value value)
      (match value
        [(quote ,[Immediate -> i]) i]
        [(let ([,uvar* ,[val*]] ...) ,[val])
         `(let ([,uvar* ,val*] ...) ,val)]
        [(if ,[Pred -> test] ,[then] ,[else])
         `(if ,test ,then ,else)]
        [(begin ,[Effect -> ef*] ... ,[v])
         (make-begin `(,ef* ... ,v))]
        [(,val-prim ,[v*] ...) (guard (value-prim? val-prim))
         (Value-Prim val-prim v*)]
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        [,x (guard (or (uvar? x) (label? x))) x]
        [,else (errorf who "invalid Value ~s" else)]))

    
    (lambda (prog)
      (match prog
        [(letrec ((,label* (lambda (,uvar* ...) ,[Value -> val*])) ...) ,[Value -> val])
         `(letrec ((,label* (lambda (,uvar* ...) ,val*)) ...) ,val)])))

  )

