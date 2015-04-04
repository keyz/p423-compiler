;;; Official Verify Scheme

(library (Compiler verify-scheme)
  (export verify-scheme)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers)
    (Framework prims))

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar for verify-scheme (assignment 10):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Value>)]*) <Value>)
;;;  Value   --> <label>
;;;           |  <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;           |  (let ([<uvar> <Value>]*) <Value>)
;;;           |  (<value-prim> <Value>*)
;;;           |  (<Value> <Value>*)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;           |  (let ([<uvar> <Value>]*) <Pred>)
;;;           |  (<pred-prim> <Value>*)
;;;  Effect  --> (nop)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;           |  (let ([<uvar> <Value>]*) <Effect>)
;;;           |  (<effect-prim> <Value>*)
;;;           |  (<Value> <Value>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       label is symbol$n, n >= 0
;;;       fixnum is an exact integer
;;;       value-prims are void (zero arguments); car, cdr, vector-length,
;;;         make-vector (one argument); *, +, -, cons, vector-ref
;;;         (two arguments)
;;;       pred-prims are boolean?, fixnum?, null?, pair?, vector?
;;;         (one argument); <, <=, =, >=, >, eq? (two arguments)
;;;       effect-prims are set-car!, set-cdr! (two arguments);
;;;         vector-set! (three arguments)
;;;
;;; Each label bound by the letrec expression must have a unique suffix,
;;; and each uvar bound by a lambda or let expression must have a unique
;;; suffix, within the same Program.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

(define-who verify-scheme
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Program
    (lambda (x)
      (define all-uvar* '())
      (define Body
        (lambda (label* fml*)
          (define (Immediate imm)
            (cond
              [(memq imm '(#t #f ())) (values)]
              [(and (integer? imm) (exact? imm))
               (unless (fixnum-range? imm)
                 (error who "integer ~s is out of fixnum range" imm))
               (values)]
              [else (error who "invalid Immediate ~s" imm)]))
          (define Value
            (lambda (uvar*)
              (lambda (val)
                (match val
                  [,label (guard (label? label))
                   (if (memq label label*)
                       (values)
                       (error who "unbound label ~s" label))]
                  [,uvar (guard (uvar? uvar))
                   (if (memq uvar uvar*)
                       (values)
                       (error who "unbound uvar ~s" uvar))]
                  [(quote ,[Immediate ->]) (values)]
                  [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                  [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[]] ...) ,val)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Value (append new-uvar* uvar*)) val)]
                  [(,prim ,x* ...)
                   (guard (checkValPrim prim))
                   (unless (= (length x*) (checkValPrim prim))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [(,x ,y ...)
                   (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                   (error who "invalid Value ~s" `(,x ,y ...))]
                  [(,[] ,[] ...) (values)]
                  [,val (error who "invalid Value ~s" val)]))))
          (define Effect
            (lambda (uvar*)
              (lambda (ef)
                (match ef
                  [(nop) (values)]
                  [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                  [(begin ,[] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,ef)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Effect (append new-uvar* uvar*)) ef)]
                  [(,prim ,x* ...)
                   (guard (checkEffectPrim prim))
                   (unless (= (length x*) (checkEffectPrim prim))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [(,x ,y ...)
                   (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                   (error who "invalid Effect ~s" `(,x ,y ...))]
                  [(,[(Value uvar*) ->] ,[(Value uvar*) ->] ...) (values)]
                  [,ef (error who "invalid Effect ~s" ef)]))))
          (define Pred
            (lambda (uvar*)
              (lambda (pr)
                (match pr
                  [(true) (values)]
                  [(false) (values)]
                  [(if ,[] ,[] ,[]) (values)]
                  [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                  [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,pr)
                   (set! all-uvar* (append new-uvar* all-uvar*))
                   ((Pred (append new-uvar* uvar*)) pr)]
                  [(,prim ,x* ...)
                   (guard (checkPredPrim prim))
                   (unless (= (length x*) (checkPredPrim prim))
                     (error who "too many or few arguments ~s for ~s" (length x*) prim))
                   (for-each (Value uvar*) x*)
                   (values)]
                  [,pr (error who "invalid Pred ~s" pr)]))))
            (lambda (x) ((Value fml*) x))))
      (define Lambda
        (lambda (label*)
          (lambda (x)
            (match x
              [(lambda (,fml* ...) ,[(Body label* fml*) ->])
               (set! all-uvar* (append fml* all-uvar*))
               (values)]
              [,x (error who "invalid Lambda ~a" x)]))))
      (match x
        [(letrec ([,label* ,[(Lambda label*) ->]] ...) ,[(Body label* '()) ->])
         (verify-x-list label* label? 'label)
         (verify-x-list all-uvar* uvar? 'uvar)]
        [,x (error who "invalid Program ~s" x)])))
  (lambda (x) (Program x) x))

)

