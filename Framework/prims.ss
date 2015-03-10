
;; This library exposes two categories of primitives for dealing with
;; language terminals in grammars (i.e. primitive things like Int and UVar).
;;
;;  (1) predicates: isFoo function returns #t if its input is a Foo
;;  (2) checkers: the Foo function checks that the input is a valid
;;      Foo, returning #f if everything is ok, and an error string otherwise.

(library (Framework prims)
         (export
          UVar FVar Label Reg Relop Binop Disp Ind Int64 Int32 UInt6 Integer
          isUVar isFVar isLabel isReg isRelop isBinop isDisp isInd
          isInt64 isInt32 isUInt6 invalid-expr
	  ValPrim PredPrim EffectPrim
	  checkValPrim checkPredPrim checkEffectPrim
	  isImmediate Immediate
	  isDatum Datum
	  )
         (import (chezscheme)
                 (Framework match)
                 (Framework helpers))

;; Return a string representing an error message:
(define invalid-expr
  (lambda (t e)
    (format "Invalid ~a: ~a\n" t e)))

;; Check if a list of characters represents a valid positive number:
(define Index
  (lambda (ls)
    (and (not (null? ls))
         (list? ls)
         (or (null? (cdr ls)) (not (eq? (car ls) #\0))) ;; No leading zeros.
         (for-all char-numeric? ls))))

;; An immediate is used in the Scheme front-end for quoted constants.
(define isImmediate
  (lambda (x)
    (or (fixnum? x)
	(eq? x #t) (eq? x #f)
	(eq? x '()))))

;; A datum is a structured, complex constant
(define isDatum
  (lambda (x)
    (cond 
     [(isImmediate x) #t]
     [(pair? x)   (and (isDatum (car x)) (isDatum (cdr x)))]
     [(vector? x) (isDatum (vector->list x))]
     [else #f])))

;; This is very slow... TODO: operate directly on the strings:
(define isUVar
  (lambda (x)
    (and (symbol? x)
      ;; TODO: use substring and string->number:
      (let ((ls (string->list (symbol->string x))))
	(let ([suffix (memq #\. ls)])
	  (and suffix
	       (Index (cdr suffix))))))))

(define isFVar
  (lambda (x)
    (and (symbol? x)
      (let ((ls (string->list (symbol->string x))))
	(match ls
	  ((#\f #\v . ,ind) (Index ind))
	  (,e #f))))))

(define isLabel
  (lambda (x)
    (and (symbol? x)
      (let ls ((ls (string->list (symbol->string x))))
	(let suf ([suffix (memq #\$ ls)])
	  (and suffix
	       (Index (cdr suffix))))))))

(define relops '(< <= = >= >))
(define binops '(* - + logand logor sra))

(define isReg
  (lambda (x)
    (and (memq x registers) #t)))

(define isRelop
  (lambda (x)
    (and (memq x relops) #t)))

(define isBinop
  (lambda (x)
    (and (memq x binops) #t)))

;; Returns the arity if it is a value-primitive, otherwise #f.
(define checkValPrim
  (lambda (x) 
    (let ((pr (assq x value-prims)))
      (if pr (cdr pr) #f))))

;; Returns the arity if it is a predicate-primitive, otherwise #f.
(define checkPredPrim
  (lambda (x) 
    (let ((pr (assq x pred-prims)))
      (if pr (cdr pr) #f))))

;; Returns the arity if it is an effect-primitive, otherwise #f.
(define checkEffectPrim
  (lambda (x) 
    (let ((pr (assq x effect-prims)))
      (if pr (cdr pr) #f))))

(define value-prims
 '((* . 2) (+ . 2) (- . 2) (car . 1) (cdr . 1) (cons . 2)
    (make-vector . 1) (vector-length . 1) (vector-ref . 2)
    (void . 0)))
(define pred-prims
 '((< . 2) (<= . 2) (= . 2) (>= . 2) (> . 2) (boolean? . 1)
    (eq? . 2) (fixnum? . 1) (null? . 1) (pair? . 1)
    (vector? . 1)))
(define effect-prims 
  '((set-car! . 2) (set-cdr! . 2) (vector-set! . 3)))

;; It looks like Kyle wanted to switch these to sexps and didn't finish:
; (define isDisp
;   (lambda (x)
;     (match x
;       [(disp ,[isReg -> reg] ,[isInt64 -> ind])
;        (and reg ind)]
;       [,_ #f])))

(define isDisp disp-opnd?)

(define isInd index-opnd?)

(define isInt64 int64?)

(define isInt32 int32?)

(define isUInt6 uint6?)

;; Terminals -- the contract is that these functions return #f if
;; NOTHING IS WRONG (i.e. the datum passes).  Otherwise, they return
;; an error message.
(define UVar (lambda (x) (if (isUVar x) #f (invalid-expr 'UVar x))))
(define FVar (lambda (x) (if (isFVar x) #f (invalid-expr 'FVar x)))) 
(define Label (lambda (x) (if (isLabel x) #f (invalid-expr 'Label x))))
(define Reg (lambda (x) (if (isReg x) #f (invalid-expr 'Reg x))))
(define Relop (lambda (x) (if (isRelop x) #f (invalid-expr 'Relop x))))
(define Binop (lambda (x) (if (isBinop x) #f (invalid-expr 'Binop x))))
(define Disp (lambda (x) (if (isDisp x) #f (invalid-expr 'Disp x))))
(define Ind (lambda (x) (if (isInd x) #f (invalid-expr 'Ind x))))
(define Int64 (lambda (x) (if (isInt64 x) #f (invalid-expr 'Int64 x))))
(define Int32 (lambda (x) (if (isInt32 x) #f (invalid-expr 'Int32 x))))
(define UInt6 (lambda (x) (if (isUInt6 x) #f (invalid-expr 'UInt6 x))))
(define Integer (lambda (x) (if (integer? x) #f (invalid-expr 'Integer x))))

(define ValPrim    (lambda (x) (if (checkValPrim x) #f    (invalid-expr 'ValPrim x))))
(define EffectPrim (lambda (x) (if (checkEffectPrim x) #f (invalid-expr 'EffectPrim x))))
(define PredPrim   (lambda (x) (if (checkPredPrim x) #f   (invalid-expr 'PredPrim x))))

(define Immediate (lambda (x) (if (isImmediate x) #f   (invalid-expr 'Immediate x))))
(define Datum (lambda (x) (if (isDatum x) #f   (invalid-expr 'Datum x))))

)
