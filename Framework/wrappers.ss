(library (Framework wrappers aux)
  (export
    ptr->datum
    env
    alloc
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    (rename (p423-letrec letrec))
    locals
    locate
    ulocals
    spills
    register-conflict
    true
    false
    nop
    frame-conflict
    compute-frame-size
    call-live
    return-point-complex
    return-point-simple
    new-frames
    (rename (p423-* *))
    (rename (p423-+ +))
    (rename (p423-- -))
    free
    bind-free
    fill-closure!
    closures
    cookie
    procedure 
    procedure-code procedure?
    make-procedure
    procedure-set!
    procedure-ref)
  (import
    (except (chezscheme) set! procedure?)
    (Framework match)
    (Framework helpers))

(define env
  (environment
    '(chezscheme)
    '(Framework helpers)
    '(Framework helpers frame-variables)))

(define-syntax wrap
  (syntax-rules ()
    ((_ (define-syntax name body))
     (define name `(define-syntax name body)))
    ((_ (define name body))
     (define name `(define name body)))))

(wrap
  (define alloc
    (lambda (nbytes)
      (unless (fxzero? (fxremainder nbytes word-size))
        (error 'alloc "~s is not a multiple of word size" nbytes))
      (let ([addr ,allocation-pointer-register])
        (set! ,allocation-pointer-register (+ addr nbytes))
        (check-heap-overflow ,allocation-pointer-register)
        addr))))

(define int64-in-range?
  (let ()
    (import scheme)
    (lambda (x)
      (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))

(define handle-overflow
  (let ()
    (import scheme)
    (lambda (x)
      (cond
        [(not (number? x)) x]
        [(int64-in-range? x) x]
        [(not (= x (logand 18446744073709551615 x)))
         (handle-overflow (logand 18446744073709551615 x))]
        [(< x 0) (handle-overflow (+ x (expt 2 64)))]
        [else (handle-overflow (- x (expt 2 64)))]))))

(define rewrite-opnds
  (lambda (x)
    (match x
      ;; Begin Haskell hack for disp/index-opnd read/show invariance
      [(disp ,r ,o)
       `(mref ,r ,o)]
      [(index ,r1 ,r2)
       `(mref ,r1 ,r2)]
      [(set! (disp ,r ,o) ,[expr])
       `(mset! ,r ,o ,expr)]
      [(set! (index ,r1 ,r2) ,[expr])
       `(mset! ,r1 ,r2 ,expr)]
      ;; End hack
      [,r (guard (disp-opnd? r))
        `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
        `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr] ...) expr]
      [,x x])))

(define compute-frame-size
  (lambda (x)
    (match x
      [(,[fs*] ...) (apply max 0 fs*)]
      [,x (if (frame-var? x) (+ (frame-var->index x) 1) 0)])))

(wrap
  (define-syntax set!
    (let ()
      (import scheme)
      (syntax-rules (,frame-pointer-register)
        [(_ ,frame-pointer-register (op xxx n))
         (begin
           (fp-offset (op (fp-offset) n))
           (set! ,frame-pointer-register (op xxx n)))]
        [(_ x expr)
         (set! x (handle-overflow expr))]))))

(define-syntax code
  (lambda (x)
    (define build
      (lambda (body)
        (syntax-case body ()
          [() #'(())]
          [(label expr ...)
           (identifier? #'label)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'(((bounce label))
                (define label
                  (lambda ()
                    (bounce (lambda () expr ...))))
                defn ...))]
          [(expr1 expr ...)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'((expr1 expr ...) defn ...))])))
    (syntax-case x ()
      [(k expr ...)
       (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
         #'((call/cc
              (lambda (bounce)
                defn ...
                expr ...))))])))

(define-syntax jump
  (syntax-rules ()
    [(_ target) (target)]))

(define-syntax locals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax ulocals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax frame-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax register-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax locate
  (let ()
    (import scheme)
    (syntax-rules ()
      [(_ ([x* loc*] ...) body)
       (let-syntax ([x* (identifier-syntax 
                          (id loc*) 
                          ((set! id e) 
                           (set! loc* (handle-overflow e))))] ...)
         body)])))

(define-syntax p423-letrec
  (let ()
    (import scheme)
    (syntax-rules (lambda)
      [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
       (letrec ([lab (lambda ignore (parameterize ([fp-offset 0]) lambda-body))] ...)
         (parameterize ([fp-offset 0]) letrec-body))])))

(define return-point-complex
  `(define-syntax return-point
     (lambda (x)
       (import scheme)
       (syntax-case x ()
         [(id rplab expr)
          #'(let ([top (fxsll frame-size word-shift)]
                  [rplab (lambda args (void))])
              (parameterize ([fp-offset (+ (fp-offset) top)])
                (set! ,frame-pointer-register
                  (+ ,frame-pointer-register top))
                expr
                (set! ,frame-pointer-register
                  (- ,frame-pointer-register top))))]))))

(define return-point-simple
  `(define-syntax return-point
     (syntax-rules ()
       ((_ lab expr)
        (let ([lab (lambda args (void))]) expr)))))

(wrap
  (define-syntax new-frames
    (lambda (x)
      (import scheme)
      (syntax-case x (return-point)
        [(id ((nfv ...) ...) expr)
         (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
           #'(let ([top (fxsll frame-size word-shift)])
               (define-syntax nfv
                 (identifier-syntax
                   [id (mref (- ,frame-pointer-register (fp-offset))
                         (fxsll (+ i frame-size) word-shift))]
                   [(set! id e) 
                    (mset! (- ,frame-pointer-register (fp-offset))
                      (fxsll (+ i frame-size) word-shift)
                      e)]))
               ...
               ...
               expr))]))))

(define-syntax call-live
  (syntax-rules ()
    [(_ (x* ...) body) body]))

(define-who p423-*
  (lambda (x y)
    (import scheme)
    (let ([ans (* x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-who p423-+
  (lambda (x y)
    (import scheme)
    (let ([ans (+ x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-who p423--
  (lambda (x y)
    (import scheme)
    (let ([ans (- x y)])
      (unless (fixnum-range? ans)
        (errorf who "result ~s is outside of fixnum range" ans))
      ans)))

(define-syntax free
    (syntax-rules ()
      [(_ (var ...) expr) expr]))


(define-syntax bind-free
  (lambda (x)
    (syntax-case x ()
      [(_ (cp fv ...) body)
       (with-syntax ([(i ...) (enumerate #'(fv ...))])
         #'(let ()
             (define-syntax fv
               (identifier-syntax
                 (vector-ref (cp cookie) i)))
             ...
             body))])))

(define fill-closure!
  (lambda (cp . free)
    (let ([env (cp cookie)])
      (for-each
        (lambda (i x) (vector-set! env i x))
        (enumerate free)
        free))))

(define-syntax closures
  (syntax-rules ()
    [(_ ([name code free ...] ...) body)
     (letrec ([name (let ([env (make-vector (length '(free ...)))])
                           (lambda args
                             (if (and (= (length args) 1)
                                      (eq? (car args) cookie))
                                 env
                                 (apply code args))))]
                   ...)
       (fill-closure! name free ...)
       ...
       body)]))

(define-record procedure ((immutable code) (immutable env)) ()
    ([constructor $make-procedure]))

(define make-procedure
    (lambda (code i)
      ($make-procedure code (make-vector i))))

(define procedure-ref
    (lambda (cp i)
      (vector-ref (procedure-env cp) i)))

(define procedure-set!
    (lambda (cp i v)
      (vector-set! (procedure-env cp) i v)))

(define (true) #t)

(define (false) #f)

(define (nop) (void))

(define cookie (cons "snicker" "doodle"))

(define ptr->datum
  (lambda (ptr)
    (define istype?
      (lambda (mask tag x)
        (= (logand x mask) tag)))
    (define tagref
      (lambda (tag disp p)
        (mref p (- disp tag))))
    (let f ([ptr ptr])
      (cond
        [(eqv? ptr $false) #f]
        [(eqv? ptr $true) #t]
        [(eqv? ptr $nil) '()]
        [(eqv? ptr $void) (void)]
        [(istype? mask-fixnum tag-fixnum ptr)
         (ash ptr (- shift-fixnum))]
        [(istype? mask-pair tag-pair ptr)
         (cons (f (tagref tag-pair disp-car ptr))
           (f (tagref tag-pair disp-cdr ptr)))]
        [(istype? mask-vector tag-vector ptr)
         (let ([n (f (tagref tag-vector disp-vector-length ptr))])
           (let ([v (make-vector n)])
             (do ([i 0 (+ i 1)])
                 ((= i n) v)
               (vector-set! v i
                 (f (tagref tag-vector
                      (+ disp-vector-data (fxsll i word-shift))
                      ptr))))))]
        [else (errorf 'ptr->datum "can't handle ~s" ptr)]))))

)

(library (Framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    verify-scheme/wrapper
    uncover-free/wrapper
    convert-closures/wrapper
    introduce-procedure-primitives/wrapper
    lift-letrec/wrapper
    normalize-context/wrapper
    optimize-jumps/wrapper
    specify-representation/wrapper
    uncover-locals/wrapper
    remove-let/wrapper
    verify-uil/wrapper
    remove-complex-opera*/wrapper
    flatten-set!/wrapper
    impose-calling-conventions/wrapper
    expose-allocation-pointer/wrapper
    uncover-frame-conflict/wrapper
    pre-assign-frame/wrapper
    assign-new-frame/wrapper
    finalize-frame-locations/wrapper
    select-instructions/wrapper
    uncover-register-conflict/wrapper
    assign-registers/wrapper
    assign-frame/wrapper
    discard-call-live/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-memory-operands/wrapper
    expose-basic-blocks/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper)
  (import
    (except (chezscheme) set!)
    (Framework match)
    (Framework GenGrammars l22-verify-uil)
    (Framework GenGrammars l23-remove-complex-opera)
    (Framework GenGrammars l24-flatten-set)
    (Framework GenGrammars l25-impose-calling-conventions)
    (Framework GenGrammars l26-expose-allocation-pointer)
    (Framework GenGrammars l27-uncover-frame-conflict)
    (Framework GenGrammars l28-pre-assign-frame)
    (Framework GenGrammars l29-assign-new-frame)
    (Framework GenGrammars l30-finalize-frame-locations)
    (Framework GenGrammars l32-uncover-register-conflict)
    (Framework GenGrammars l33-assign-registers)
    (Framework GenGrammars l35-discard-call-live)
    (Framework GenGrammars l36-finalize-locations)
    (Framework GenGrammars l37-expose-frame-var)
    (Framework GenGrammars l38-expose-memory-operands)
    (Framework GenGrammars l39-expose-basic-blocks)
    (Framework GenGrammars l41-flatten-program)
    (Framework helpers)
    (Framework driver)
    (only (Framework wrappers aux)
      env rewrite-opnds compute-frame-size
      return-point-complex return-point-simple
      new-frames set! alloc))

(define pass->wrapper
  (lambda (pass)
    ;; RRN: Seems better to replace this with string-append + string<->symbol 
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((uncover-free) uncover-free/wrapper)
      ((convert-closures) convert-closures/wrapper)
      ((introduce-procedure-primitives) introduce-procedure-primitives/wrapper)
      ((lift-letrec) lift-letrec/wrapper)
      ((normalize-context) normalize-context/wrapper)
      ((specify-representation) specify-representation/wrapper)
      ((uncover-locals) uncover-locals/wrapper)
      ((remove-let) remove-let/wrapper)
      ((verify-uil) verify-uil/wrapper)
      ((remove-complex-opera*) remove-complex-opera*/wrapper)
      ((flatten-set!) flatten-set!/wrapper)
      ((impose-calling-conventions) impose-calling-conventions/wrapper)
      ((expose-allocation-pointer) expose-allocation-pointer/wrapper)
      ((uncover-frame-conflict) uncover-frame-conflict/wrapper)
      ((pre-assign-frame) pre-assign-frame/wrapper)
      ((assign-new-frame) assign-new-frame/wrapper)
      ((finalize-frame-locations) finalize-frame-locations/wrapper)
      ((select-instructions) select-instructions/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((assign-frame) assign-frame/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-memory-operands) expose-memory-operands/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((optimize-jumps) optimize-jumps/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

;;-----------------------------------
;; source/wrapper
;; verify-scheme/wrapper
;;-----------------------------------
(define-language-wrapper
  (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux) * + -)
    (except (chezscheme) * + -))
  (reset-machine-state!)
  ,x)


;;-----------------------------------
;; uncover-free/wrapper/wrapper
;;-----------------------------------
(define-language-wrapper uncover-free/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux) * + - free)
    (except (chezscheme) * + -))  
  ,x)

;;-----------------------------------
;; convert-closures/wrapper
;;-----------------------------------
(define-language-wrapper convert-closures/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      * + - cookie bind-free fill-closure! closures)
    (except (chezscheme) * + -))
  ,x)

;;----------------------------------------
;; introduce-procedure-primitives/wrapper
;; lift-letrec/wrapper
;;----------------------------------------
(define-language-wrapper
  (introduce-procedure-primitives/wrapper lift-letrec/wrapper)
  (x) 
  (environment env)
  (import
    (only (Framework wrappers aux)
      * + - procedure make-procedure procedure-ref procedure-set! 
            procedure-code procedure?)
    (except (chezscheme) * + - procedure?))
  ,x)

;;-----------------------------------
;; normalize-context/wrapper
;;-----------------------------------
(define-language-wrapper
  normalize-context/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      true false nop * + -
      procedure make-procedure procedure-ref procedure-set!
      procedure-code procedure?)
    (except (chezscheme) * + - procedure?))
  (reset-machine-state!)
  ,x)

;;-----------------------------------
;; specify-representation/wrapper
;;-----------------------------------
(define-language-wrapper
  specify-representation/wrapper
  (x)
  (environment env)
  ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow true false nop ptr->datum))
  (ptr->datum ,x))

;;-----------------------------------
;; uncover-locals/wrapper
;;-----------------------------------
(define-language-wrapper
  uncover-locals/wrapper
  (x)
  (environment env)
  ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow locals true false nop ptr->datum)
    (except (chezscheme) set!))
  (ptr->datum ,x))

;;-----------------------------------
;; verify-uil/wrapper
;; remove-let/wrapper
;; remove-complex-opera*/wrapper
;; flatten-set!/wrapper
;;-----------------------------------
(define-language-wrapper
  (verify-uil/wrapper remove-let/wrapper
   remove-complex-opera*/wrapper flatten-set!/wrapper)
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow locals true false nop ptr->datum)
    (except (chezscheme) set! lambda))
  (ptr->datum ,x))

;; TODO: Duplicate grammar for remove-complex-opera*/wrapper grammar check.
;; TODO: Duplicate grammar for flatten-set!/wrapper

;;-----------------------------------
;; impose-calling-conventions/wrapper
;;-----------------------------------
(define-language-wrapper impose-calling-conventions/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,alloc
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l25-impose-calling-conventions x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; expose-allocation-pointer/wrapper
;;-----------------------------------
(define-language-wrapper expose-allocation-pointer/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l26-expose-allocation-pointer  x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; uncover-frame-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-frame-conflict/wrapper
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals spills call-live
      frame-conflict true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l27-uncover-frame-conflict x) x)))
  (ptr->datum ,return-value-register))

;;----------------------------------
;; pre-assign-frame
;;----------------------------------
(define-language-wrapper pre-assign-frame/wrapper (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals locate call-live 
      frame-conflict true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l28-pre-assign-frame x) x)))
  (ptr->datum ,return-value-register))

;;----------------------------------
;; assign-new-frame
;;----------------------------------
(define-language-wrapper assign-new-frame/wrapper (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals ulocals spills locate
      frame-conflict true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(if (grammar-verification) (verify-grammar:l29-assign-new-frame x) x)))
  (ptr->datum ,return-value-register))


;;-----------------------------------
;; finalize-frame-locations/wrapper
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
(define-language-wrapper
  (finalize-frame-locations/wrapper
   select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate
      locals ulocals frame-conflict
      true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
	     ,(if (grammar-verification) (verify-grammar:l30-finalize-frame-locations x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  ,set!
  ,return-point-simple
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate locals ulocals frame-conflict
      register-conflict true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l32-uncover-register-conflict x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate locals ulocals spills
      frame-conflict true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l33-assign-registers x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; discard-call-live/wrapper
;;-----------------------------------
(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l35-discard-call-live x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; finalize-locations/wrapper
;;-----------------------------------
(define-language-wrapper finalize-locations/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec true false nop ptr->datum)
    (except (chezscheme) set! letrec))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l36-finalize-locations x) x)))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; expose-frame-var/wrapper
;; expose-memory-operands/wrapper
;;-----------------------------------
(define-language-wrapper
  (expose-frame-var/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow true false nop ptr->datum)
    (except (chezscheme) set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l37-expose-frame-var x) x))))
  (ptr->datum ,return-value-register))

;; DUPLICATED from previous, except with different grammar verification:
(define-language-wrapper
  (expose-memory-operands/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow true false nop ptr->datum)
    (except (chezscheme) set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l38-expose-memory-operands x) x))))
  (ptr->datum ,return-value-register))


;;-----------------------------------
;; expose-basic-blocks/wrapper
;;-----------------------------------
(define-language-wrapper
  (expose-basic-blocks/wrapper optimize-jumps/wrapper)
  (x)
  (environment env)
  ,set!
  (import
    (only (Framework wrappers aux) handle-overflow ptr->datum)
    (except (chezscheme) set!))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l39-expose-basic-blocks x) x))))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; flatten-program/wrapper
;;-----------------------------------
(define-language-wrapper flatten-program/wrapper (x)
  (environment env)
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow code jump ptr->datum)
    (except (chezscheme) set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l41-flatten-program x) x))))
  (ptr->datum ,return-value-register))

;;-----------------------------------
;; generate-x86/wrapper
;;-----------------------------------
(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (get-line in)))

)
