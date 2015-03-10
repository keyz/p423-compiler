(library (Framework wrappers aux)
  (export
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    locals
    ulocals
    spills
    (rename (lambda-p423 lambda))
    register-conflict
    locate
    true
    false
    nop
    frame-conflict)
  (import
    (except (chezscheme) set!)
    (Framework match)
    (Framework helpers))

(define int64-in-range?
  (lambda (x)
    (<= (- (expt 2 63)) x (- (expt 2 63) 1))))

(define handle-overflow
  (lambda (x)
    (cond
      [(not (number? x)) x]
      [(int64-in-range? x) x]
      [(not (= x (logand 18446744073709551615 x)))
       (handle-overflow (logand 18446744073709551615 x))]
      [(< x 0) (handle-overflow (+ x (expt 2 64)))]
      [else (handle-overflow (- x (expt 2 64)))])))

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

(define-syntax set!
  (let ()
    (import (chezscheme))
    (syntax-rules ()
      [(_ x expr)
       (set! x (handle-overflow expr))])))

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

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax ulocals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax lambda-p423
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))

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

(define (true) #t)

(define (false) #f)

(define (nop) (void))

)

(library (Framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    verify-scheme/wrapper
    uncover-frame-conflict/wrapper
    introduce-allocation-forms/wrapper
    select-instructions/wrapper
    uncover-register-conflict/wrapper
    assign-registers/wrapper
    assign-frame/wrapper
    finalize-frame-locations/wrapper
    discard-call-live/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-basic-blocks/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper)
  (import
    (chezscheme)
    (Framework match)
    (Framework GenGrammars l01-verify-scheme)
    (Framework GenGrammars l28-uncover-frame-conflict)
    (Framework GenGrammars l29-introduce-allocation-forms)
    (Framework GenGrammars l31-select-instructions)
    (Framework GenGrammars l32-uncover-register-conflict)
    (Framework GenGrammars l33-assign-registers)
    (Framework GenGrammars l35-discard-call-live)
    (Framework GenGrammars l36-finalize-locations)
    (Framework GenGrammars l37-expose-frame-var)
    (Framework GenGrammars l39-expose-basic-blocks)
    (Framework GenGrammars l41-flatten-program)
    (Framework helpers)
    (Framework driver)
    (only (Framework wrappers aux) rewrite-opnds))

(define env
  (environment
    '(except (chezscheme) set!)
    '(Framework helpers)
    '(Framework helpers frame-variables)))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((uncover-frame-conflict) uncover-frame-conflict/wrapper)
      ((introduce-allocation-forms) introduce-allocation-forms/wrapper)
      ((select-instructions) select-instructions/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((assign-frame) assign-frame/wrapper)
      ((finalize-frame-locations) finalize-frame-locations/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

(define-language-wrapper (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import (only (Framework wrappers aux)
            set! handle-overflow  locals
            lambda true false nop))
  (reset-machine-state!)
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l01-verify-scheme x) x)))
  ,return-value-register)

;;-----------------------------------
;; uncover-frame-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-frame-conflict/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      set! handle-overflow locals lambda true false nop frame-conflict))
  (call/cc (lambda (k) (set! ,return-address-register k) 
       ,(if (grammar-verification) (verify-grammar:l28-uncover-frame-conflict x) x)))
  ,return-value-register)

;;-----------------------------------
;; introduce-allocation-forms/wrapper
;; finalize-frame-locations/wrapper
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
(define-language-wrapper
  (introduce-allocation-forms/wrapper
   finalize-frame-locations/wrapper
   select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      locals ulocals locate set! handle-overflow
      lambda true false nop frame-conflict))
  (call/cc (lambda (k) (set! ,return-address-register k)
             ;;we call l31 here because this is in an iterate form and the grammar doesn't change.
       ,(if (grammar-verification) (verify-grammar:l31-select-instructions x) x)))
  ,return-value-register)

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! locate locals ulocals
      lambda register-conflict frame-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l32-uncover-register-conflict x) x)))
  ,return-value-register)

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! locate locals ulocals
      spills frame-conflict lambda true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l33-assign-registers x) x)))
  ,return-value-register)

(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  (import (only (Framework wrappers aux)
             handle-overflow set! locate
            true false nop)
    (only (chezscheme) lambda))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l35-discard-call-live x) x)))
  ,return-value-register)

(define-language-wrapper finalize-locations/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l36-finalize-locations x) x)))
  ,return-value-register)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l37-expose-frame-var x) x))))
  ,return-value-register)

(define-language-wrapper expose-basic-blocks/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l39-expose-basic-blocks x) x))))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  (import
    (only (Framework wrappers aux)
      handle-overflow set! code jump))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l41-flatten-program x) x))))
  ,return-value-register)

(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)
