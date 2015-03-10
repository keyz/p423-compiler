(library (Framework wrappers aux)
  (export
    env
    alloc
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    (rename (p423-letrec letrec))
    locals
    ulocals
    spills
    register-conflict
    locate
    true
    false
    nop    
    frame-conflict
    compute-frame-size
    call-live
    return-point-complex
    return-point-simple
    new-frames)
  (import
    (except (chezscheme) set! letrec)
    (Framework match)
    (Framework helpers))

(define env
  (environment
    '(except (chezscheme) set! letrec)
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

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax ulocals
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

(define (true) #t)

(define (false) #f)

(define (nop) (void))

)

(library (Framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    verify-uil/wrapper
    remove-complex-opera*/wrapper
    flatten-set!/wrapper
    impose-calling-conventions/wrapper
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
    (Framework GenGrammars l27-uncover-frame-conflict)
    (Framework GenGrammars l28-pre-assign-frame)
    (Framework GenGrammars l29-assign-new-frame)
    (Framework GenGrammars l30-finalize-frame-locations)
    (Framework GenGrammars l32-uncover-register-conflict)
    (Framework GenGrammars l33-assign-registers)
    (Framework GenGrammars l35-discard-call-live)
    (Framework GenGrammars l36-finalize-locations)
    (Framework GenGrammars l37-expose-frame-var)
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
      ((verify-uil) verify-uil/wrapper)
      ((remove-complex-opera*) remove-complex-opera*/wrapper)
      ((flatten-set!) flatten-set!/wrapper)
      ((impose-calling-conventions) impose-calling-conventions/wrapper)
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
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

;;-----------------------------------
;; source/wrapper
;; verify-uil/wrapper
;;-----------------------------------
(define-language-wrapper
  (source/wrapper verify-uil/wrapper)
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow locals true false nop)
    (only (chezscheme) letrec))
  (reset-machine-state!)
  ,x ;,(if (grammar-verification) (verify-grammar:l01-verify-scheme x) x)
  )

;;-----------------------------------
;; remove-complex-opera*/wrapper
;;-----------------------------------
;; DUPLICATE of verify-scheme/wrapper except for the grammar check:
(define-language-wrapper
  (remove-complex-opera*/wrapper)
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow locals true false nop)
    (only (chezscheme) letrec))
  (reset-machine-state!)
  ,x ;,(if (grammar-verification) (verify-grammar:l23-remove-complex-opera x) x)
  )

;;-----------------------------------
;; flatten-set!/wrapper
;;-----------------------------------
;; DUPLICATE of verify-scheme/wrapper except for the grammar check:
(define-language-wrapper
  (flatten-set!/wrapper)
  (x)
  (environment env)
  ,set! ,alloc
  (import
    (only (Framework wrappers aux)
      handle-overflow locals true false nop)
    (only (chezscheme) letrec))
  (reset-machine-state!)
  ,x ; ,(if (grammar-verification) (verify-grammar:l24-flatten-set x) x)
  )

;;-----------------------------------
;; impose-calling-conventions/wrapper
;;-----------------------------------
(define-language-wrapper (impose-calling-conventions/wrapper)
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-complex
  ,new-frames ,alloc
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l25-impose-calling-conventions x) x)))
  ,return-value-register)

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
      frame-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
    ,(if (grammar-verification) (verify-grammar:l27-uncover-frame-conflict x) x)
    ))
  ,return-value-register)

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
      frame-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
       ,(if (grammar-verification) (verify-grammar:l28-pre-assign-frame x) x) 
       ))
  ,return-value-register)


;;----------------------------------
;; assign-new-frame
;;----------------------------------
(define-language-wrapper 
  (assign-new-frame/wrapper 
   assign-frame/wrapper)
  (x)
  (environment env)
  (define frame-size ,(compute-frame-size x))
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locals ulocals spills locate
      frame-conflict true false nop))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(if (grammar-verification) (verify-grammar:l29-assign-new-frame x) x)
      ))
  ,return-value-register)

;;-----------------------------------
;; finalize-frame-locations/wrapper
;;-----------------------------------
(define-language-wrapper
  (finalize-frame-locations/wrapper
   select-instructions/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate
      locals ulocals frame-conflict
      true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,(if (grammar-verification) (verify-grammar:l30-finalize-frame-locations x) x)))
  ,return-value-register)

#;
;;-----------------------------------
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
;; DUPLICATE OF ABOVE WITH DIFFERENT GRAMMAR VERIFICATION:
(define-language-wrapper
  (select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate
      locals ulocals frame-conflict
      true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k)
       ,(if (grammar-verification) (verify-grammar:l30-finalize-frame-locations x) x)))
  ,return-value-register)

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  ,set!
  ,return-point-simple
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate locals ulocals
      frame-conflict register-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l32-uncover-register-conflict x) x)))
  ,return-value-register)

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec locate locals ulocals
      spills frame-conflict true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l33-assign-registers x) x)))
  ,return-value-register)

(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import (only (Framework wrappers aux)
             handle-overflow letrec locate
            true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l35-discard-call-live x) x)))
  ,return-value-register)

(define-language-wrapper finalize-locations/wrapper
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow letrec true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) 
		   ,(if (grammar-verification) (verify-grammar:l36-finalize-locations x) x)))
  ,return-value-register)


(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  ,return-point-simple
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow true false nop)
    (only (chezscheme) letrec))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l37-expose-frame-var x) x))))
  ,return-value-register)

(define-language-wrapper expose-basic-blocks/wrapper
  (x)
  (environment env)
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow)
    (only (chezscheme) letrec))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds (if (grammar-verification) (verify-grammar:l39-expose-basic-blocks x) x))))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  ,set!
  (import
    (only (Framework wrappers aux)
      handle-overflow code jump)
    (only (chezscheme) letrec))
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
