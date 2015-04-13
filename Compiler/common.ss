(library (Compiler common)
  (export
    binop?
    relop?
    triv?
    primitives
    value-prim?
    pred-prim?
    effect-prim?
    prim?)
  
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

  (define (binop? x)
    (memq x '(mref + - * logand logor sra)))
  
  (define (relop? x)
    (memq x '(< > <= >= =)))

  (define (triv? t)
    (or (uvar? t)
        (label? t)
        (int64? t)))

  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (set-car! . 2) (set-cdr! . 2)
      (vector? . 1) (vector-length . 1) (vector-ref . 2)
      (vector-set! . 3) (void . 0)))
  
  (define (value-prim? x)
    (or (memq x '(+ - * car cdr cons make-vector vector-length vector-ref void))
        (memq x '(make-procedure procedure-code procedure-ref))))

  (define (pred-prim? x)
    (or (memq x '(< <= = >= > boolean? eq? fixnum? null? pair? vector?))
        (memq x '(procedure?))))

  (define (effect-prim? x)
    (or (memq x '(set-car! set-cdr! vector-set!))
        (memq x '(procedure-set!))))

  (define (prim? x)
    (or (value-prim? x)
        (pred-prim? x)
        (effect-prim? x)
        (eq? x 'procedure?)
        (memq x '(make-procedure procedure-code procedure-ref procedure-set!))))
  
  )
