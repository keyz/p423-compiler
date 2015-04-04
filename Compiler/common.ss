(library (Compiler common)
  (export
    binop?
    relop?
    triv?)
  
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
  
  )
