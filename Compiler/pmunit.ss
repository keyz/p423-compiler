(library (Compiler pmunit)
  (export pmdeb check-equal? pmt)
  (import (chezscheme))

  (define-syntax pmdeb
    (syntax-rules ()
      [(_ x) (printf "~s: ~s\n" 'x x)]
      [(_ x1 x2 ...)
       (begin
         (pmdeb x1)
         (pmdeb x2 ...))]))

  (define-syntax check-equal?
    (syntax-rules ()
      [(_ e v)
       (if (equal? e v)
           #t
           (begin
             (printf
              "\n----------------\nFAIL\nactual:     ~s \nexpected:   ~s\nexp:        ~s\n----------------\n"
              e v 'e)
             #f))]))

  (define-syntax check-equal?-with-num
    (syntax-rules ()
      [(_ n e v)
       (if (equal? e v)
           #t
           (begin
             (printf
              "\n----------------\n#~s FAIL\nactual:     ~s \nexpected:   ~s\nexp:        ~s\n----------------\n"
              n e v 'e)
             #f))]))

  (define-syntax pmt
    (syntax-rules ()
      [(_ e1 ...) (pmt-aux 1 #t e1 ...)]))

  (define-syntax pmt-aux
    (syntax-rules ()
      [(_ n b) (if b
                   (printf "cool\n")
                   (printf "not cool\n"))]
      [(_ n b (e v) e2 ...)
       (if b
           (pmt-aux (add1 n) (check-equal?-with-num n e v) e2 ...)
           (begin
             (check-equal?-with-num n e v)
             (pmt-aux (add1 n) #f e2 ...)))]))

  )
