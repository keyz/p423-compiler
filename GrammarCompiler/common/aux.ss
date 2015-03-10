(library (GrammarCompiler common aux)
         (export builtin-tags
                 builtin-types
                 compose
                 haskell-upcase-name
                 scheme->haskell/string
                 scheme->filename/string
                 numbered-names
                 number-symbol
                 capitalize-string
                 char-haskell?
                 char-filename?
                 terminal?
                 non-terminal?
                 string-filter
                 to-string)
         (import (chezscheme))

(define-syntax compose
  (syntax-rules ()
    ((_ f) f)
    ((_ f f* ...)
     (lambda (x)
       ((compose f* ...)
        (f x))))))

(define builtin-tags
  (lambda ()
    '(true false)))

(define builtin-types
  (lambda ()
    '(True False Num)))

(define split
  (lambda (c ls)
    (cond
      ((null? ls) '(()))
      ((eq? (car ls) c)
       (let ((res (split c (cdr ls))))
         (cons '() res)))
      (else
       (let ((res (split c (cdr ls))))
         (let ((a (car res))
               (d (cdr res)))
           (cons (cons (car ls) a) d)))))))

(define haskell-upcase-name
  (lambda (s)
    (string->symbol
     (capitalize-string
      (scheme->haskell/string
       (symbol->string s))))))

(define scheme->filename/string
  (lambda (s)
    (string-filter
     char-filename? s)))

(define scheme->haskell/string
  (lambda (s)
    (let ((ls (string->list s)))
      (let ((ls* (map
                  (lambda (ls)
                    (list->string
                     (filter
                      char-haskell? ls)))
                  (split #\- ls))))
        (apply string-append
               (car ls*)
               (map string-titlecase (cdr ls*)))))))

(define numbered-names
  (lambda (ls)
    (let loop ((ls ls) (seen '()))
      (cond
        ((null? ls) '())
        (else
         (let ((s (downcase-prefix (car ls))))
           (let-values (((p rest)
                         (partition (lambda (p) (string=? (car p) s)) seen)))
             (cond
               ((null? p)
                (cons
                 (string->symbol s)
                 (loop (cdr ls)
                       (cons `(,s . 1) rest))))
               (else
                (let* ((p (car p))
                       (n (cdr p))
                       (n^ (add1 n)))
                  (cons
                   (number-symbol s n^)
                   (loop (cdr ls)
                         (cons `(,s . ,n^) rest)))))))))))))

(define number-symbol
  (lambda (str n)
    (string->symbol
     (string-append
      str (number->string n)))))

(define downcase-prefix
  (lambda (s)
    (string
     (char-downcase
      (string-ref (symbol->string s) 0)))))

(define capitalize-string
  (lambda (s)
    (let ((ls (string->list s)))
      (list->string
       (cons (char-upcase (car ls))
             (cdr ls))))))

(define string-filter
  (lambda (p s)
    (list->string
     (filter p (string->list s)))))

(define char-haskell?
  (lambda (c)
    (or (eq? c #\')
        (char-alphabetic? c)
        (char-numeric? c))))

(define char-filename?
  (lambda (c)
    (or (char-alphabetic? c)
        (char-numeric? c)
        (eq? c #\-))))

(define to-string
  (lambda (x)
    (format "~a" x)))

(define terminal?
  (lambda (s)
    (or (null? s)
        (and (symbol? s)
             (char-lower-case?
              (string-ref (symbol->string s) 0))))))

(define non-terminal?
  (lambda (s)
    (and (symbol? s)
         (char-upper-case?
          (string-ref (symbol->string s) 0)))))

)
