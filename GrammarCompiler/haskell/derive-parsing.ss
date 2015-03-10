;(library (GrammarCompiler haskell derive-parsing)
;         (export derive-parsing)
         (import (chezscheme)
                 (GrammarCompiler common aux)
                 (GrammarCompiler common match))

(define derive-parsing
  (lambda (x)
    (match x
      ((module ,name ,[Type -> t*] ...)
       `(module ,name . ,t*)))))

(define Type
  (lambda (x)
    (match x
      ((data ,name . ,s*)
       (let ((s* (map (Sub name) s*)))
         `(data ,name . ,s*))))))

(define Sub
  (lambda (n)
    (lambda (x)
      (match x
        (((,name . ,ts)
          ,lhs
          (print ,p))
         (let-values (((e fs m w) (gen-parsing p ts)))
           (let ((pf (parse-function n)))
             (let ((all-of-it `(,pf ,n ,e ,m ,lhs . ,w)))
               `((,name . ,ts)
                 ,lhs 
                 (print ,p)
                 (parse ,all-of-it))))))))))

;(data Tail
;  ((app Triv)
;    (t)
;    (print (ppSexp (list (string app) (pp t))))
;    (app Triv))
;  ((begin (list Effect) Tail)
;    (l t)
;    (print
;      (ppSexp
;        (cons (string begin) (append (map pp l) (list (pp t))))))
;    (begin Effect * Tail)))

;parseTail :: LispVal -> Maybe Tail
;parseTail (List ((Symbol begin) : l)) =
;  do (l,t) <- parseListWithFinal parseEffect parseTail l
;     return (Begin l t)
;
;(parseTail
; ((List (cons (Symbol begin) l))
;  (((tuple l t) . (parseListWithFinal parseEffect parseTail l))
;   (Begin l t))))

(define parse
  (lambda (x n)

    (define List (lambda (x n) (values x n '())))

    (match x
      (,t (guard (terminal? t))
          (values `(Symbol ,t) n '()))
      (,nt (guard (non-terminal? nt))
           (let ((f (parse-function nt))
                 (v (make-var 'x n)))
             (values v n `((,v . (,f ,v))))))
      ((,ls *)
       (let ((v (make-var 'ls n)))
         (let-values (((f _ m) (List ls (add1 n))))
           (values v n `((,v . (parseList ,f ,v)) . ,m)))))
      ((,ls * ,last)
       (let ((v1 (make-var 'ls n)))
         (let-values (((f n m) (List ls (add1 n))))
           (let ((v2 (make-var 'x n)))
             (let-values (((last n m^) (parse last (add1 n))))
               (values `(,v1 ,v2) n `(,@m . ,m^)))))))
      ((,a . ,d)
       (let ((v1 (make-var 'x n)))
         (let-values (((a n m) (parse a (add1 n))))
           (let ((v2 (make-var 'x n)))
             (let-values (((d n m^) (parse d (add1 n))))
               (values `(,v1 . ,v2) n `(,@m . ,m^)))))))
      (() (values '() n '())))))

(define map-vals
  (lambda (f ls fs)
    (cond
      ((null? ls) (values '() fs '() '()))
      (else
       (let-values (((a fs m w) (f (car ls))))
         (let-values (((d fs m^ w^) (map-vals f (cdr ls) fs)))
           (values `(,a . ,d) fs `(,@m . ,m^) `(,@w . ,w^))))))))

(define parse-function
  (lambda (s)
    (string->symbol
     (string-append
      "parse"
      (symbol->string s)))))

(define make-var
  (lambda (s n)
    (string->symbol
     (string-append
      (symbol->string s)
      (number->string n)))))

;)
