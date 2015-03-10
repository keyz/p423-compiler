(library (GrammarCompiler haskell lift-prints)
         (export lift-prints)
         (import (chezscheme)
                 (GrammarCompiler common aux)
                 (GrammarCompiler common match))

(define lift-prints
  (lambda (x)
    (match x
      ((module ,name . ,t*)
       (let-values (((t* p*) (Types t*)))
         `((module ,name . ,t*)
           (print . ,p*)))))))

(define Type
  (lambda (t)
    (match t
      ((data ,name . ,s*)
       (let-values (((s* p*) (Subs name s*)))
         (values `(data ,name . ,s*) p*))))))

(define Sub
  (lambda (s)
    (match s
      ((,s (print . ,p))
       (values s `(,p))))))

(define map-2vals
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) (values '() '()))
        (else
         (let-values (((x v*) (f (car ls))))
           (let-values (((x* v**) ((map-2vals f) (cdr ls))))
             (values `(,x . ,x*) `(,@v* . ,v**)))))))))

(define Types (map-2vals Type))
(define Subs
  (lambda (name s*)
    (let-values (((s* p*) ((map-2vals Sub) s*)))
      (values s* `((,name . ,p*))))))

)
