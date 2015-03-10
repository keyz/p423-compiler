(library (GrammarCompiler haskell derive-printing)
         (export derive-printing)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux))

;; deriving-printing operates on the saved copy of the source grammars
;; subforms, deriving a haskell function for each that produces a string
;; representation of haskell data that is identical to the scheme sexp form.

(define derive-printing
  (lambda (x)

    (define Sub
      (lambda (x)
        (match x
          ;; `(,name . ,fields) is the flattened version, form is the original
          (((,name . ,fields) ,form)
           (let ((field-names (field-names fields)))
             (let-values (((_ p) (Form (map cons field-names fields) form)))
               (let ((p (place-ppSexps (condense-structures p))))
                 `((,name . ,fields)
                   (print (,name . ,field-names) ,p))))))
          (,e (errorf 'derive/Sub "invalid: ~a" e)))))

    ;; Produces isomorphic haskell pretty-printing function
    (define Form
      (lambda (vars form)
        (match form
          (,nt (guard (non-terminal? nt))
               (let ((p (car vars)) (vars (cdr vars)))
                 (let ((v (car p)) (t (cdr p)))
                   (values vars `(pp ,v)))))
          (() (values vars '()))
          ((,l * . ,rest)
           (let-values (((vars p) (List vars l)))
             (let ((f (car p)) (v (cdr p)))
               (let-values (((vars rest) (Form vars rest)))
                 (values vars `(append (map ,f ,v) ,rest))))))
          ((,p . ,rest) (guard (list? p))
           (let-values (((vars p) (Form vars p)))
             (let-values (((vars rest) (Form vars rest)))
               (values vars `(cons ,p ,rest)))))
          ((,t . ,rest) (guard (terminal? t))
           (let-values (((vars rest) (Form vars rest)))
             (values vars `(cons (string ,t) ,rest))))
          ((,nt . ,rest) (guard (non-terminal? nt))
           (let ((p (car vars)) (vars (cdr vars)))
             (let ((v (car p)) (t (cdr p)))
               (let-values (((vars rest) (Form vars rest)))
                 (values vars `(cons (pp ,v) ,rest))))))
          (,e (errorf 'derive/Form "invalid: ~a" e)))))

    (define List
      (lambda (vars form)
        (let ((p (car vars)) (vars (cdr vars)))
          (let ((v (car p)) (t (cdr p)))
            (match t
              ((list (tuple . ,ts))
               (let ((vs (field-names ts)))
                 (let-values (((_ f) (Form (map cons vs ts) form)))
                   (values vars `((lambda ,vs ,f) . ,v)))))
              ((list ,x)
               (values vars `(pp . ,v)))
              (,e (errorf 'derive/List "invalid: ~a" e)))))))

    (match x
      ((tags ,tags
         ;(terminals ,t/p*
           (module ,name (data ,name* ,[Sub -> sub*] ...) ...))
         ;)
       `(tags ,tags
          ;(terminals ,t/p*
            (module ,name (data ,name* ,sub* ...) ...)))
          ;)
      (,e (errorf 'derive-printing "invalid: ~a" e)))))

;; produces a list of names which each represent a haskell datum's field.
(define field-names
  (lambda (fs)
    (numbered-names
     (map (lambda (f) (if (pair? f) 'l f))
          fs))))

(define place-ppSexps
  (lambda (e)
    (define place
      (lambda (b e)
        (if b `(ppSexp ,e) e)))
    (define Elem
      (lambda (e)
        (match e
          ((list ,[l] ...)
           `(ppSexp (list . ,l)))
          ((cons ,[a] ,[List -> d])
           `(ppSexp (cons ,a ,d)))
          ((append ,[List -> l1] ,[List -> l2])
           `(ppSexp (append ,l1 ,l2)))
          ((map pp ,e) `(ppSexp (map pp ,e)))
          ((map (lambda ,fml ,[Elem -> body]) ,e)
           `(ppSexp (map (lambda ,fml ,body) ,e)))
          (,e e))))
    (define List
      (lambda (e)
        (match e
          ((list ,[Elem -> l] ...)
           `(list . ,l))
          ((cons ,[Elem -> a] ,[d])
           `(cons ,a ,d))
          ((append ,[l1] ,[l2])
           `(append ,l1 ,l2))
          ((map pp ,e) `(map pp ,e))
          ((map (lambda ,fml ,[Elem -> body]) ,e)
           `(map (lambda ,fml ,body) ,e))
          (,e (errorf 'place-ppSexps "huh? ~a" e)))))
    (Elem e)))

;; Cleans up pretty-printing function into readable form,
;; as well as raises ppSexp function to proper positions
(define condense-structures
  (lambda (e)
    (match e
      ((cons ,[a] ,[d])
       (match d
         ((list . ,l) `(list ,a . ,l))
         ((append ,l1 ,l2) `(cons ,a (append ,l1 ,l2)))
         (,else `(cons ,a ,d))))
      ((append ,[l1] ,[l2])
       (match `(,l1 ,l2)
         ((,ll1 (list)) ll1)
         (((list) ,ll2) ll2)
         (((list . ,ll1) (list . ,ll2))
          `(list ,ll1 ... . ,ll2))
         (((list . ,ll1) (cons ,a ,d))
          `(append (list ,ll1 ... ,a) ,d))
         (,e `(append ,l1 ,l2))))
      (() '(list))
      ((map pp ,x)
       `(map pp ,x))
      ((map (lambda ,fml* ,[e]) ,x)
       `(map (lambda ,fml* ,e) ,x))
      ((string ,s) `(string ,s))
      ((pp ,s) `(pp ,s))
      (,e (errorf 'condense "invalid: ~a" e)))))

)
