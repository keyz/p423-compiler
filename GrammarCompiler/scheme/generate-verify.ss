(library (GrammarCompiler scheme generate-verify)
         (export generate-verify verifier-name)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux))

(define generate-verify
  (lambda (x)
    (match x
      ((,[Name -> name]
        (start ,st)
        ;(with-terminals ,[Terminal -> t/p*] ...)
        ,[Type -> t*] ...)
       `(define ,name
          (lambda (x)
            ,t* ...
            ;,t/p* ...
            (let ((res (,st x)))
              (if res (errorf ',name "~a" res) x))))))))

(define Name
  (lambda (s)
    (string->symbol
     (string-append
      "verify-grammar:"
      (symbol->string s)))))

(define verifier-name Name)

(define Terminal
  (lambda (t/p)
    (let ((term (car t/p)) (pred (cadr t/p)))
      `(define ,term
         (lambda (x)
           (if (,pred x) #f (invalid-expr ',term x)))))))

(define Type
  (lambda (x)
    (match x
      ((,name . ,sub*)
       (let-values (((s ns) (partition non-terminal? sub*)))
         (let* ((grouped (group-by-head ns))
		(s (map Simple s)))
           `(define ,name
              (lambda (x)
                (match x
                  ,s ...
                  ,grouped ...
                  (,(uq 'e) (invalid-expr ',name e)))
		))))))))

(define Simple
  (lambda (s)
    `(,(uq 'e) (guard (not (,s e))) #f)))

;; Grammars for this course are non-ambiguous.  However, the leading
;; symbol does not uniquely identify a given construct in the grammar.
;; 
;; This function groups non-terminals based on their leading symbol.
(define (group-by-head ls)
  (match ls 
    [() '()]
    [((,sym ,bod* ...) ,rst ...) (guard (symbol? sym))
     (let-values ([(sibs others) (partition (lambda (x) (and (pair? x) (eq? sym (car x)))) rst)])
       (let ([this-clause (if (null? sibs)
			      (SubMatchClause (car ls))
			      `[(,sym . ,(uq 'bod)) 
				(and ,(map (lambda (variant)
					     `(match (cons ',sym bod)
						,(SubMatchClause variant)
						[,(uq 'e) (invalid-expr ',sym e)]))
					(cons (car ls) sibs))
				     ...)]
			      )])
	 (cons this-clause (group-by-head others))))]
    [,x (errorf 'group-by-head "expected non-terminal to be a list headed by a symbol: ~a" x)]))

;; Handle each nontrivial pattern for a nonterminal:
(define (SubMatchClause s)
  (let-values
        (((s n seen)
          (let loop ((s s) (n 1) (seen '()))
            ;; seen - accumulate nonterminals encountered
            (cond
              ((null? s) (values '() n seen))
              (else
               (let ((a (car s)) (d (cdr s)))
                 (cond
                   ((pair? a)
                    (let-values (((a n seen) (loop a n seen)))
                      (let-values (((d n seen) (loop d n seen)))
                        (values `(,a . ,d) n seen))))
                   ((terminal? a)
                    (let-values (((d n seen) (loop d n seen)))
                      (values `(,a . ,d) n seen)))
                   ((non-terminal? a)
                    (let ((name (number-symbol "x" n)))
                      (let-values (((d n seen) (loop d (add1 n) (cons name seen))))
                        (values
                         (cons (uq `(,a -> ,name)) d)
                         n
                         seen))))
                   ((eq? a '*)
                    (let-values (((d n seen) (loop d n seen)))
                      (values `(... . ,d) n seen))))))))))
    `(,s (any . ,seen))
      ))

(define uq
  (lambda (s)
    (list 'unquote s)))

)
