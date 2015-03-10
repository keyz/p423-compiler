(library (GrammarCompiler common desugar-directives)
         (export desugar-directives)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux))

(define desugar-directives
  (lambda (x)
    (match x
      ((p423-grammars ,init . ,rest)
       (let ((gs (process-rest init rest)))
         `(p423-grammars . ,gs))))))

(define process-rest
  (lambda (prev gs)
    (scan-left increment-grammar prev gs)))

(define increment-grammar
  (lambda (prev g)

    (define Stage1
      (lambda (ds)
        (match ds
          (() '(() () ()))
          (((%remove . ,r*) . ,rest)
           (cons (map process-remove r*)
                 (Stage2 rest)))
          (,else (cons '() (Stage2 else))))))

    (define Stage2
      (lambda (ds)
        (match ds
          (() '(() ()))
          (((%rename . ,r*) . ,rest)
           (cons (map process-rename r*)
                 (Stage3 rest)))
          (,else (cons '() (Stage3 else))))))

    (define Stage3
      (lambda (ds)
        (match ds
          (() '(()))
          (((%add . ,a*))
           (list (map process-add a*)))
          (,else
           (errorf 'increment-grammar "Badly ordered directives: ~a" else)))))

    (define process-remove
      (lambda (r)
        (match r
          ((,NT . ,sub*) r)
          (,NT `(,r)))))

    (define process-rename
      (lambda (r)
        (match r
          ((,NT1 -> ,NT2) `(,NT1 . ,NT2))
          (,else (errorf 'process-rename "bad rename directive: ~a" else)))))

    (define process-add
      (lambda (a)
        (match a
          ((,NT . ,sub*) a)
          (,else
           (errorf 'process-add "bad add directive: ~a" else)))))

    (let ((name (car g)) (ds (cdr g)))
      (let ((ds (Stage1 ds)))
        (let ((rem (car ds)) (ren (cadr ds)) (add (caddr ds)))
          (match prev
            ((,old-name (start ,st) . ,[(compose (Remove rem) (Rename ren) (Add add) merge-alists) -> types])
             (let ((result `(,name (start ,st) . ,types)))
	       (pretty-print result)
	       result))
            (,e (errorf 'increment-grammar "invalid: ~a" e))))))))

(define Remove
  (lambda (r*)
    (lambda (g)

      (define Terminals
        (lambda (ts)
          (if
           (null? ts) '()
           (let ((t (car ts)))
             (cond
               ((assq (car t) r*) =>
                (lambda (p)
                  (Terminals (cdr ts))))
               (else
                (cons t (Terminals (cdr ts)))))))))

      (define Types
        (lambda (ts)
          (if
           (null? ts) '()
           (let ((t (car ts)))
             (cond
               ((assq (car t) r*) =>
                (lambda (p)
                  (let ((type (car p)) (rem-subs (cdr p)))
                    (cond
                      ((null? rem-subs)
                       (Types (cdr ts)))
                      (else
                       (let ((new-subs
                              (remp
                               (lambda (s)
                                 (if (list? s)
                                     (memq (car s) rem-subs)
                                     (memq s rem-subs)))
                               (cdr t))))
                         (cond
                           ((null? new-subs)
                            (Types (cdr ts)))
                           (else
                            (cons
                             (cons type new-subs)
                             (Types (cdr ts)))))))))))
               (else
                (cons t (Types (cdr ts)))))))))

      (Types g))))

(define Rename
  (lambda (r*)
    (lambda (g)

      (define Types
        (lambda (ts)
          (cond
            ((null? ts) '())
            ((pair? (car ts))
             (cons (Types (car ts))
                   (Types (cdr ts))))
            ((assq (car ts) r*) =>
             (lambda (p)
               (cons (cdr p)
                     (Types (cdr ts)))))
            (else (cons (car ts)
                        (Types (cdr ts)))))))

      (Types g))))

(define Add
  (lambda (a*)
    (lambda (g)
      (append g a*))))

(define scan-left
  (lambda (f q ls)
    (let ((res (cond
                 ((null? ls) '())
                 (else
                  (let ((q (f q (car ls))))
                    (scan-left f q (cdr ls)))))))
      (cons q res))))

(define trans2-map
  (lambda (f ls)
    (let ((ls (map f ls)))
      (map (lambda (ls) (apply append ls))
           (list (map car ls) (map cdr ls))))))

(define merge-alists
  (lambda (als*)
    (if
     (null? als*) '()
     (let ((first (car als*)) (rest (cdr als*)))
       (let-values (((these others)
                     (partition (lambda (ls) (eq? (car ls) (car first))) rest)))
         (cons
          (cons (car first)
                (apply append (cdr first) (map cdr these)))
          (merge-alists others)))))))

)
