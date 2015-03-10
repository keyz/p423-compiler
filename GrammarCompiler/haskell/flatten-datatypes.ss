(library (GrammarCompiler haskell flatten-datatypes)
         (export flatten-datatypes)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux))

;; flatten-datatypes traverses a single grammar and
;; performs the following operations:
;;   - collects an alist mapping key terminal symbols to
;;     the list of non-terminals under which they occur.
;;   - drops the start symbol form
;;   - flattens each subform of the non-terminals, but
;;   - saves a copy of the original subforms, to
;;     be used to derive pretty-printing functions

(define seen-tags '())

(define flatten-datatypes
  (lambda (x)
    (set! seen-tags '())
    (match x
      ((,name
        (start ,st)
        ,[Type -> type*] ...)
       `(tags ,seen-tags
         (module ,name . ,type*)))
      (,e (errorf 'flatten-datatypes "invalid grammar: ~a" e)))))

;; builds an alist that merges the values associated with a given key.
(define add-tag
  (lambda (tag type)
    (set! seen-tags
      (let-values (((used-tag rest)
                    (partition
                     (lambda (t)
                       (eq? (car t) tag))
                     seen-tags)))
        (cond
          ((null? used-tag) `((,tag ,type) . ,seen-tags))
          (else
           (let ((used-tag (car used-tag)))
             (let ((used-types (cdr used-tag)))
               (cons `(,tag ,type . ,used-types) rest)))))))))

(define Type
  (lambda (x)
    (match x
      ((,name ,[(Sub name) -> sub] ,[(Sub name) -> sub*] ...)
       `(data ,name ,sub . ,sub*))
      (,else (errorf 'flatten/Type "invalid: ~a" else)))))

;; Sub returns (e^ e), where e is the original subform, and
;; e^ is the subform after flattening. The copies are for printing
;; and parsing purposes.
(define Sub
  (lambda (type)
    (lambda (x)
      (match x
        ((,t . ,e*) (guard (terminal? t))
         (begin
           (add-tag t type)
           (let ((new-e* (flatten-form e*)))
             `((,t . ,new-e*) (,t . ,e*)))))
        (,nt (guard (non-terminal? nt))
             (let ((tag (string->symbol
                         (string-downcase
                          (symbol->string nt)))))
               (begin
                  ;; covers cases like Disp, where a Loc will use the Disp constructor,
                  ;; but the Disp datatype will also.
                 (add-tag nt type)
                 `((,nt ,nt) ,nt))))
        ;; untagged subforms, eg. (Relop Triv Triv), are tagged with 'app'
        (,e*
         (begin
           (add-tag 'app type)
           (let ((new-e* (flatten-form e*)))
             `((app . ,new-e*) ,e*))))
        (,else (errorf 'flatten/Sub "invalid: ~a" else))))))

;; removes nesting and terminal symbols to produce form
;; that will directly translate to haskell datatype.
(define flatten-form
  (lambda (x)
    (match x
      (,t (guard (terminal? t)) '())
      (,nt (guard (non-terminal? nt)) `(,nt))
      (() '())
      ((,[List -> x] * . ,[rest])
       (append x rest))
      ((,[x] . ,[rest])
       (append x rest))
      (,else (errorf 'flatten-form "invalid: ~a" else)))))

;; haskell lists must be uniformly typed.
;; a single item in the list produces a list of that type.
;; multiple items in the list produce a list of tuples.
(define List
  (lambda (x)
    (cond
      ((null? x) '())
      ((or (atom? x)
           (null? (cdr x)))
       `((list ,x)))
      (else
       (let ((x (flatten-form x)))
         `((list (tuple . ,x))))))))

)
