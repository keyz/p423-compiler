(library (GrammarCompiler haskell assign-tags)
         (export assign-tags)
         (import (chezscheme)
                 (GrammarCompiler common aux)
                 (GrammarCompiler common match))

;; assign-tags computes haskell type constructor names
;; from the map of tags produced by flatten-datatypes,
;; and then walks over the module, renaming all subforms
;; with the computed results.

;(define prints '())

(define assign-tags
  (lambda (x)
    (match x
      ((tags ,tags ,module)
       (let ((assigned-pre (shortest-distinct-prefixes
                            (append (prep-builtins) (tagged-sl* tags)))))
         (let ((assigned (map (lambda (tt)
                                (let ((tag (car tt))
                                      (type-ls (cdr tt)))
                                  (cons tag (make-haskell-type type-ls))))
                              assigned-pre)))
           (walk-module assigned module))))
      (,else (errorf 'assign-tags "invalid: ~a" else)))))

;; produces a haskell datatype constructor out of the output from
;; shortest-distinct-prefixes. s* is a list with a symbol as its
;; car and a list of characters as its cdr.
(define make-haskell-type
  (lambda (s*)
    (string->symbol
     (string-append
      (capitalize-string
       (scheme->haskell/string
        (symbol->string
         (car s*))))
      (list->string (cdr s*))))))

(define walk-module
  (lambda (assigned x)
    (define Module
      (lambda (x)
        (match x
          ((module ,[haskell-upcase-name -> name]
             ,[Type -> types] ...)
           ;; lifting prints from the subform level to the module level
           `(module ,name . ,types))
          (,else (errorf 'assign/Module "invalid: ~a" else)))))

    (define Type
      (lambda (x)
        (match x
          ((data ,name . ,subs)
           (let ((assigned-here (make-tag-alist name assigned)))
             (let ((subs (Subs assigned-here subs)))
               `(data ,name . ,subs))))
               ;(match subs
               ;  (((,forms (print ,ps ,fs)) ...)
               ;   ;(set! prints `((,name (,ps ,fs) ...) . ,prints))
               ;   `(data ,name . ,forms))))))
          (,else (errorf 'assign/Type "invalid: ~a" else)))))

    (define Subs
      (lambda (assigned-here s*)
        (cond
          ((null? s*) '())
          (else
           (let ((s (car s*)))
             (let-values
                 (((s rest)
                   (match s
                     (((,name . ,fields)
                       (print (,name . ,field-names) ,print))
                      (cond
                        ((assq name assigned-here) =>
                         (lambda (p)
                           (let ((const (cdr p)))
                             (values
                              `((,const . ,fields)
                                (print (,const . ,field-names) ,print))
                              (remove p assigned-here)))))
                        (else (values s assigned-here)))))))
               (cons s (Subs rest (cdr s*)))))))))
    (Module x)))

(define make-tag-alist
  (lambda (name assigned)
    (map
     (lambda (tt)
       (let ((t (caar tt))
             (tag (cdr tt)))
         (cons t tag)))
     (filter
      (lambda (tt)
        (eq? (cdar tt) name))
      assigned))))

(define shortest-distinct-prefixes
  (lambda (ls*)
    (cond
      ((null? ls*) '())
      ((null? (cdr ls*))
       (let ((first (car ls*)))
         (let ((tag (car first)))
           (list (cons tag '())))))
      ((all-equal? ls*)
       (distinct-suffixes
        (map
         (lambda (ls)
           (let ((tag (car ls)))
             (cons tag '())))
         ls*)))
      (else
       (let-values (((n nn)
                     (partition
                      (lambda (ls) (null? (cdr ls)))
                      ls*)))
         (apply
          append
          (distinct-suffixes n)
          (let ((grouped (group-by-first nn)))
            (map
             (lambda (ls*)
               (rebuild-lists
                (cadar ls*)
                (shortest-distinct-prefixes
                 (map
                  (lambda (ls)
                    (cons
                     (car ls)
                     (cddr ls)))
                  ls*))))
             grouped))))))))

(define group-by-first
  (lambda (ls*)
    (cond
      ((null? ls*) '())
      (else
       (let ((first (car ls*))
             (rest (cdr ls*)))
         (let ((tag (car first))
               (suf (cdr first)))
           (let ((c (car suf)))
             (let-values
               (((first-group other-groups)
                 (partition
                  (lambda (ls)
                    (let ((tag (car ls))
                          (suf (cdr ls)))
                      (eq? (car suf) c)))
                  rest)))
               (cons
                (cons first first-group)
                (group-by-first other-groups))))))))))

(define all-equal?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((null? (cdr ls)) #t)
      (else
       (let ((a (car ls)) (d (cdr ls)))
         (let ((ad (car d)))
           (and (equal? a ad)
                (all-equal? d))))))))

(define distinct-suffixes
  (lambda (ls)
    (let loop ((ls ls) (i 1))
      (cond
        ((null? ls) '())
        (else
         (cons (append (car ls) (string->list (number->string i)))
               (loop (cdr ls) (+ i 1))))))))

;(define distinct-suffixes
;  (lambda (ls-null)
;    (cond
;      ((null? ls-null) '())
;      (else
;       (cons (car ls-null)
;             (map
;              (lambda (ls)
;                (cons (car ls)
;                      (cons #\' (cdr ls))))
;              (distinct-suffixes (cdr ls-null))))))))

(define rebuild-lists
  (lambda (c ls*)
    (map
     (lambda (ls)
       (cons (car ls) (cons c (cdr ls))))
     ls*)))

(define prep-builtins
  (lambda ()
    (map
     (lambda (t)
       `((,t) . (,t)))
     (builtin-tags))))

(define tagged-sl*
  (lambda (t*)
    (cond
      ((null? t*) '())
      (else
       (let ((tag (caar t*)))
         (let loop ((types (cdar t*)))
           (cond
             ((null? types)
              (tagged-sl* (cdr t*)))
             (else
              (let ((first-type (car types))
                    (other-types (cdr types)))
                (let ((type-string (sym->char-list first-type))
                      (rest (loop other-types)))
                  `(((,tag . ,first-type) ,tag . ,type-string)
                    . ,rest)))))))))))

(define sym->char-list
  (lambda (x)
    (string->list
     (symbol->string x))))

)
