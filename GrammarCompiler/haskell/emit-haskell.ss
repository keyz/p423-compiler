(library (GrammarCompiler haskell emit-haskell)
         (export emit-haskell)
         (import (chezscheme)
                 (GrammarCompiler common match))

(define pragmas
  (lambda ()
    '("StandaloneDeriving")))

(define imports
  (lambda ()
    '("FrameworkHs.Prims"
      "FrameworkHs.Helpers"
      "Text.PrettyPrint.HughesPJ (text)"
      "Blaze.ByteString.Builder (fromByteString)")))

(define derives
  (lambda ()
    '("Eq"
      "Read"
      "Show"
      "Ord")))

(define-syntax spaces
  (syntax-rules ()
    ((_) (void))
    ((_ e e* ...)
     (begin
       e (newline)
       (spaces e* ...)))))

(define emit-haskell
  (lambda (path)
    (lambda (x)
      (match x
        (((module ,name . ,types) (print . ,prints))
         (let ((path (replace #\/ #\. path))
               (type-names (map cadr types)))
           (spaces
             (Pragmas)
             (Module path name)
             (Imports)
             (Types types)
             (PP prints)
             (Deriving type-names))))))))

(define Pragmas
  (lambda ()
    (let ((ls (pragmas)))
      (cond
        ((null? ls) (newline))
        (else
          (begin
            (printf "{-# LANGUAGE OverloadedStrings #-}\n")
            (printf "{-# LANGUAGE ")
            (printf (car ls))
            (let loop ((ls (cdr ls)))
              (cond
                ((null? ls) (printf " #-}\n\n"))
                (else
                  (begin
                    (printf ",")
                    (printf (car ls))
                    (loop (cdr ls))))))))))))

(define Module
  (lambda (path name)
    (printf "module ~a.~a where\n" path name)))

(define Imports
  (lambda ()
    (let ((ls (imports)))
      (for-each
        (lambda (i)
          (printf "import ")
          (printf i)
          (newline))
        ls))))

(define Types
  (lambda (types)
    (unless (null? types)
      (match (car types)
        ((data ,name ,form ,forms ...)
         (begin
           (printf "data ~a\n" name)
           (printf "  =")
           (Form #t form)
           (newline)
           (for-each
             (lambda (f)
               (printf "  |")
               (Form #t f)
               (newline))
             forms)
           (Types (cdr types))))
        (,else
         (begin
           (printf "-- unrecognized: ~a\n" else)
           (Types (cdr types))))))))

(define Form
  (lambda (lead-space? e)
    (unless (null? e)
      (match e
        (,s (guard (symbol? s))
            (when lead-space? (printf " "))
            (printf "~a" s))
        (((list ,l) . ,rest)
         (begin
           (when lead-space? (printf " "))
           (printf "[")
           (Form #f `(,l))
           (printf "]")
           (Form lead-space? rest)))
        (((tuple ,t . ,t*) . ,rest)
         (begin
           (when lead-space? (printf " "))
           (printf "(")
           (Form #f `(,t))
           (for-each
             (lambda (t)
               (printf ",")
               (Form #f `(,t)))
             t*)
           (printf ")")))
        ((,x . ,rest)
         (begin
           (Form lead-space? x)
           (Form #t rest)))))))

(define format-term/preds
  (lambda (t/p*)
    (let ((ss (map (lambda (t/p)
                     (let ((t (car t/p)) (p (cadr t/p)))
                       (format "~a, ~a\n" t p)))
                   t/p*)))
      (let ((first (string-append "  ( " (car ss)))
            (rest (map (lambda (s)
                         (string-append "  , " s))
                       (cdr ss))))
        (string-append
         (apply string-append first rest)
         "  )\n")))))

(define PP
  (lambda (prints)
    (for-each
     (lambda (p)
       (match p
         ((,name (,form* ,f*) ...)
          (printf "instance PP ~a where\n" name)
          (let loop ((form* form*) (f* f*))
            (unless (null? form*)
              (begin
                (printf "  pp ~a = ~a\n" (car form*)  (format-pp (car f*)))
                (loop (cdr form*) (cdr f*)))))
	  ;; Ditto, except ppp:
	  (let loop ((form* form*) (f* f*))
            (unless (null? form*)
              (begin
                (printf "  ppp ~a = ~a\n" (car form*) (format-ppp (car f*)))
                (loop (cdr form*) (cdr f*)))))
	  )))
     prints)))

(define format-pp
  (lambda (f)
    (define List
      (lambda (l)
        (cond
          ((null? l) "")
          ((null? (cdr l)) (format-pp (car l)))
          (else (format "~a,~a" (format-pp (car l)) (List (cdr l)))))))
    (define Func
      (lambda (f)
        (match f
          ((lambda ,fml* ,body)
           (format "(\\(~a) -> ~a)" (List fml*) (format-pp body)))
          (pp 'pp))))
    (match f
      ((ppSexp ,[p])
       (format "(ppSexp ~a)" p))
      ((list . ,[List -> l])
       (format "[~a]" l))
      ((string ,s) (format "fromByteString \"~a\"" s))
      ((map ,fn ,v)
       (format "(map ~a ~a)" (Func fn) v))
      ((cons ,[a] ,[d])
       (format "(~a : ~a)" a d))
      ((append ,[l1] ,[l2])
       (format "(~a ++ ~a)" l1 l2))
      ((pp ,s) (format "(pp ~a)" s))
      (,e (format "~a" e)))))

;; This alternative version uses Text.PrettyPrint.HughesPJ rather than BlazeBuilder:
(define format-ppp
  (lambda (f)
    (define List
      (lambda (l)
        (cond
          ((null? l) "")
          ((null? (cdr l)) (format-ppp (car l)))
          (else (format "~a,~a" (format-ppp (car l)) (List (cdr l)))))))
    (define Func
      (lambda (f)
        (match f
          ((lambda ,fml* ,body)
           (format "(\\(~a) -> ~a)" (List fml*) (format-ppp body)))
          (pp 'ppp))))
    (match f
      ((ppSexp ,[p])
       (format "(pppSexp ~a)" p))
      ((list . ,[List -> l])
       (format "[~a]" l))
      ((string ,s) (format "text \"~a\"" s))
      ((map ,fn ,v)
       (format "(map ~a ~a)" (Func fn) v))
      ((cons ,[a] ,[d])
       (format "(~a : ~a)" a d))
      ((append ,[l1] ,[l2])
       (format "(~a ++ ~a)" l1 l2))
      ((pp ,s) (format "(ppp ~a)" s))
      (,e (format "~a" e)))))


(define Deriving
  (lambda (types)
    (let ((ls (derives)))
      (for-each
        (lambda (t)
          (for-each
            (lambda (c)
              (printf "deriving instance ~a ~a\n" c t))
            ls))
        types))))

(define print-deriving
  (lambda (types classes)
    (for-each
      (lambda (t)
        (for-each
          (lambda (c)
            (printf "deriving instance ~a ~a\n" c t))
          classes))
      types)))

(define replace
  (lambda (old new s)
    (list->string
     (let loop ((ls (string->list s)))
       (cond
         ((null? ls) '())
         ((eq? (car ls) old)
          (cons new (loop (cdr ls))))
         (else
          (cons (car ls) (loop (cdr ls)))))))))

)
