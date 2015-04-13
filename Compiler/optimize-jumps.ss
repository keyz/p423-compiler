#|

A11 - Apr 03, 2015

pass: optimize-jumps

This pass removes redundant jumps (function calls) from the code.

how it works:
1. find all redundant stuffs (`[label (lambda () (jump))]`), then add relations (label -> jump) to a graph
2. remove all the bindings as above; for circular (mutual) references, add a dummy binding to the program (e.g., `[f$1 (lambda () (f$1))]`)
3. replace all old labels in the body with the new labels (meaningful ones)

Input & Output:
Program  ::= (letrec ((Label (lambda () Tail)) *) Tail)  ;;      
Tail     ::= (if (Relop Triv Triv) (Label) (Label))      ;;
           | (begin Effect * Tail)
           | (Triv)                                      
Effect   ::= (set! Loc Triv)                             ;;      
           | (set! Loc (Binop Triv Triv))                ;;      
Triv     ::= Loc                                         ;;      
           | Integer | Label
Loc      ::= Reg | Disp | Ind                            ;;


|#


(library (Compiler optimize-jumps)
  (export optimize-jumps)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match)
    (Compiler common)
    (Compiler utils))

  (define-who optimize-jumps

    (define (get-new-def*-and-jump-table defs*)
      (let loop ([defs* defs*] [jump-table '()] [acc '()])
        (cond
         [(null? defs*) (values (reverse acc) (reverse jump-table))]
         [(null? (car defs*)) (loop (cdr defs*) jump-table acc)]
         [(label? (caar (cddadr (car defs*)))) ;; i know, i know... but it works.
          (loop (cdr defs*)
                (graphq-add (caar defs*) `(,(caar (cddadr (car defs*)))) jump-table)
                acc)]
         [else (loop (cdr defs*) jump-table (cons (car defs*) acc))])))

    (define (resolve-jt-and-new-defs old-labels jump-table)
      (let loop ([labels old-labels] [acc '()] [optional-new-defs '()])
        (cond
         [(null? labels) (values (reverse acc) (reverse optional-new-defs))]
         [(null? (graphq-dfs (car labels) jump-table)) ;; oops, circular references
          (loop (cdr labels)
                acc
                (cons `[,(car labels) (lambda () (,(car labels)))] optional-new-defs))]
         [else (loop (cdr labels)
                     (cons `(,(car labels) . ,(car (graphq-dfs (car labels) jump-table))) acc)
                     optional-new-defs)])))

    (define replace-label
      (lambda (j-pairs)
        (lambda (label)
          (cond
           [(assq label j-pairs) => cdr]
           [else label]))))
    
    (define Tail
      (lambda (j-pairs)
        (lambda (tail)
          (match tail
            [(if (,relop ,[(Triv j-pairs) -> triv1] ,[(Triv j-pairs) -> triv2])
                 (,label1) (,label2))
             `(if (,relop ,triv1 ,triv2)
                  (,((replace-label j-pairs) label1))
                  (,((replace-label j-pairs) label2)))]
            [(begin ,[(Effect j-pairs) -> ef*] ... ,[tail])
             `(begin ,ef* ... ,tail)]
            [(,[(Triv j-pairs) -> triv]) `(,triv)]
            [,el (errorf who "Invalid Tail ~s" el)]))))
    
    (define Effect
      (lambda (j-pairs)
        (lambda (effect)
          (match effect
            [(set! ,loc (,binop ,[(Triv j-pairs) -> triv1] ,[(Triv j-pairs) -> triv2]))
             `(set! ,loc (,binop ,triv1 ,triv2))]
            [(set! ,loc ,[(Triv j-pairs) -> triv])
             `(set! ,loc ,triv)]
            [,el (errorf who "Invalid Effect ~s" el)]))))

    (define Triv
      (lambda (j-pairs)
        (lambda (triv)
          (match triv
            [,int (guard (integer? int)) int]
            [,lab (guard (label? lab)) ((replace-label j-pairs) lab)]
            [,el el])))) ;; loc

    (lambda (prog)
      (match prog
        [(letrec (,def* ...) ,tail) ;; def: [,label* (lambda () ,tail*)]
         (let-values ([(new-def jump-table) (get-new-def*-and-jump-table def*)]) ;; generate new defs (without redundant jumps), and a graph for relations
           (let-values ([(j-pairs new-defs-for-circular) ;; reduce the graph to an easy assoc. list, also gets dummy defs for mutual references
                         (resolve-jt-and-new-defs (map car jump-table) jump-table)])
             (match `(letrec ,(append new-def new-defs-for-circular) ,tail)
               [(letrec ([,f-label* (lambda () ,[(Tail j-pairs) -> f-tail*])] ...)
                  ,[(Tail j-pairs) -> f-tail])
                `(letrec ([,f-label* (lambda () ,f-tail*)] ...) ,f-tail)])))])))
  )

