(library (GrammarCompiler scheme emit-scheme)
         (export emit-scheme)
         (import (chezscheme))

(define (emit-scheme filename passname)
  (lambda (maindef)
    (printf ";; Automatically generated file -- DO NOT MODIFY\n")
    (pretty-print
     `(library (Framework GenGrammars ,filename)
	  (export ,passname)
	  (import (chezscheme) 
		  (Framework match)
		  (Framework prims))
	  (define (any . nested-bool-ls)
            (letrec ((helper 
		      (lambda (x)
			(cond 
			 [(not x)   #f]
			 [(null? x) #f]
			 [(pair? x) (or (helper (car x)) (helper (cdr x)))]
			 [else      x]
			 ))))
	      (helper nested-bool-ls)))
	  ,maindef)
     )))
)
