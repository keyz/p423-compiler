#!/usr/bin/scheme-script
(import (GrammarCompiler main))

(define source-grammar-file
  (lambda ()
    (let ((args (command-line-arguments)))
      (if (not (= (length args) 1))
          (errorf 'compile-grammars (usage args))
          (car args)))))

(define usage
  (lambda (a)
    (printf "Invalid arguments: ~a\n" a)
    (printf "Usage:\n")
    (printf "  scheme --script compile_grammars.ss <src_file>\n\n")))

(compile-grammars (source-grammar-file)
  (scheme-path "Framework/GenGrammars")
  (haskell-path "FrameworkHs/GenGrammars"))
