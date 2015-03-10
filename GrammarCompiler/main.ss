(library (GrammarCompiler main)
         (export compile-grammars scheme-path haskell-path)
         (import (chezscheme)
                 (GrammarCompiler common match)
                 (GrammarCompiler common aux)
                 (GrammarCompiler common desugar-directives)
                 (GrammarCompiler haskell flatten-datatypes)
                 (GrammarCompiler haskell derive-printing)
                 (GrammarCompiler haskell assign-tags)
                 ;(GrammarCompiler haskell derive-parsing)
                 (GrammarCompiler haskell lift-prints)
                 (GrammarCompiler haskell emit-haskell)
                 (GrammarCompiler scheme generate-verify)
                 (GrammarCompiler scheme emit-scheme))

(define (scheme-path x) (syntax-violation #f "misplaced aux keyword" x))
(define (haskell-path x) (syntax-violation #f "misplaced aux keyword" x))

(define-syntax compile-grammars
  (syntax-rules (scheme-path haskell-path)
    ((_ src (scheme-path ss-path) (haskell-path hs-path))
     (main src ss-path hs-path))))

(define haskell-passes
  (compose
   flatten-datatypes
   derive-printing
   assign-tags
   ;derive-parsing))
   lift-prints))

(define scheme-passes
  (compose
   generate-verify))

(define main
  (lambda (source-file scheme-path haskell-path)
    (let ((grammars (read-file source-file)))
      (printf "========================================\n")
      (printf " Desugared grammars: \n")
      (printf "========================================\n")
      (match (desugar-directives grammars)
        ((p423-grammars (,name* . ,g*) ...)
         (printf "========================================\n")
         (map (lambda (name g)
                (printf " * Codegen for grammar: ~s\n" name)
                (let ((g `(,name . ,g)))
                  (begin
                    (write-haskell haskell-path name g)
                    (write-scheme scheme-path name g))))
              name* g*))))))

(define write-code
  (lambda (code-f name-f suf out-f path name g)
    (let ((code (code-f g))
          (outfile
           (string-append
            path "/"
            (name-f (symbol->string name))
            suf)))
      (with-output-to-file outfile
        (lambda ()
          (out-f code))
        'replace)
      name)))

(define write-haskell
  (lambda (path name code)
    (write-code haskell-passes
                (compose scheme->haskell/string
                         capitalize-string)
                ".hs"
                (emit-haskell path)
                ;pretty-print
                path
                name
                code)))

(define write-scheme
  (lambda (path name code)
    (write-code scheme-passes
                scheme->filename/string
                ".ss"
                (emit-scheme name (verifier-name name))
                path
                name
                code)))

(define read-file
  (lambda (f)
    (call-with-input-file f read)))

)
