(library (Compiler generate-x86-64)
  (export generate-x86-64)
  (import (chezscheme)
    (Framework helpers)
    (Framework match))

  (define-who generate-x86-64
    (define prim->opcode
      (lambda (prim)
        (cdr (assq prim
                   '((+ . addq) (- . subq) (* . imulq)
                     (logand . andq) (logor . orq) (sra . sarq))))))
    (define relop->opcode
      (lambda (relop not?)
        (cdr (assq relop (if not?
                             '((= . jne) (< . jge) (<= . jg) (> . jle) (>= . jl))
                             '((= . je) (< . jl) (<= . jle) (> . jg) (>= . jge)))))))
    (define Code
      (lambda (ef)
        (match ef
          [,lab (guard (label? lab)) (emit-label lab)]
          [(jump ,rand) (emit-jump 'jmp rand)]
          [(set! ,rand1 ,lab)
           (guard (label? lab))
           (emit 'leaq lab rand1)]
          [(set! ,rand1 (,prim ,rand1 ,rand2))
           (emit (prim->opcode prim) rand2 rand1)]
          [(set! ,rand1 ,rand2) (emit 'movq rand2 rand1)]
          [(if (not (,relop ,rand1 ,rand2)) (jump ,lab))
           (emit 'cmpq rand2 rand1)
           (emit-jump (relop->opcode relop #t) lab)]
          [(if (,relop ,rand1 ,rand2) (jump ,lab))
           (emit 'cmpq rand2 rand1)
           (emit-jump (relop->opcode relop #f) lab)]
          [,ef (error who "invalid Code syntax ~s" ef)])))
    (lambda (x)
      (match x
        [(code ,code* ...) (emit-program (for-each Code code*))]
        [,x (error who "invalid Program syntax ~s" x)])))

  )
