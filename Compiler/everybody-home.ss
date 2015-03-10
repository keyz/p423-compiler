(library (Compiler everybody-home)
  (export everybody-home?)
  (import
    (chezscheme)
    (Framework match)
    (Framework helpers))

  (define-who everybody-home?

    (define all-home?
      (lambda (body)
        (match body
          [(locals (,local* ...)
             (ulocals (,ulocal* ...)
               (spills (,spill* ...)
                 (locate (,home* ...)
                   (frame-conflict ,ct ,tail))))) #f]
          [(locate (,home* ...) ,tail) #t]
          [,x (error who "invalid Body ~s" x)])))
    
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,body*)] ...) ,body)
         (andmap all-home? `(,body ,body* ...))]
        [,x (error who "invalid Program ~s" x)])))

  )
