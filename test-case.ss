(letrec ([len$0 (lambda (ls.1)
		     (locals (size.2)
			     (begin
			       (set! size.2 (mref ls.1 0))
			       size.2)))])
  (locals (ls.1)
	  (begin
	    (set! ls.1 (alloc 12))
	    (len$0 ls.1))))
