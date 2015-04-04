;; P423 Testing Framework

;; Copyright (C) 2011 Aaron W. Hsu {arcfide@sacrideo.us}
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adapated for P423 Spring 2012 by Claire Alvis and Chris Frisz
;;
;; This testing framework will run the tests found in (Framework
;; test-suite) over any compiler exported in (Compiler compile).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To run a test suite:
;; (test-valid)    runs all the valid tests
;; (test-invalid)  runs all the invalid tests
;; (test-all)      runs all the tests
;; (run-tests)
;;    runs the compiler in (test-compiler) over the tests in
;;    (test-suite) with a fresh test runner. if you've customized
;;    anything, use this to run your customizations.
;;
;; Debugging:
;; After you run a test suite, you may use the following procedures
;; to recover what went wrong with the tests that failed.
;;
;; (display-test-failure <test number>)
;;    returns a description of the test's error condition
;;
;; (inspect (test-failure-condition <test number>))
;;    this enters the scheme debugger with the error condition
;;    so you can inspect it
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizations:
;;
;; (test-suite <new-test-suite>)
;;    Creates a test-suite from the list of test cases specified by
;;    <new-test-suite>
;; (refine-test-suite <test-number>*)
;;    Reduces the current test suite to those specified by the
;;    <test-number>* set. Note that this causes original numbering to
;;    be lost.
;;
;; (test-compiler <new-test-compiler>)
;;    Specifies which compiler is to be used. This is generally
;;    defined by the (Compiler compile) library.
;;
;; (reset-test-runner)
;;    Resets the current test runner to a fresh test runner,
;;    discarding all the information from the previous run of the test
;;    suite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (Framework testing)
  (export
    ;; to run tests
    test-valid
    test-invalid
    test-all
    test-all-xml
    run-tests

    ;; to change the parameters
    test-compiler
    test-suite invalid-tests valid-tests invalid valid
    refine-test-suite
    refine-to-unexpected

    ;; debugging
    test-ref
    display-test-failure
    test-failure-condition)
  
  (import
    (chezscheme)
    (Compiler compile)
    (Framework driver))


;; Each test is labeled with whether it is expected to be a valid test
;; or an invalid one.

;; Defines the test-case record, along with a writer that prints
;; whether the test case is a valid test or not, and the test source.
(define-record test-case (valid? source))
#;
(record-writer (type-descriptor test-case)
  (lambda (r p wr)
    (begin
      (display "<" p)
      (if (test-case-valid? r)
          (display "valid test " p)
          (display "invalid test " p))
      (wr (test-case-source r) p)
      (display ">" p))))

;; Look up a given test in the test-suite by index, return its source code.
(define (test-ref suite num)
  (test-case-source (list-ref suite num)))

;; This automatically adds the test-case wrapper.
(define invalid-tests
  (make-parameter '()
    (lambda (x)
      (unless (and (list? x))
        (errorf 'invalid-tests "~s not a valid test suite" x))
      (map (lambda (t) (make-test-case #f t)) x))))

(define valid-tests
  (make-parameter '()
    (lambda (x)
      (unless (and (list? x))
        (errorf 'valid-tests "~s not a valid test suite" x))
      (map (lambda (t) (make-test-case #t t)) x))))

;; These are mutable parameters filled in by loading a separate file:
; (define invalid-tests (make-parameter 'uninitialized-invalid-tests-parameter))
; (define valid-tests (make-parameter 'uninitialized-valid-tests-parameter))

(define-syntax invalid
  (syntax-rules ()
    ((_ ts ...)
     (invalid-tests '(ts ...)))))

(define-syntax valid
  (syntax-rules ()
    ((_ ts ...)
     (valid-tests '(ts ...)))))


;; Record that defines the test runner. This records the number of
;; passed/failed tests cases, the result of the previous test, and an
;; entire test history.
(define-record test-runner
  (pass-expected pass-unexpected
   fail-expected fail-unexpected
   history))

(define (incr-field record-set! field)
  (lambda (runner)
    (record-set! runner (+ (field runner) 1))))
(define incr-pass-expected
  (incr-field set-test-runner-pass-expected! test-runner-pass-expected))
(define incr-pass-unexpected
  (incr-field set-test-runner-pass-unexpected! test-runner-pass-unexpected))
(define incr-fail-expected
  (incr-field set-test-runner-fail-expected! test-runner-fail-expected))
(define incr-fail-unexpected
  (incr-field set-test-runner-fail-unexpected! test-runner-fail-unexpected))
(define (add-to-history runner p)
  (set-test-runner-history! runner
    (cons p (test-runner-history runner))))

(define (fresh-test-runner)
  (make-test-runner 0 0 0 0 '()))
(define (reset-test-runner)
  (current-test-runner (fresh-test-runner)))
(define current-test-runner
  (make-parameter (fresh-test-runner)
    (lambda (x)
      (cond
        [(test-runner? x) x]
        [else (errorf 'current-test-runner
                "~s is not a valid test-runner" x)]))))

(define test-suite
  (make-parameter '()
    (lambda (x)
      (cond
        [(and (list? x) (for-all test-case? x)) x]
        [else (errorf 'test-suite
                "~s is not a valid test suite" x)]))))

(define test-compiler
  (make-parameter p423-compile
    (lambda (x)
      (cond
        [(procedure? x) x]
        [else (errorf 'test-compiler
                "~s is not a valid compiler" x)]))))

(define (refine-test-suite . num*)
  (let* ((suite (test-suite))
         (max-index (- (length suite) 1)))
    (let ((new-suite
            (map
              (lambda (n)
                (unless (<= 0 n max-index)
                  (errorf 'refine-test-suite
                    "Number ~s not a valid test index" n))
                (list-ref suite n))
              num*)))
      (begin
        (test-suite new-suite)
        (reset-test-runner)))))

(define (refine-to-unexpected)
  (let ((runner (current-test-runner)))
    (let ((unexpected
            (map car (filter cadr (test-runner-history runner)))))
      (apply refine-test-suite unexpected))))

(define (test-valid)
  (begin
    (test-suite (valid-tests))
    (run-tests)))

(define (test-invalid)
  (begin
    (test-suite (invalid-tests))
    (run-tests)))

(define (test-all)
  (begin
    (load "test-suite.ss")
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (vt (valid-tests))
          (it (invalid-tests)))
      (begin
        ;; Process the valid tests
        (test-suite vt)
        (printf "Testing (valid-tests)\n")
        (print-group-heading)
        (for-each (test-one compiler runner) (test-suite))

        ;; Process the invalid tests
        (test-suite it)
        (printf "\nTesting (invalid-tests)\n")
        (print-group-heading)
        (for-each (test-one compiler runner) (test-suite))

        ;; Finish up
	(test-suite (append vt it))
        (print-finalization runner)
        ))))

(define (test-all-xml)
  (begin
    (load "test-suite.ss")
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (vt (valid-tests))
          (it (invalid-tests)))
      (begin
        ;; Process the valid tests
        (test-suite vt)
        (printf "<test-group name=\"valid-tests\">\n")
        (for-each (test-one-xml compiler runner) (test-suite))
        (printf "</test-group>\n")

        ;; Process the invalid tests
        (test-suite it)
        (printf "<test-group name=\"invalid-tests\">\n")
        (for-each (test-one-xml compiler runner) (test-suite))
        (printf "</test-group>\n")

        ;; Finish up
        (test-suite (append vt it))

        ;; send and ending character so the autograder knows we are done. 
        ;; this is necessary because the scheme repl continues after the run
        ;; completes, therefore the subprocess from the autograder will hang
        ;; waiting for completion
        (printf "<EOF/>")
        ))))



;; Runs the compiler in (test-compiler) over the tests in (test-suite)
;; with a fresh test runner. If you've customized anything, use this
;; to run your customizations.
(define (run-tests)
  (begin
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (suite (test-suite)))
      (begin 
        (print-group-heading)
        (for-each (test-one compiler runner) suite)
        (print-finalization runner)))))

;; This prints out the information for a single test.
;; Don't use test-one to run the compiler on a single program,
;; just call the current compiler on that program.
(define (test-one compiler runner)
  (lambda (input)
    (let ((pr (guard (x [else x])
                     (compiler (test-case-source input))))
          (expected-failure
            (not (test-case-valid? input))))
      (begin
        (print-individual-completion pr runner)
        (record-test-result pr expected-failure runner)))))

;; This prints out the information for a single test.
;; Don't use test-one to run the compiler on a single program,
;; just call the current compiler on that program.
(define (test-one-xml compiler runner)
  (lambda (input)
    (let ((pr (guard (x [else x])
                     (compiler (test-case-source input))))
          (expected-failure
            (not (test-case-valid? input))))
      (begin
        (print-individual-completion-xml pr runner (if expected-failure "fail" "pass"))
        (record-test-result pr expected-failure runner)))))

(define (print-group-heading)
  (printf "Test~8,8tResult\n")
  (printf "---------------------------~n"))

;; Prints an individual test completion.
;; Example output:
;;
;;    1    Pass    
;;    2    Fail    Pass: PASS-NAME
;;    3    Fail    Runtime error (in who)
;; ...
(define (print-individual-completion pr runner)
  (apply printf "~4d    ~8a~a~n"
         (current-test-number runner)
         (result->string pr)))

;; Prints an individual test completion.
;; Example output:
;;    
;;    <test-result name="test-name" result="pass/fail" expected="pass/fail" />
;;    
(define (print-individual-completion-xml pr runner expected)
  (printf "<test-result name=\"test-~s\" result=~s expected=~s />\n"
          (current-test-number runner)
          (result->pass/fail pr)
          expected))

(define (result->string pass-result)
  (cond
    [(wrapper-violation? pass-result)
     (list "Fail" "Wrapper violation")]
    [(pass-verification-violation? pass-result)
     (list "Fail"
           (format "~a: ~s"
                   "Wrapper answer verification error"
                   (pass-verification-violation-pass pass-result)))]
    [(or (error? pass-result) (violation? pass-result))
     (list "Fail" 
           (if (who-condition? pass-result)
             (format "Runtime error in ~s" (condition-who pass-result))
             "Runtime error"))]
    [else (list "Pass" "")]))

(define (result->pass/fail pass-result)
  (cond
    [(wrapper-violation? pass-result)
     "wrapper violation"]
    [(pass-verification-violation? pass-result)
     (format "~a: ~s"
       "wrapper answer verification error"
       (pass-verification-violation-pass pass-result))]
    [(or (error? pass-result) (violation? pass-result))
     "fail"]
    [else "pass"]))

;; Prints a final summary for the testing.
;; Example output:
;;
;; Testing Summary
;; ----------------------------
;; Expected Passess:        100
;; Unexpected Passes:        10
;; Expected Failures:        20
;; Unexpected Failures:      25
;; Total:                   155
(define (print-finalization runner)
  (let ((pass-expected   (test-runner-pass-expected   runner))
        (pass-unexpected (test-runner-pass-unexpected runner))
        (fail-expected   (test-runner-fail-expected   runner))
        (fail-unexpected (test-runner-fail-unexpected runner)))
    (printf "~nTesting Summary~n")
    (printf "~a~n" (make-string 28 #\-))
    (printf "Expected Passes:~24,8t~4d~n"      pass-expected)
    (printf "Unexpected Passes:~24,8t~4d~n"    pass-unexpected)
    (printf "Expected Failures:~24,8t~4d~n"    fail-expected)
    (printf "Unexpected Failures: ~24,8t~4d~n" fail-unexpected)
    (printf "Total:~24,8t~4d~n"
            (+ pass-expected pass-unexpected
               fail-expected fail-unexpected))
    (zero? (+ fail-unexpected pass-unexpected))))


;; Calculates the current test number
(define (current-test-number runner)
  (+ (test-runner-pass-expected   runner)
     (test-runner-pass-unexpected runner)
     (test-runner-fail-expected   runner)
     (test-runner-fail-unexpected runner)))

;; This records the result of the previous test, whether it be an
;; expected pass (just increments test-runner-pass-expected), or
;; failed (increments test-runner-failed-[un]expected and stores the
;; error condition in the history). also store the unexpected-passes.
(define (record-test-result pass-result expected-fail runner)
  (cond
    ((or (error? pass-result)
         (wrapper-violation? pass-result)
         (pass-verification-violation? pass-result)
         (violation? pass-result))
     (begin
       (add-to-history runner
                       (cons (current-test-number runner)
                             ;; If the failure was expected, store #f. otherwise #t
                             (cons (not expected-fail) pass-result)))
       (if expected-fail
         ;; An expected failure
         (incr-fail-expected runner)
         ;; An unexpected failure
         (incr-fail-unexpected runner))))
    (else
      (if expected-fail
        ;; An unexpected pass
        ;; Since this is an unexpected result, store #t.
        (begin
          (add-to-history runner
                          (cons (current-test-number runner)
                                (cons #t (void))))
          (incr-pass-unexpected runner))
        ;; An expected pass
        (incr-pass-expected runner)))))

(define (display-test-failure test-num)
  (let ([result (test-failure-condition test-num)])
    (when result
      (cond
        [(pass-verification-violation? result)
         (display-pass-verification-violation result)]
        [(wrapper-violation? result)
         (printf "Error in wrapper ~a:~n"
                 (wrapper-violation-name result))
         (display-condition result)]
        [else (display-condition result)])
      (newline))))

(define (test-failure-condition test-num)
  (let ((runner (current-test-runner)))
    (let ([result (assv test-num (test-runner-history runner))])
      (and result (cddr result)))))

)

