(library (Compiler utils)
  (export
    hset?
    hset-init
    hset-push
    list->hset
    hset-union
    hset-intersection
    hset-difference
    hset->list
    hunion
    hintersection
    hdifference)
  (import
    (chezscheme)
    (Framework helpers)
    (Framework match))

  ;; hset related stuffs: a faster set implementation using hashtable

  ;; hset-dummy: a dummy value for hsets
  (define hset-dummy 'hukarz)

  ;; (hset? s): returns true iff s is a hashtable (i.e., contains unique items)
  (define (hset? s)
    (hashtable? s))

  ;; (hset-init): creates a new set. uses equal? for possible number/list comparisons.
  (define (hset-init)
    (make-hashtable equal-hash equal?))

  ;; (hset-push x hset): if x is not already in the set hset, then it will **mutate** hset
  ;; to add x and return hset; otherwise hset is returned.
  (define hset-push
    (lambda (x hset)
      (if (hset? hset)
          (begin
            (hashtable-cell hset x hset-dummy)
            hset)
          (errorf 'hset-push "not a hset: ~s" hset))))

  ;; (list->hset ls): returns a hset of all unique elements from ls.
  (define list->hset
    (lambda (ls)
      (let ([result (hset-init)])
        (begin
          (for-each (lambda (x) (hset-push x result)) ls)
          result))))

  ;; (hset-union hs1 hs2 ...): returns a new hset containing all the unique
  ;; items that appear in any of the hset arguments. no mutation on any arguments.
  (define hset-union
    (case-lambda
      [(hs1 hs2)
       (let ([result (hset-init)])
         (begin
           (vector-for-each
            (lambda (x) (hset-push x result))
            (hashtable-keys hs1))
           (vector-for-each
            (lambda (x) (hset-push x result))
            (hashtable-keys hs2))
           result))]
      [(hs1 . hss)
       (let ([result (hset-init)])
         (let loop ([hss hss])
           (cond
            [(null? hss)
             (begin
               (vector-for-each
                (lambda (x) (hset-push x result))
                (hashtable-keys hs1))
               result)]
            [else
             (begin
               (vector-for-each
                (lambda (x) (hset-push x result))
                (hashtable-keys (car hss)))
               (loop (cdr hss)))])))]))

  ;; (hset-intersection hs1 hs2 ...): returns a new hset containing all the
  ;; unique items that appear in all of the hset arguments. no mutation on any arguments.
  (define hset-intersection
    (case-lambda
      ([hs1 hs2]
       (let ([result (hset-init)])
         (begin
           (vector-for-each
            (lambda (x)
              (if (hashtable-contains? hs2 x)
                  (hset-push x result)
                  (void)))
            (hashtable-keys hs1))
           result)))
      ([hs1 . hss]
       (let ([result (hset-init)])
         (let loop ([hss hss])
           (cond
            [(null? hss) result]
            [else
             (begin
               (vector-for-each
                (lambda (x)
                  (if (andmap (lambda (hs) (hashtable-contains? hs x)) hss)
                      (hset-push x result)
                      (void)))
                (hashtable-keys hs1)))]))))))

  ;; (hset-difference hs1 hs2): returns a new hset containing all the
  ;; items from hs1 that were not in hs2. no mutation on any arguments.
  (define hset-difference
    (lambda (hs1 hs2)
      (let ([result (hset-init)])
        (begin
          (vector-for-each
           (lambda (x)
             (if (hashtable-contains? hs2 x)
                 (void)
                 (hset-push x result)))
           (hashtable-keys hs1))
          result))))

  ;; (hset->list hs): returns a list of all elements (keys) from hs.
  (define hset->list
    (lambda (hs)
      (vector->list (hashtable-keys hs))))

  ;; hunion, hintersection, and hdifference behave the same as union, intersection, and difference
  
  (define hunion
    (lambda args
      (hset->list
       (apply hset-union
        (map list->hset args)))))

  (define hintersection
    (lambda args
      (hset->list
       (apply hset-intersection
        (map list->hset args)))))
  
  (define hdifference
    (lambda args
      (hset->list
       (apply hset-difference
        (map list->hset args)))))

  )






#|
hset tests

(define h1 (hset-init))
(hset-push 'a h1)
(hset-push 'b h1)
(hset-push 'c h1)
(hset-push 'b h1)
(hset-push 128 h1)
(hset-push 256 h1)

(define h2 (hset-init))
(hset-push 'a h2)
(hset-push 'b h2)
(hset-push 'c h2)
(hset-push 'b h2)
(hset-push 128 h2)

(define h3 (hset-init))
(hset-push 'a h3)
(hset-push 'c h3)

(define h4 (hset-init))
(hset-push 12 h4)

(define h5 (hset-init))

(define (t hs) (hset->list hs))

|#
