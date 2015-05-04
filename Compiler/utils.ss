(library (Compiler utils)
  (export
    graphq-add
    graphq-get-arcs
    graphq-dfs
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

  ;; graph related stuffs. just for fun.

  ;; a better union for lists
  (define (unionq ls1 ls2)
    (let loop ([ls1 ls1] [ls2 ls2] [acc ls2])
      (cond
       [(null? ls2) ls1]
       [(null? ls1) acc]
       [else (loop (cdr ls1)
                   ls2
                   (if (memq (car ls1) acc)
                       acc
                       (cons (car ls1) acc)))])))

  ;; graphq-add: adds a node and its neighbors to a graph
  (define (graphq-add node arcs graph)
    (cond
     [(assq node graph)
      (let loop ([node node] [arcs arcs] [graph graph] [acc '()])
        (cond
         [(null? graph) acc]
         [(eq? node (caar graph))
          (loop node arcs (cdr graph) (cons `(,node . ,(unionq arcs (cdar graph))) acc))]
         [else (loop node arcs (cdr graph) (cons (car graph) acc))]))]
     [else (cons `(,node . ,arcs) graph)]))

  ;; graphq-get-arcs: gets a node's neighbors, if there are. otherwise #f
  (define (graphq-get-arcs node graph)
    (cond
     [(assq node graph) => cdr]
     [else #f]))

  ;; graphq-dfs: gets a node's neighbors. dfs.
  (define (graphq-dfs node graph)
    (let loop ([stack `(,node)] [acc '()] [visited '()])
      (cond
       [(null? stack) acc]
       [(graphq-get-arcs (car stack) graph) =>
        (lambda (ls)
          (if (memq (car stack) visited)
              (loop (cdr stack) acc visited) ;; visited, do nothing
              (loop (append ls (cdr stack)) acc (cons (car stack) visited))))]
       [else
        (if (memq (car stack) visited)
            (loop (cdr stack) acc visited) ;; visited, do nothing
            (loop (cdr stack) (cons (car stack) acc) (cons (car stack) visited)))])))

  
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
              (when (hashtable-contains? hs2 x)
                (hset-push x result)))
            (hashtable-keys hs1))
           result)))
      ([hs1 . hss]
       (let ([result (hset-init)])
         (cond
          [(null? hss) result]
          [else
           (begin 
             (vector-for-each
              (lambda (x)
                (when (andmap (lambda (hs) (hashtable-contains? hs x)) hss)
                  (hset-push x result)))
              (hashtable-keys hs1))
             result)])))))

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
      (let ([result (hset-init)])
        (for-each (lambda (ls)
                    (for-each (lambda (x)
                                (unless (hashtable-contains? result x)
                                  (hset-push x result)))
                              ls))
                  args)
        (hset->list result))))

  (define hintersection
    (lambda args
      (if (null? args)
          '()
          (hset->list
           (apply hset-intersection
                  (map list->hset args))))))
  
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
