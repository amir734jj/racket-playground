#lang racket

;;; factorial function
(define (fact n)
  (cond
    ((= n 0) 1)
    (true (* n (fact (- n 1))))
  )
)

;;; fibuncacci function
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (true (+ (fib (- n 1)) (fib (- n 2))))
  )
)

;;; binary tree struct
(struct bst-node (val left right) #:transparent #:mutable)

;;; add to binary tree
(define (bst-add tree value)
  (if (null? tree) (bst-node value null null)
      (let ((x (bst-node-val tree)))
        (cond
          ((= value x) tree)
          ((< value x) (if (null? (bst-node-left tree))
                       (set-bst-node-left! tree (bst-node value null null))
                       (bst-add (bst-node-left tree) value)))
          ((> value x) (if (null? (bst-node-right tree))
                       (set-bst-node-right! tree (bst-node value null null))
                       (bst-add (bst-node-right tree) value))))
        tree)))

;;; function that check whether node does not have any child
(define (childless? node)
  (and (null? (bst-node-left node))
       (null? (bst-node-right node))))

;;; function that checks whether ndoe has a left child only
(define (left-child-only? node)
  (and (not (null? (bst-node-left node)))
       (null? (bst-node-right node))))

;;; function that checks whether ndoe has a left child only
(define (right-child-only? node)
  (and (null? (bst-node-left node))
       (not  (null? (bst-node-right node)))))
       
;;; function that check if value is in tree
(define (bst-in? b n)
  (if (null? b) #f
      (let ([x (bst-node-val b)])
        (cond
          [(= n x) #t]
          [(< n x) (bst-in? (bst-node-left b) n)]
          [(> n x) (bst-in? (bst-node-right b) n)]))))

;;; function that checks if tree is indeed BST tree
(define (bst? b)
  (define (between? b low high)
    (or (null? b)
        (and (<= low (bst-node-val b) high)
             (between? (bst-node-left b) low (bst-node-val b))
             (between? (bst-node-right b) (bst-node-val b) high))))
  (between? b -inf.0 +inf.0))
  
;;; function that deletes a value from tree
(define (bst-del tree value)
    (define (get-min-value tree)
      (cond
        [(null? (bst-node-left tree)) (bst-node-val tree)]
        [else (get-min-value (bst-node-left tree))]))
  
    (define (remove cur-node parent setter)
      (cond
        [(childless? cur-node) (setter parent null)]
        [(left-child-only? cur-node) (if (null? parent)
                                         (bst-node-left cur-node)
                                         (setter parent (bst-node-left cur-node)))]
        [(right-child-only? cur-node) (if (null? parent)
                                          (bst-node-right cur-node)
                                          (setter parent (bst-node-right cur-node)))]
        [else (let ([min (get-min-value (bst-node-right cur-node))])
                (set-bst-node-val! cur-node min)
                (start-removing (bst-node-right cur-node)
                                min
                                cur-node
                                set-bst-node-right!))]))
    
    (define (find-and-remove cur-node value parent setter)
      (let* ([x (bst-node-val cur-node)]
             [result
              (cond
                [(= value x) (remove cur-node parent setter)]
                [(< value x) (find-and-remove (bst-node-left cur-node) value cur-node set-bst-node-left!)]
                [(> value x) (find-and-remove (bst-node-right cur-node) value cur-node set-bst-node-right!)])])
        (if (void? result) tree result)))
  
    (define (start-removing tree value [parent null] [setter null])
      (if (bst-in? tree value)
          (find-and-remove tree value parent setter)
          tree))
  
    (start-removing tree value))
    
(define (list->bst l)
  (define (convert l) (cond
    [(= (length l) 1) (bst-node (first l) null null)]
    [(= (length l) 2)
     (if (list? (first l))
         (bst-node (second l) (list->bst (first l)) null)
         (bst-node (first l) null (list->bst (second l)) ))]
    [(= (length l) 3)
     (bst-node (second l)
               (list->bst (first l))
               (list->bst (third l)))]))
  (let ([b (convert l)])
    (if (bst? b) b
        (raise-argument-error 'list->bst "bst?" b))))

(bst-add (bst-add null 3) 2)