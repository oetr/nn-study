;; making a multinomial-dist because it would be too easy to use Racket's non-existent native alternative
#lang racket

;; TODO: benchmark
(#%declare #:unsafe)

(require math/flonum
         math/distributions
         racket/unsafe/ops
         racket/performance-hint)

(define (multinomial-pdf a b x log?)
  (cond [log?  (cond [(and (x . fl>= . a) (x . fl<= . b))  (- (fllog (fl- b a)))]
                     [else  -inf.0])]
        [else  (cond [(and (x . fl>= . a) (x . fl<= . b))  (fl/ 1.0 (fl- b a))]
                     [else  0.0])]))

;; id is index of from in the original array
(struct interval (from to left right id) #:mutable #:transparent)

(define (insert! tree new-interval)
  (if (< (interval-from new-interval) (interval-from tree))
      (let ([left (interval-left tree)])
        (if (empty? left)
            (set-interval-left! tree new-interval)
            (insert! left new-interval)))
      (let ([right (interval-right tree)])
        (if (empty? right)
            (set-interval-right! tree new-interval)
            (insert! right new-interval)))))

(define (get-id tree n)
  (cond [(empty? tree) (error 'get-id "no fitting interval found for ~a" n)]
        [(< n (interval-from tree))
         (get-id (interval-left tree) n)]
        [(>= n (interval-to tree))
         (get-id (interval-right tree) n)]
        [else (interval-id tree)]))

(define (make-interval-tree vals)
  ;; make intervals
  (define intervals
    (sort
     (let loop ([from 0.0][to-i 0])
       (cond [(>= to-i (flvector-length vals)) empty]
             [else
              (define to (flvector-ref vals to-i))
              (cons (interval from to empty empty to-i)
                    (loop to (+ 1 to-i)))]))
    (lambda (x y)
      (> (- (interval-to x) (interval-from x))
         (- (interval-to y) (interval-from y))))))

  (define tree (car intervals))
  (for ([interval (cdr intervals)])
    (insert! tree interval))
  tree
  )


(define (multinomial-dist lof-p)
  ;; normalize
  (define sum (fl (apply + lof-p)))
  (define normalized
    (list->flvector
     (reverse
      (let loop ([s 0.0] [acc empty] [lof-p lof-p])
        (cond ((empty? lof-p) acc)
              (else
               (define n (car lof-p))
               (define to (+ s (/ n sum)))
               (loop to (cons to acc) (cdr lof-p))))))))
  (define tree (make-interval-tree normalized))
  ;; TODO: make a proper PDF
  (define my-dummy-pdf (lambda (x [log? #f])
                         (multinomial-pdf 0.0 1.0 (fl x) log?)))
  (define my-sample (case-lambda
                      [()  (get-id tree (unsafe-flvector-ref (fluniform-sample 0.0 1.0 1) 0))]
                      [(n)  (for/list ([random-n (fluniform-sample 0.0 1.0 n)])
                              (get-id tree random-n))]))
  (distribution my-dummy-pdf my-sample))


;; TODO: test properly, the first tests seem off...
;;(sample (multinomial-dist '(1 2 3 100 200)) 10000)
