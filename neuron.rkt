#lang racket

(require racket/struct
         math/distributions
         "micrograd.rkt")

(provide (all-defined-out))

(struct neuron (w b)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'neuron)
      (lambda (obj) (list (list 'w (neuron-w obj))
                          (list 'b (neuron-b obj))
                          ))))])


(define (make-neuron nof-inputs)
  (define uniform-1+1 (uniform-dist -1.0 1.0))
  (neuron (for/list ([i nof-inputs]) (make-value (sample uniform-1+1) #:label (~a "w_" i)))
          (make-value (sample uniform-1+1) #:label "b")))

;; computes w*x + b
(define (neuron-compute n lof-x)
  (define lof-w (neuron-w n))
  
  (unless (= (length lof-x) (length lof-w))
    (error 'neuron-compute "Unequal length. w: ~a, x: ~a~n" (length lof-w) (length lof-x)))

  (value-tanh!
   (values+! (cons (neuron-b n)
                   (for/list ([w lof-w]
                              [x lof-x]
                              [i (length lof-x)])
                     (value*! w (value-wrap x #:label (~a "x_" i))))))))

(define (neuron-parameters n)
  (append (neuron-w n) (list (neuron-b n))))



(struct layer (neurons)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'layer)
      (lambda (obj)
        (define neurons (layer-neurons obj))
        (list (length (neuron-w (first neurons)))
              'x
              (length neurons)))))])


(define (make-layer nof-inputs nof-outputs)
  (layer (for/list ([_ nof-outputs]) (make-neuron nof-inputs))))

(define (layer-compute a-layer lof-x)
  (define labeled-lof-x
    (for/list ([x lof-x]
               [i (length lof-x)])
      (value-wrap x #:label (~a "x_" i))))
  (define result (for/list ([n (layer-neurons a-layer)])
                   (neuron-compute n labeled-lof-x)))
  (if (= 1 (length result))
      (first result)
      result))

(define (layer-parameters layer)
  (flatten (map neuron-parameters (layer-neurons layer))))

(struct MLP (layers)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'MLP)
      (lambda (obj) (MLP-layers obj))))])

(define (make-MLP nof-inputs lof-n-outputs)
  (MLP (for/list ([n-inputs (cons nof-inputs lof-n-outputs)]
                  [n-outputs lof-n-outputs])
         (make-layer n-inputs n-outputs))))


(define (MLP-compute mlp lof-x)
  (for/fold ([x lof-x])
            ([layer (MLP-layers mlp)])
    (layer-compute layer x)))

(define (MLP-parameters mlp)
  (flatten (map layer-parameters (MLP-layers mlp))))

(define (mse-loss ygt yout)
  (values+! 
   (for/list ([y ygt]
              [actual yout]
              [i (length ygt)])
     (value-expt! (value-! actual y #:label (~a "y_" i))
                  2))))


(define (MLP-train mlp xs ygt n
                   #:learning-rate (learning-rate 0.01))
  (define parameters (MLP-parameters mlp))
  (for ([i n])
    ;; forward pass
    (define y-pred (for/list ([x xs]) (MLP-compute mlp x)))
    (define loss (mse-loss ygt y-pred))

    ;; zero grads
    (for ([p parameters]) (set-value-grad! p 0.0))

    ;; backward pass
    (backward! loss)

    ;; update
    (for ([p parameters])
      (define new-data (+ (value-data p)
                          (* -1.0 learning-rate (value-grad p))))
      (set-value-data! p new-data))

    (printf "~a: loss: ~a~n" i (value-data loss))))

(module+ test
  (require rackunit)
  (define EPSILON 0.0000001)

  (define (check-equal proc value expected (message ""))
    (andmap (lambda (v expected)
            (check-within (proc v) expected EPSILON
                          message))
          value expected))
  
  (define t0 (map make-value (range 1 6)))
  (define t1 (map make-value (range 1 6)))
  (define t2 (map value*! t0 t1))
  (define t3 (map value-tanh! t2))
  (define result (values+! t3))
  (backward! result)

  (define t0-expected-grad (list 4.1997434161e-01 2.6819013661e-03 1.8275975121e-07 2.0250467969e-13
                                 0.0000000000e+00))
  (check-equal value-grad t0 t0-expected-grad "t0 grad not equal")

  (define t2-expected-data (list 1.0 4.0 9.0 16.0 25.0))
  (check-equal value-data t2 t2-expected-data "t2 data not equal")

  (define t2-expected-grad
    (list 4.1997434161e-01 1.3409506830e-03 6.0919917071e-08 5.0626169923e-14
          0.0000000000e+00))
  (check-equal value-grad t2 t2-expected-grad "t2 grad not equal")
  
  
  (define t3-expected-data (list 0.7615941560 0.9993292997 0.9999999695 1.0000000000 1.0000000000))
  (check-equal value-data t3 t3-expected-data "t3 data not equal")
  (define t3-expected-grad (list 1.0 1.0 1.0 1.0 1.0))
  (check-equal value-grad t3 t3-expected-grad "t3 grad not equal")
  
  )


