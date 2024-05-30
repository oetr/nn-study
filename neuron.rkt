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
   (value-add!* (cons (neuron-b n)
                      (for/list ([w lof-w]
                                 [x lof-x]
                                 [i (length lof-x)])
                        (value-mul! w (value-wrap x #:label (~a "x_" i))))))))


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
  (for/list ([n (layer-neurons a-layer)])
    (neuron-compute n lof-x)))


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
