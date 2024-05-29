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
  (neuron (for/list ([_ nof-inputs]) (make-value (sample uniform-1+1)))
          (make-value (sample uniform-1+1))))

;; computes w*x + b
(define (neuron-compute n lof-x)
  (define lof-w (neuron-w n))
  
  (unless (= (length lof-x) (length lof-w))
    (error 'neuron-compute "Unequal length. w: ~a, x: ~a~n" (length lof-w) (length lof-w)))

  (value-tanh!
   (value-add!* (cons (neuron-b n)
                      (for/list ([w lof-w]
                                 [x lof-x])
                        (value-mul! w x))))))


(struct layer (neurons))

(define (make-layer nof-inputs nout)
  (layer (for/list ([_ nout]) (make-neuron nof-inputs))))

(define (layer-compute a-layer lof-x)
  (for/list ([n (layer-neurons a-layer)])
    (neuron-compute n lof-x)))
