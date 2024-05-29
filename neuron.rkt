#lang racket

(require racket/struct
         math/distributions
         "micrograd.rkt"
         "dot.rkt")

(provide (all-defined-out))

(struct neuron (w b)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'neuron)
      (lambda (obj) (list (list 'w (neuron-w obj))
                          (list 'b (neuron-b obj))
                          ))))])


(define (make-neuron nin)
  (define uniform-1+1 (uniform-dist -1.0 1.0))
  (neuron (for/list ([i nin]) (make-value (sample uniform-1+1)))
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

