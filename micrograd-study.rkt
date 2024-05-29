#lang racket

(require "micrograd.rkt"
         "dot.rkt")

;; first example
(when #f
  (define-value a 2.0)
  (define-value b -3.0)
  (define-value c 10.0)
  (define-value* e a b)
  (define-value+ d e c)
  (define-value f -2.0)
  (define-value* L d f)
  (backward! L)
  (draw-dot L))

;; first neuron
(when #t
  ;; inputs x1, x2
  (define-value x1 2.0)
  (define-value x2 0.0)
  ;; weights w1, w2
  (define-value w1 -3.0)
  (define-value w2 1.0)
  ;; bias of the neuron
  (define-value b 6.8813735870195432)
  ;; 
  (define-value* x1*w1 x1 w1)
  (define-value* x2*w2 x2 w2)
  (define-value+ x1*w1+x2*w2 x1*w1 x2*w2)
  (define-value+ n x1*w1+x2*w2 b)

  (define o (value-tanh! n #:label "o"))
  
  (backward! o)
  (draw-dot o))
