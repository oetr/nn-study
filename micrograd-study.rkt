#lang racket

(require "micrograd.rkt"
         "dot.rkt")

;; first example
(when #f
  (define a (make-value 2.0 #:label "a"))
  (define b (make-value -3.0 #:label "b"))
  (define c (make-value 10.0 #:label "c"))
  (define e (value-mul! a b #:label "e"))
  (define d (value-add! e c #:label "d"))
  (define f (make-value -2.0 #:label "f"))
  (define L (value-mul! d f #:label "L"))
  (backward! L)
  (draw-dot L))

;; first neuron
(when #t
  ;; inputs x1, x2
  (define x1 (make-value 2.0 #:label "x1"))
  (define x2 (make-value 0.0 #:label "x2"))
  ;; weights w1, w2
  (define w1 (make-value -3.0 #:label "w1"))
  (define w2 (make-value 1.0 #:label "w2"))
  ;; bias of the neuron
  (define b (make-value 6.8813735870195432 #:label "b"))
  ;; 
  (define x1*w1 (value-mul! x1 w1 #:label "x1*w1"))
  (define x2*w2 (value-mul! x2 w2 #:label "x2*w2"))
  (define x1*w1+x2*w2 (value-add! x1*w1 x2*w2 #:label "x1*w1+x2*w2"))
  (define n (value-add! x1*w1+x2*w2 b #:label "n"))

  (define o (value-tanh! n #:label "o"))
  
  (backward! o)
  (draw-dot o))
