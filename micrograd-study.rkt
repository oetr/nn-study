#lang racket

(require "./micrograd.rkt")
(require "./graph.rkt")

;; first example
(let ()
  (define a (make-value 2.0 #:label "a"))
  (define b (make-value -3.0 #:label "b"))
  (define c (make-value 10.0 #:label "c"))
  (define e (value-mul! a b)) (set-value-label! e "e")
  (define d (value-add! e c)) (set-value-label! d "d")
  (define f (make-value -2.0 #:label "f"))
  (define L (value-mul! d f)) (set-value-label! L "L")
  (backward! L)
  (make-graph L))

