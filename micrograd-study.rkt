#lang racket

(require "micrograd.rkt"
         "dot.rkt"
         "neuron.rkt")

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

;; manual neuron
(when #f
  ;; inputs x1, x2
  (define-value x1 2.0)
  (define-value x2 0.0)
  ;; weights w1, w2
  (define-value w1 -3.0)
  (define-value w2 1.0)
  ;; bias of the neuron
  (define-value b 6.8813735870195432)
  ;; x1*w1 + x2*w2 + b
  (define-value* x1*w1 x1 w1)
  (define-value* x2*w2 x2 w2)
  (define-value+ x1*w1+x2*w2 x1*w1 x2*w2)
  (define-value+ n x1*w1+x2*w2 b)
  (define-value-tanh o n)
  (backward! o)
  (draw-dot o))

;; first neuron
(when #f
  (define n (make-neuron 10))
  (define out (neuron-compute n (range 10 20)))
  (backward! out)
  (draw-dot out #:dpi 100 #:type "pdf" #:path "/mnt/ramdisk/out.pdf"))


;; first layer
(when #f
  (define l (make-layer 2 10))
  (define out (layer-compute l (range 1 3)))
  (backward! out)
  (draw-dot out #:dpi 100 #:type "pdf" #:path "/mnt/ramdisk/layer.pdf"))

;; first MLP
(when #f
  (define mlp (make-MLP 2 (list 10 10 10 1)))
  (define out (MLP-compute mlp (range 1 3)))
  (backward! out)
  (draw-dot out #:dpi 100 #:type "pdf" #:path "/mnt/ramdisk/MLP.pdf"))

;; computing loss
(when #f
  (define mlp (make-MLP 3 (list 4 4 1)))
  (define xs (list (list 2.0 3.0 -1.0)
                   (list 3.0 -1.0 0.5)
                   (list 0.5 1.0 1.0)
                   (list 1.0 1.0 -1.0)))
  (define ys (list 1.0 -1.0 -1.0 1.0))
  (define y-pred (for/list ([x xs]) (MLP-compute mlp x)))
  y-pred
  (define loss (mse-loss ys y-pred)) (set-value-label! loss "loss")
  (backward! loss)
  (define parameters (MLP-parameters mlp))
  (print (length parameters))
  (draw-dot loss #:dpi 100 #:type "pdf" #:path "/mnt/ramdisk/loss.pdf")
  )

;; training
(when #t
  (define mlp (make-MLP 3 (list 4 4 1)))
  (define xs (list (list 2.0 3.0 -1.0)
                   (list 3.0 -1.0 0.5)
                   (list 0.5 1.0 1.0)
                   (list 1.0 1.0 -1.0)))
  (define ys (list 1.0 -1.0 -1.0 1.0))
  (MLP-train mlp xs ys 20 #:learning-rate 0.2)
  (define y-pred (for/list ([x xs]) (MLP-compute mlp x)))
  (print (map value-data y-pred))
  (draw-dot y-pred #:dpi 100 #:type "pdf" #:path "/mnt/ramdisk/training.pdf"))
