#lang racket

(require racket/struct)

(provide (all-defined-out))

;; Value
(struct value (data grad backward prev op label)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj)
        (cond [(not (string=? "" (value-op obj))) (~a 'value: " " (value-op obj))]
              [(not (string=? "" (value-label obj))) (~a 'value: " " (value-label obj))]
              [else 'value]))
      (lambda (obj) (list (list 'data (value-data obj))
                          (list 'grad (value-grad obj))
                          ))))])


(define (grad+! val add)
  (set-value-grad! val (+ (value-grad val) add)))


(define (make-value data (children '()) #:op (op "") #:label (label ""))
  (value data 0.0 void (apply set children) op label))


(define (value-wrap val)
  (if (number? val)
      (make-value val)
      val))


(define (value-add! val1 val2)
  (define other (value-wrap val2))
  
  (define out (make-value (+ (value-data val1)
                             (value-data other))
                          (list val1 other)
                          #:op "+"))
  
  (define (backward)
    (grad+! val1 (value-grad out))
    (grad+! other (value-grad out)))

  (set-value-backward! out backward)
  out)


(define (value-mul! val1 val2)
  (define other (value-wrap val2))

  (define out (make-value (* (value-data val1)
                             (value-data other))
                          (list val1 other)
                          #:op "*"))

  (define (backward)
    (grad+! val1 (* (value-data other) (value-grad out)))
    (grad+! other (* (value-data val1) (value-grad out))))

  (set-value-backward! out backward)
  out)


(define (backward! v)
  (define visited (mutable-set))
  (define topo '())
  (define (build-topo v)
    (unless (set-member? visited v)
      (set-add! visited v)
      (for ([child (value-prev v)])
        (build-topo child))
      (set! topo (cons v topo))))
  (build-topo v)
  (set-value-grad! v 1.0)
  (for-each (lambda (v) ((value-backward v)))
            topo))