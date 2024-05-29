;; inspired by https://github.com/karpathy/micrograd
#lang racket

(require racket/struct
         syntax/parse/define)

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


(define (value-add! val1 val2 #:label (label ""))
  (define w1 (value-wrap val1))
  (define w2 (value-wrap val2))
  
  (define out (make-value (+ (value-data w1)
                             (value-data w2))
                          (list w1 w2)
                          #:op "+"
                          #:label label))
  
  (define (backward)
    (grad+! w1 (value-grad out))
    (grad+! w2 (value-grad out)))

  (set-value-backward! out backward)
  out)

(define (value-add!* vals #:label (label ""))
  (define wraps (map value-wrap vals))
  (define out (make-value (foldl (lambda (v result)
                                   (+ result (value-data v)))
                                 0.0 wraps)
                          wraps
                          #:op "+"
                          #:label label))

  (define (backward)
    (for-each (lambda (v) (grad+! v (value-grad out)))
              wraps))

  (set-value-backward! out backward)
  out)


(define (value-mul! val1 val2 #:label (label ""))
  (define w1 (value-wrap val1))
  (define w2 (value-wrap val2))

  (define out (make-value (* (value-data w1)
                             (value-data w2))
                          (list w1 w2)
                          #:op "*"
                          #:label label))

  (define (backward)
    (grad+! w1 (* (value-data w2) (value-grad out)))
    (grad+! w2 (* (value-data val1) (value-grad out))))

  (set-value-backward! out backward)
  out)


(define (value-tanh! val1 #:label (label ""))
  (define w1 (value-wrap val1))
  (define e^2x (exp (* 2 (value-data w1))))
  (define t (/ (+ -1 e^2x)
               (+  1 e^2x)))
  
  (define out (make-value t (list w1) #:op "tanh" #:label label))
  
  (define (backward)
    (grad+! w1 (* (- 1 (sqr t)) (value-grad out))))
  
  (set-value-backward! out backward)
  out)

(define (backward! v)
  (define visited (mutable-set))
  (define topo '())
  (define (build-topo! v)
    (unless (set-member? visited v)
      (set-add! visited v)
      (for ([child (value-prev v)])
        (build-topo! child))
      (set! topo (cons v topo))))

  (define wrapped-v (if (list? v) v (list v)))
  (build-topo! (make-value 0.0 wrapped-v))
  
  (map (lambda (v) (set-value-grad! v 1.0)) wrapped-v)
  (for-each (lambda (v) ((value-backward v)))
            topo))


(define-syntax (define-value stx)
  (syntax-parse stx
    [(_ name:id data (~optional (~seq #:op op:str)))
     #'(define name (make-value data (~? (~@ #:op op)) #:label (symbol->string 'name)))]
    [(_ name:id data children (~optional (~seq #:op op:str)))
     #'(define name (make-value data children (~? (~@ #:op op)) #:label (symbol->string 'name)))]))

(define-syntax (define-value* stx)
  (syntax-parse stx
    [(_ name:id a b)
     #'(define name (value-mul! a b  #:label (symbol->string 'name)))]))

(define-syntax (define-value+ stx)
  (syntax-parse stx
    [(_ name:id a b)
     #'(define name (value-add! a b #:label (symbol->string 'name)))]))

(define-syntax (define-value-tanh stx)
  (syntax-parse stx
    [(_ name:id a)
     #'(define name (value-tanh! a #:label (symbol->string 'name)))]))

