#lang racket
(require racket/draw
         "./micrograd.rkt")

(provide (all-defined-out))

(define (value->graphviz v)
  (define (traverse-value start)
    (define (traverse-aux seen edges val)
      (define val-hash (eq-hash-code val))
      (define children (value-prev val))
      (cond [(set-empty? children) (values (set-add seen val) edges)]
            [else
             (for/fold ([seen seen]
                        [edges edges])
                       ([child children])
               (traverse-aux (set-add seen val)
                             (cons (~a (eq-hash-code child) " -> \"" val-hash (value-op val) "\";") edges)
                             child))]))
    
    (define-values (seen edges) (traverse-aux (set) (list) start))
    
    (append* edges (for/list ([val seen])
                     (cons (~a (eq-hash-code val)
                               " [label = \"" (value-label val) "| data " (value-data val) "| grad "
                               (value-grad val)
                               "\" shape=\"record\" ];")
                           ;; operation node
                           (if (not (string=? "" (value-op val)))
                               ;; make a new node (op -> after-op)
                               (let ([id (~a "\""(eq-hash-code val) (value-op val)"\"")])
                                 (list 
                                  (~a id " [label=\"" (value-op val) "\"];")
                                  (~a id " -> " (eq-hash-code val) ";")))
                               empty)))))
  
    (define graph (append (cons "digraph {"
                                (traverse-value v))
                          (list "}")))
    (define fold (foldr string-append "" graph))
  fold)

  
(define (make-graph v)
  (parameterize ([current-custodian (make-custodian)])
    (let ([p-stdin (open-input-string (value->graphviz v))]
          [out (open-output-bytes)]
          [err (open-output-bytes)])
      (define proc-data (process/ports out p-stdin err "dot -Tpng -Gdpi=100 -Gsize=5,5!"))
      (define control (list-ref proc-data 4))
      (control 'wait)
      (define errors (get-output-string err))
      (if (string=? "" errors)
          (make-object bitmap% (open-input-bytes (get-output-bytes out)))
          (printf "ERROR: ~a~n" errors)))))
