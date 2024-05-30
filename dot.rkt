#lang racket

(require racket/draw
         "micrograd.rkt")

(provide (all-defined-out))

(define (value->dot v)

  (struct edge (from to)
    #:methods
    gen:equal+hash
    [(define (equal-proc a b equal?-recur)
       ;; two edges are equal if they connect the same two value structs
       (and (equal?-recur (edge-from a) (edge-from b))
            (equal?-recur (edge-to a) (edge-to b))))
     (define (hash-proc edge hash-recur)
       ;; primary hash code
       (+ (hash-recur (edge-from edge))
          (* 3 (hash-recur (edge-to edge)))))
     ;; secondary hash code
     (define (hash2-proc edge hash2-recur)
       (+ (hash2-recur (edge-from edge))
          (hash2-recur (edge-to edge))))])

  (define (traverse-value start)
    (define (extract-children-and-edges seen edges val)
      (define children (value-prev val))
      (cond [(set-member? seen val) (values seen edges)]
            [(set-empty? children) (values (set-add seen val) edges)]
            [else
             ;; make edges: each child to operation (child -> op)
             (for/fold ([seen seen][edges edges])
                       ([child children])
               ;; recurse on the child
               (extract-children-and-edges
                (set-add seen val)
                ;; add all edges: child -> op-id
                (set-add edges (edge child val))
                child))]))
    
    (define-values (seen edges)
      (for/fold ([seen (set)]
                 [edges (set)]
                 #:result (values seen edges))
                ([start (if (list? start) start (list start))])
        (define-values (new-seen new-edges) (extract-children-and-edges (set) (set) start))
        (values (set-union seen new-seen)
                (set-union edges new-edges))))

    (define (edge->dot-edge edge)
      (define child (edge-from edge))
      (define val (edge-to edge))
      ;; op has the id "<hash><op>"
      (define op-id (~a "\"" (eq-hash-code val) (value-op val) "\""))
      (~a (eq-hash-code child) " -> " op-id ";"))

    (append* (set-map edges edge->dot-edge)
             (for/list ([val seen])
               ;; label each value with a given label
               (cons
                ;; relable the hash of each value with "label | data | grad":
                ;; <hash> [label = "<label> | <data> | <grad>" shape="record"]
                (~a (eq-hash-code val)
                    " [label = \"" (value-label val) "| data "
                    (real->decimal-string (value-data val) 2)
                    "| grad "
                    (real->decimal-string (value-grad val) 2)
                    "\" shape=\"record\" ];")
                ;; operation values always make two nodes connected by an edge:
                ;; <op> -> <label>
                ;; the children connect to op
                ;; <children> *-> <op> -> <label>
                (if (not (string=? "" (value-op val)))
                    ;; make a new node and edge "<hash><op> -> <hash>"
                    (let ([id (~a "\""(eq-hash-code val) (value-op val)"\"")])
                      (list
                       ;; label the new node with the op
                       (~a id " [label=\"" (value-op val) "\"];")
                       ;; edge
                       (~a id " -> " (eq-hash-code val) ";")))
                    empty)))))
  
  (define dot-lines
    (append (cons "digraph {rankdir=\"LR\";"
                  (traverse-value v))
            (list "}")))
  (define fold (foldr string-append "" dot-lines))
  fold)


(define (draw-dot v
                  #:dpi (dpi 100)
                  #:size (size #f)
                  #:path (path #f)
                  #:type (type "png"))
  (parameterize ([current-custodian (make-custodian)])
    (let ([p-stdin (open-input-string (value->dot v))]
          [out (open-output-bytes)]
          [err (open-output-bytes)])
      (define proc-data
        (process/ports out p-stdin err
                       (string-join
                        (list "dot"
                              (~a "-T" type)
                              (~a "-Gdpi=" dpi)
                              (if size (~a "-Gsize=" size) "")))))
      (define control (list-ref proc-data 4))
      (control 'wait)
      (define errors (get-output-string err))
      (unless (string=? "" errors)
        (error 'draw-dot "errors"))
      (define data (get-output-bytes out))
      (if path
          (with-output-to-file path
            #:exists 'truncate/replace
            (lambda () (display data)))
          (make-object bitmap% (open-input-bytes data))))))

