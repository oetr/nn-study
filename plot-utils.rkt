#lang racket
(require plot
         images/flomap)

(provide (all-defined-out))

;; plotting in Racket is on another level...
;; rolling my own heatmap :)

(define (heatmap vov scale)
  (define width (vector-length vov))
  (define height (vector-length (vector-ref vov 0)))
  
  ;; max val is used to normalize each pixel intensity
  (define max-val (* 1.0
                     (for*/fold ([max-acc -1])
                                ([r vov][c r])
                       (max c max-acc))))
  (flomap->bitmap
   (build-flomap* 4 (* width scale) (* height scale)
                  (lambda (x y)
                    (define intensity
                      (/
                       (vector-ref (vector-ref vov (quotient x scale))
                                   (quotient y scale)) max-val))
                    ;; classic grayscale
                    (vector 1.0 intensity intensity intensity)))))
