#lang racket

(provide mark-and-sweep
         mark-and-sweep*)

(require graph      ; graph data structures and algorithms
         threading) ; thrush combinator macros

(define (mark-and-sweep graph root)
  "Traverse the specified directed graph starting at the specified vertex,
   marking verticies as they are visited. Return the list of vertices in the
   graph that were not visited."
  (define-vertex-property graph marked? #:init #f)

  (let recur ([vertex root])
    (marked?-set! vertex #t)
    (for ([neighbor (in-neighbors graph vertex)])
      (when (not (marked? neighbor))
        (recur neighbor))))

  (~>> graph get-vertices (filter (negate marked?))))

(define (mark-and-sweep* graph roots)
  "Traverse the specified directed graph starting at an inserted vertex with
   an edge leading to each of the specified vertices, marking verticies as
   they are visited. Return the list of vertices in the graph that were not
   visited."
  (let* ([root  (gensym)]
         [graph (graph-copy graph)])
    (add-vertex! graph root)
    (for ([subroot roots])
      (add-edge! graph root subroot))

    (mark-and-sweep graph root)))

(module+ test
  (require rackunit)

  (define (sequence->set seq)
    (for/set ([item seq])
      item))

  (define (check-equal-no-order? left right)
    (check-equal? (sequence->set left) (sequence->set right)))

  (define g (directed-graph '((root a) (a b) (b c) (x y) (y c))))
  (check-equal? (list->set (mark-and-sweep g 'root))
                (list->set '(x y))))
