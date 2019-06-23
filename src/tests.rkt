#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
              (seqn {+ x 1}
                    x))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test para creacion y uso de clases base:
;;parser de clases
(test (parse '{class
                  {field x 2}}) (class (list (field 'x (num 2)))))
(test (parse '{class
                  {method m () (+ 1 2)}}) (class (list (method 'm '() (binop + (num 1) (num 2))))))
(test (parse '{class
                  {method m () (this)}}) (class (list (method 'm '() (this)))))
(test/exn (parse '{this}) "Parse error: this definition outside class")
(test (run-val '{local
                  ([define A
                    (class
                        (field x 2))])
                  10}) 10)
(test (run-val '{local
                  ([define A
                     (class
                         (field x 2)
                       (method m () (get this x)))]
                   [define o (new A)])
                  (send o m)}) 2)
(test (run-val '{local
                  ([define A
                     (class
                         (field x 2)
                       (method m () (set this x 5)))]
                   [define o (new A)])
                  (seqn
                   (send o m)
                   (get o x))}) 5)
