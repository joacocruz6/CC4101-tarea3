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
(test/exn (parse '{seqn (local
                    ([define A
                       (class
                           (field x 2))])
                    10)
                        this}) "Parse error: this definition outside class")

;;Test of make fields enviroment
(test (make-fields-env (list (field 'x (num 2))) empty-env) (aEnv (hash 'x (numV 2)) (mtEnv)))
;;Test of field lookup
(test (field-lookup 'x (list (field 'x (num 2)) (field 'y (bool #f))) (make-fields-env (list (field 'x (num 2)) (field 'y (bool #f))) empty-env)) (box (numV 2)))
(test/exn (field-lookup 'z (list (field 'x (num 2)) (field 'y (bool #f))) (make-fields-env (list (field 'x (num 2)) (field 'y (bool #f))) empty-env)) "field not found")
;;Test of method lookup
(test (method-lookup 'm (list (method 'm '() (binop + (num 1) (num 2))) (method 'foo '() (this)))) (method 'm '() (binop + (num 1) (num 2))))
(test/exn (method-lookup 'bar (list (method 'm '() (binop + (num 1) (num 2))) (method 'foo '() (this)))) "method not found")
;;General test for main
(test (run-val '{local
                  ([define A
                     (class
                         (field x 2)
                       (method m () (+ 2 2)))]
                   [define o (new A)])
                  (send o m)}) 4)
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
(test/exn (run-val '(seqn (local
                  [(define A
                     (class
                         (field x 2) 
                       (method m () (get this x))))]
                  10)
                (new A))) "env-lookup: free identifier: A")
(test (run-val '(local
            [(define x 10)
             (define A
               (class
                   (method m (y) (+ x y))))
             (define o (new A))]
            (send o m 1))) 11)
(test (run-val '(local
             [(define c (class
                            (field x 1)
                          (field y 2)
                          (method sum (z) (+ (get this x) (+ (get this y) z)))
                          (method set-x (val) (set this x val))))
              (define o (new c))]
             (seqn
              (send o set-x (+ 1 3))
              (+ (send o sum 3) (get o y)))))
      11)
(test (run-val '(local
              [(define A
                 (class
                     (method apply (c)
                             (send (new c) m))))
               (define o (new A))]
              (send o apply (class
                                (field x 2) 
                                (method m () (get this x)))))) 2)

