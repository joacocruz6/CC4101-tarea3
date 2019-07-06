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

;; Test para creacion y uso de clases base (Comentado por las herencias, tuve que cambiar el parser):
;;parser de clases

#;[(test (parse '{class
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
]
;;Test of make fields enviroment
#;(test (make-fields-env (list (field 'x (num 2))) empty-env) (aEnv (hash 'x (numV 2)) (mtEnv))) ;; Falla por como esta implementado hash :/
;;Test of field lookup
(test (field-lookup 'x (list (field 'x (num 2)) (field 'y (bool #f))) object (make-fields-env (list (field 'x (num 2)) (field 'y (bool #f))) object empty-env)) (box (numV 2)))
(test/exn (field-lookup 'z (list (field 'x (num 2)) (field 'y (bool #f))) object (make-fields-env (list (field 'x (num 2)) (field 'y (bool #f))) object empty-env)) "field not found")
;;Test of method lookup
(test (method-lookup 'm (list (method 'm '() (binop + (num 1) (num 2))) (method 'foo '() (this))) object) (method 'm '() (binop + (num 1) (num 2))))
(test/exn (method-lookup 'bar (list (method 'm '() (binop + (num 1) (num 2))) (method 'foo '() (this))) object) "method not found")
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
;; Test para scope
(test (run-val '{local
                  [(define x 10)
                   (define c (class
                                 (method sum () (+ x 2))))
                   (define x 11)
                   (define o (new c))]
                  (send o sum)}) 12)
(test (run-val '(local
                [(define x 10)
                 (define A
                   (class
                       (method m (y) (+ x y))))
                 (define x 20)
                 (define o (new A))]
                (send o m 1))) 11)
#|
Test para soporte de herencia
|#
(define obj (class 'undefined '()))
;; Test para el parser:
(test (parse '{class
                  {field x 2}}) (class obj (list (field 'x (num 2)))))
(test (parse '{class
                  {method m () (+ 1 2)}}) (class obj (list (method 'm '() (binop + (num 1) (num 2))))))
(test (parse '{class
                  {method m () (this)}}) (class obj (list (method 'm '() (this)))))
(test (parse '{class <: c1}) (class (id 'c1) '()))
(test (parse '{class <: c1
                {method m () (+ 1 2)}}) (class (id 'c1) (list (method 'm '() (binop + (num 1) (num 2))))))
(test (parse '{class
                  <: c1
                {method m () (super h 10)}}) (class (id 'c1) (list (method 'm '() (super 'h '(10))))))
                
(test/exn (parse '{this}) "Parse error: this definition outside class")
(test/exn (parse '{seqn (local
                    ([define A
                       (class
                           (field x 2))])
                    10)
                        this}) "Parse error: this definition outside class")
;; Test para el interprete
(test (run-val '{local
                  [{define c1 {class
                                 {method h (x) {+ 1 x}}}}
                   {define c2 {class <: c1
                                {method m () {+ 4 10}}}}
                   {define o (new c2)}]
                  (send o h 10)}) 11)

(test (run-val '{local
                  [{define c1 {class
                                 {method h (x) {+ 1 x}}}}
                   {define c2 {class <: c1
                                {method h (x) {+ 4 x}}}}
                   {define o (new c2)}]
                  (send o h 10)}) 14)
(test (run-val '(local
              [(define c1 (class
                              (method f (z) (< z 7))))
               (define c (class <: c1))
               (define o (new c))]
              (send o f 20))) #f)
(test/exn (run-val '{local
                  [{define c1 {class
                                 {method p (x) {+ 1 x}}}}
                   {define c2 {class <: c1
                                {method s (x) {+ 4 x}}}}
                   {define o (new c2)}]
                  (send o h 10)}) "method not found")

(test (run-val '{local
                  [{define c1 {class
                                 {method h (x) {+ 1 x}}}}
                   {define c2 {class <: c1
                                {method m () {super h 10}}}}
                   {define o (new c2)}]
                  (send o m)}) 11)
(test (run-val '{local
                  [{define x 4}
                   {define c1 {class
                                  {method h (y) {+ y x}}}}
                   {define c2 {class <: c1
                                {method m () {super h 10}}}}
                   {define o {new c2}}]
                  {send o m}}) 14)
(test (run-val '{local
                  [{define x 4}
                   {define c1 {class
                                  {method h (y) {+ y x}}}}
                   {define x 5}
                   {define c2 {class <: c1
                                {method m () {super h 10}}}}
                   {define o {new c2}}]
                  {send o m}}) 14)
;; Field shadowing
(test (run-val '(local
              [(define A (class 
                           [field x 1]
                           [field y 0]
                           [method ax () (get this x)]))
               (define B (class <: A
                           [field x 2]
                           [method bx () (get this x)]))
               (define b (new B))]
              (send b ax))) 1)
(test (run-val '(local
                  [(define A (class
                                 [field x 1]
                               [field y 0]
                               [method ax () {seqn {set this x 10} 20}
                                       ]
                               )
                     )
                   (define B (class <: A
                                [field x 2]
                                [method bx () (get this x)]
                                ))
                   (define o (new B))]
                   (seqn
                    (send o ax)
                    (send o bx)))) 2)
;; Funciones como azucar sintactico:
(test (parse '{fun {x} {+ x x}}) (new (class (class 'undefined '()) (list (method 'm '(x) (binop + (id 'x) (id 'x)))))))
(test (run-val
       '{{fun {x} {+ x x}} 2}) 4)
(test (run-val '{local
                  [{define f {fun {x y} {+ x y}}}
                   {define a 10}
                   {define g {fun {x y} {+ a {f x y}}}}]
                  {g 20 30}}) 60)
(test (run-val '{local
                  [{define f {fun {x y} {+ x y}}}
                   {define a 10}
                   {define g {fun {x y} {+ a {f x y}}}}
                   {define a 20}]
                  {g 20 30}}) 60)
(test (run-val '(local
              [(define f (fun (x)
                              (+ x x)))]
              (f 5))) 10)