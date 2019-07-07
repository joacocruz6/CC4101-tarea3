#lang play

#|
<expr> ::= <num>
         | <id>
         | <bool>
         | (if <expr> <expr> <expr>)
         | (+ <expr> <expr>)
         | '< <expr> <expr>)
         | (* <expr> <expr>)
         | (= <expr> <expr>)
         | (- <expr> <expr>)
         | (and <expr> <expr>)
         | (or <expr> <expr>)
         | (not <expr> <expr>)         
         | (seqn <expr> <expr>)
         | (local { <def>*} <expr>)

<def>    ::= (define <id> <expr>)


;EXTENSION PARA CLASE Y OBJETOS
<expr> ::= ... (expresiones del lenguage entregado) ...
        | (class <member>*)
        | (new <expr>)
        | (get <expr> <id>)
        | (set <expr> <id> <expr>)
        | (send <expr> <id> <expr>*)
        | this
        | (class <: <expr> <member>* )
        | (super <id> <expr>*
 
<member>  ::= (field <id> <expr>)
         | (method <id> (<id>*) <expr>)
|#


(deftype Expr
  (num n)
  (bool b)
  (id s)   
  (binop f l r)
  (unop f s)
  (my-if c tb fb)  
  (seqn expr1 expr2)  
  (lcal defs body)
  ;; Clases en el AST
  (class super-class members)
  (new class-id)
  (get obj field)
  (set obj field new-field)
  (this)
  (super method-name args)
  (field id expr)
  (method id args body)
  (send obj m-id args)
)
;; values
(deftype Val
  (numV n)
  (boolV b)
  (classV env parent-class field-list methods-list)
  (objectV class-ref))
;; definiciones de ambiente
(deftype Def
  (my-def id expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Environment abstract data type
 
empty-env        :: Env
env-lookup       :: Sym Env -> Val
multi-extend-env :: List<Sym> List<Val> Env -> Env
extend-frame-env! :: Sym Val Env -> Env 


representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#

(deftype Env
  (mtEnv)
  (aEnv hash env)) 

(def empty-env (mtEnv))

#|
env-lookup:: Sym Env -> Val
Busca un símbolo en el ambiente, retornando su valor asociado.
|#
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv hash rest)
     (if (hash-has-key? hash x)
         (hash-ref hash x)
         (env-lookup x rest))]))

#|
multi-extend-env:: List(Sym) List(Expr) Env -> Env
Crea un nuevo ambiente asociando los símbolos a sus valores.
|#
(define (multi-extend-env ids exprs env)
  (if (= (length ids) (length exprs))
      (aEnv (make-hash (map cons ids exprs)) env)
      (error "wrong_input, mismatched lengths")))

#|
extend-frame-env!:: Sym Val Env -> Void
Agrega un nuevo par (Sym, Val) al ambiente usando mutación.
Este método no crea un nuevo ambiente.
|#
(define (extend-frame-env! id val env)
  (match env
    [(mtEnv) (aEnv (make-hash (list (cons id val))) env)]
    [(aEnv h rEnv) (let* ([l (hash->list h)]
                          [la (cons (cons id val) l)])
                     (set-aEnv-hash! env (make-hash la)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definicion de object
(define object-class (class 'undefined '()))
;; parse :: s-expr -> Expr
(define (parse s-expr [inner #f] [inner-super #f])
  (match s-expr
    [(? number?) (num s-expr)]
    [(? symbol?)
     (if (equal? 'this s-expr)
         (if inner
             (this)
             (error "Parse error: this definition outside class"))
         (id s-expr))]    
    [(? boolean?) (bool s-expr)]
    ['() '()]
    [(list '* l r) (binop * (parse l inner) (parse r inner))]
    [(list '+ l r) (binop + (parse l inner) (parse r inner))]
    [(list '- l r) (binop - (parse l inner) (parse r inner))]
    [(list '< l r) (binop < (parse l inner) (parse r inner))]
    [(list '= l r) (binop = (parse l inner) (parse r inner))]    
    [(list 'or l r) (binop (λ (i d) (or i d)) (parse l inner) (parse r inner))]
    [(list 'and l r) (binop (λ (i d) (and i d)) (parse l inner) (parse r inner))]
    [(list 'not b) (unop not (parse b inner))]
    [(list 'if c t f) (my-if (parse c inner)
                             (parse t inner)
                             (parse f inner))]
    [(list 'seqn e1 e2) (seqn (parse e1 inner) (parse e2 inner))]    
    [(list 'local (list e ...)  b)
     (lcal (map parse-def e) (parse b inner))]
    ;; Parser de clases nuevas, se le añade inner para saber si se puede o no usar this.
    [(list 'class '<: super-class members ...) (class (parse super-class) (map parse-class members))]
    [(list 'class members ...) (class object-class (map parse-class members))]
    [(list 'this) (if inner
                      (this)
                      (error "Parse error: this definition outside class"))]
    [(list 'super method-name args ... )
     (if inner
         (super method-name args)
         (error "Parse error: can't use super outside of a method"))]
    [(list 'get obj fld-name) (get (parse obj inner) (parse fld-name inner))]
    [(list 'set obj fld-name new-val) (set (parse obj inner) (parse fld-name inner) (parse new-val inner))]
    [(list 'new class-id) (new (parse class-id inner))]
    [(list 'send obj m-id args ...) (send (parse obj inner) m-id args)]
    [(list 'fun args body) (new (class object-class (list (method 'm args (parse body)))))]
    [(list fexpr args ...) (send (parse fexpr) 'm args)]
    ))

;; parse-class ::= s-expr -> Expr
;; parsea la definicion de una clase
(define (parse-class member)
  (match member
    [(list 'field id value) (field id (parse value))]
    [(list 'method id args-list body) (method id args-list (parse body #t #t))]))
;; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  (match s-expr
    [(list 'define id b) (my-def id (parse b))]))

;; interp :: Expr Env -> Val
(define (interp expr env)
  (match expr
    [(num n) (numV n)]    
    [(bool b) (boolV b)]
    [(id x) (env-lookup x env)]  
    [(binop f l r) (make-val (f (open-val (interp l env))
                                (open-val (interp r env))))]
    [(unop f s) (make-val (f (open-val (interp s env))))]
    [(my-if c t f)
     (def (boolV cnd) (interp c env))
     (if cnd
         (interp t env)
         (interp f env))]
    [(seqn expr1 expr2) (begin 
                          (interp expr1 env)
                          (interp expr2 env))]
    [(lcal defs body)
     (let* ([new-env (multi-extend-env '() '() env)])
       (for-each (λ(x)
                   (let ([in-def (interp-def x new-env)])
                     (extend-frame-env! (car in-def) (cdr in-def) new-env)
                     #t)) defs)       
       (interp body new-env))     
     ]
    ;; class Interpretation
    [(class super-class members)
     (if (and (equal? 'undefined super-class) (equal? '() members))
         (classV empty-env 'undefined '() '())
         (classV env (interp super-class env) (init-field (filter field? members) env) (filter method? members)))]
    ;; New interpretation
    [(new e) (def (classV obj-env super-class field-list method-list) (interp e env))
             (def mythis (box 'undefined))
             (begin
               (extend-frame-env! 'this mythis obj-env)
               (def object-created (objectV (classV obj-env super-class field-list method-list)))
               (begin
                 (set-box! mythis object-created)
                 object-created))]
    ;; get interpretation
    [(get e field)
     (def (objectV  (classV obj-env super-class field-list method-list)) (interp e env))
     (def (id x) field)
     (match e
       [(this) (def (classV _ f-super f-list _) (env-lookup 'this-class env))
               (unbox (field-lookup x f-list f-super))]
       [ _ (unbox (field-lookup x field-list super-class))])]
    ;; send interpretation
    [(send e method-name args)
     (def (objectV (classV obj-env super-class field-list method-list)) (interp e env))
     (def (cons (method _ m-args m-body) (classV s-env m-super m-fields m-list)) (method-lookup method-name (classV obj-env super-class field-list method-list)))
     (def m-env (multi-extend-env m-args (map (lambda (x) (interp x env)) (map parse args)) obj-env))
     (begin
       (extend-frame-env! 'super super-class m-env)
       (begin
         (extend-frame-env! 'this-class (classV m-env m-super m-fields m-list) m-env)
         (interp m-body m-env)))]
    ;;this interpretation
    [(this) (unbox (env-lookup 'this env))]
    ;; set interpretation
    [(set e field-id v)
     (def (objectV (classV obj-env super-class field-list method-list)) (interp e env))
     (def (id x) field-id)
     (def new-val (interp v env))
     (match e
       [(this) (def (classV _ f-class f-list m-list) (env-lookup 'this-class env))
               (def exists (field-lookup x f-list f-class))
               (set-box! exists new-val)]
       [_ (def exists (field-lookup x field-list super-class))
     (set-box! exists new-val)])]
    ;; super interpretation
    [(super name args)
     (def (classV obj-env super-class sfields smethods) (env-lookup 'super env))
     (def (cons (method _ m-args m-body) (classV class-env parent sup-fields sup-methods)) (method-lookup name (classV obj-env super-class sfields smethods)))
     (def m-env (multi-extend-env m-args (map (lambda (x) (interp x env)) (map parse args)) obj-env))
     (begin
       (extend-frame-env! 'super super-class m-env)
       (begin
         (extend-frame-env! 'this-class (classV class-env parent sup-fields sup-methods) m-env)
       (interp m-body m-env)))]))
;;field-lookup ::= symbol List[Pair] classV -> box
;; Performs the lookup of a field of a object
(define (field-lookup id fields super-class)
  (def (classV _ s-class f-list _) super-class)
  (match fields
    ['()
     (if (equal? object super-class)
                 (error "field not found")
                 (field-lookup id f-list s-class))]
    [(cons (cons x val) next)
     (if (equal? id x)
         val
         (field-lookup id next super-class))]))
;;init-field ::= List[field] env -> List[Pair]
;;Make a list of pairs id box to the values of the field of an object
(define (init-field field-list env)
  (match field-list
    ['() '()]
    [(cons (field id val) next) (def field-value (box (interp val env)))
                            (cons (cons id field-value) (init-field next env))]))
;;method-lookup ::= symbol List[method] -> (cons method class) /error if not found
;;Finds the first ocurrence of the method of an object
(define (method-lookup id myclass)
  (def (classV class-env super-class f-list method-list) myclass)
  (match method-list
    ['()
     (match super-class
       ['undefined (error "method not found")]
       [(classV c-env parent _ parent-methods) (method-lookup id super-class)])]
    [(cons (method m-name m-args m-body) next)
     (if (equal? m-name id)
         (cons (method m-name m-args m-body) myclass)
         (method-lookup id (classV class-env super-class f-list next)))]))
;; open-val :: Val -> Scheme Value
(define (open-val v)
  (match v
    [(numV n) n]
    [(boolV b) b]))

;; make-val :: Scheme Value -> Val
(define (make-val v)
  (match v
    [(? number?) (numV v)]
    [(? boolean?) (boolV v)]
    ))

;; interp-def :: Def, Env -> Expr
(define (interp-def a-def env)
  (match a-def
    [(my-def id body) (cons id (interp body env))]))

;; run :: s-expr -> Val
(define (run s-expr)
  (interp (parse s-expr) empty-env))

#|
run-val:: s-expr -> Scheme-Val + Val
Versión alternativa de run, que retorna valores de scheme para primitivas y
valores de MiniScheme para clases y objetos
|#
(define object (interp object-class empty-env))
(define (run-val s-expr)
  (define val (interp (parse s-expr) empty-env))
  (match val
    [(numV n) n]
    [(boolV b) b]
    [(classV _ _ _ _) '<class>]
    [(objectV _) '<object>]
    [x x]))