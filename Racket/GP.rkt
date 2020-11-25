#lang racket
(require plot)
;(require csv-reading)  ;doesn't work


;; The range of values that 'x' takes: (-1 -0.9 -0.8 ... 0 ... 0.8 0.9 1)
(define RANGE (range -1 1 0.1))
(define len (length RANGE)); ;x data

;; The desired value range of the actual function,
;; namely x ^ 2 + x + 1, on the above range
;; later it will be with the data found on the internet 
(define SAMPLES (map (λ (x) (+ (* x x x) x 2)) RANGE)) ;y data

;;leafs 
(define TERMINALS '(0 1 2 "x"))

;;functions

;(define ope1 (make-hash))
;(hash-set! ope1 "plus" +)
;(define ope2 (make-hash))
;(hash-set! ope2 "moins" -)
;(define ope3 (make-hash))
;(hash-set! ope3 "fois" *)
;(define ope4 (make-hash))
;(hash-set! ope4 "diviser" /)

(define ope (make-hash))
(hash-set! ope "plus" +)
(hash-set! ope "moins" -)
(hash-set! ope "fois" *)

(define safe_div (lambda (a b)
                   (if (eq? b 0) a (/ a b))))

(hash-set! ope "diviser" safe_div)



;(define FUNCTIONS '(ope1 ope2 ope3 ope4)
;)
(define FUNCTIONS '("plus" "moins" "fois" "diviser")
)



;;fitness function
(define fitness (lambda (Y1 Y2)
        (if (null? Y1) 0 (+ (abs (- (car Y1) (car Y2))) (fitness (cdr Y1) (cdr Y2))))))


;return i-th element of a list
(define ielement (lambda (i L)
                   (if (eq? i 0) (car L) (ielement (- i 1) (cdr L))))) 

;create tree struct (binary tree)
(struct tree (left right node leaf)) ;leaf equal 1 if leaf 0 elsewhere
(struct leaf (leaf)) ; test leaf struct

;creation of a random tree
; all the 4 in the function should be change if the number of leaf or functions changes 
(define random_prog (lambda (depth)
  (if (>= (random 10) (* depth 2))
      ;then
      (tree (random_prog (+ depth 1)) (random_prog (+ depth 1)) (ielement (random 4) FUNCTIONS) 0)
      ;else
      (tree (leaf (ielement (random 4) TERMINALS)) (leaf (ielement (random 4) TERMINALS)) (ielement (random 4) FUNCTIONS) 1))))


;int to string
(define (to-string x)
  (define o (open-output-string))
  (write x o)
  (define str (get-output-string o))
  (get-output-bytes o #t)
  str
  )


;print the 'tree' struct


(define print_tree (lambda (arbre)
                     (match arbre [(leaf "x") "x"]
                       [(leaf a) (to-string a)]
                       [(tree l r n leaf) (string-append (string-append "(" (string-append (string-append (print_tree l) n) (print_tree r))) ")")])))



;choose random node of a tree
(define random_node (lambda (selected depth)
                      (match selected [(tree l r n leaf)
                                       #:when (= leaf 1) (tree l r n leaf)]
                        [(tree l r n leaf)
                         #:when (< (random 10) (* depth 2)) selected]
                        [(tree l r n leaf) (if (> (random) 0.5) (random_node l (+ depth 1)) (random_node r (+ depth 1)))])))
               

;mutation of a tree
(define mutation (lambda (selected)
                   (match (random_node selected 0) [(tree l r n leaf)
                         
                                                    #:when (> (random) 0.5) (tree (random_prog 0) r n leaf)]
                     [(tree l r n leaf) (tree l (random_prog 0) n leaf)])))



;perform crossover operation between tree1 and tree2
(define crossover (lambda (t1 t2)
                          (match (random_node t1 0) [(tree l r n leaf)
                          #:when (> (random) 0.5) (tree (random_node t2 0) r n leaf)]
                     [(tree l r n leaf) (tree l (random_node t2 0) n leaf)])))



;evaluate a tree with 1 value

(define eval (lambda (val arbre)
               (match arbre [(leaf "x") val]
                 
                 [(tree l r n leaf) ((hash-ref ope n) (eval val l) (eval val r))]
                 [(leaf a) a] )))


;create list with value evaluated thanks to tree 
(define liste_eval (lambda (list_x arbre)
                     (if (null? list_x) '() (cons (eval (car list_x) arbre) (liste_eval (cdr list_x) arbre))))) 


;algorithm constants
(define evolution_size 5)
(define max_generation 1000)
(define pop_size 2000)
(define popu (make-vector pop_size))

;variable that will contain the best prog and the associate fitness
(define best (random_prog 0))
(define best_fit (fitness (liste_eval RANGE best) SAMPLES)) 


;find best prog of the pop
(define best_tree (lambda (pop)
                    (if (null? pop) best
                        (cond ((< (fitness (liste_eval RANGE (car pop)) SAMPLES) best_fit) (begin (set! best (car pop)) (set! best_fit  (fitness (liste_eval RANGE (car pop)) SAMPLES) ) (best_tree (cdr pop))))
                            (else (best_tree (cdr pop)))))))


;create initial pop
(define pop0 (lambda (size)
               (if (= size 0) '() (cons (random_prog 0) (pop0 (- size 1))))))




;create new pop generation
(define actu_pop (lambda (pop)
                   (if (= (length pop) 1) (cons (mutation (car pop)) '());cannot crossover with 1 element
                   (if (null? pop) '() (cons (evolve (car pop) (car (cdr pop))) (actu_pop (cdr pop)))))))


                                           

;evolve 1 people of population with mutation or crossover
(define evolve( lambda (t1 t2)
                 (if (> (random) 0.5) (mutation t1) (crossover t1 t2))))
                                             
;genetic programming algo
(define gp (lambda (pop n)
             (if (= n 0) (best_tree pop) (gp (actu_pop pop) (- n 1)))))



;===================================================================================================================================================
;vector way

;(define best_tree (lambda (size)
;                    (if (= size 0) best
;                        (cond ((< (fitness (liste_eval RANGE (vector-ref popu (- size 1))) SAMPLES) best_fit)
;                               (begin (set! best (vector-ref popu (- size 1))) (set! best_fit  (fitness (liste_eval RANGE (vector-ref popu (- size 1))) SAMPLES) ) (best_tree (- size 1))))
;                            (else (best_tree (- size 1)))))))

;(define pop0 (lambda (size)
;               (if (= size 0) popu (begin (vector-set! popu (- size 1) (random_prog 0)) (pop0 (- size 1)))))) 
;(define actu_pop (lambda (size)
;                   (if (= size 2) (cons (mutation (vector-ref popu (- size 1))) '());cannot crossover with 1 element
;                   (if (= size 1) '() (cons (evolve (vector-ref popu (- size 1)) (vector-ref popu (- size 2))) (actu_pop (- size 1)))))))
;(define gp (lambda (pop_size n)
;             (if (= n 0) (best_tree pop_size) (begin (actu_pop (- pop_size 1)) (gp (- pop_size 1) (- n 1))))))


(define y (liste_eval RANGE (gp (pop0 pop_size) max_generation)))
(plot (points (map vector RANGE SAMPLES) #:color 'green))
(plot (points (map vector RANGE y) #:color 'red))



;((xfoisx)diviser((0diviser1)fois(((0fois1)plus(0fois0))plus(xfois0))))  --> x*x

;((((0diviser1)plus(0plus0))plus(((1diviser2)fois((xplus0)fois((1foisx)fois(1plus1))))moins((0moins2)moins(((0foisx)moins(0plus0))
;plus((0fois2)diviser(0fois2))))))moins(((0diviser1)plus(0plus0))plus((((xmoinsx)fois(0plus0))fois((xdiviser2)plus(1moins0)))plus((1moins0)moins(xdiviser1)))))