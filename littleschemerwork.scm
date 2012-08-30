(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
     ((null? x) #T)
     ((not (atom? (car x))) #F)
     (else (lat? (cdr x))))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #F)
     ((eq? a (car lat)) #T)
     (else (member? a (cdr lat))))))
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat)) (cdr lat))
     ((cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define seconds
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (cadr (car lat)) (seconds (cdr lat)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL-f
  (lambda (comp?)
    (lambda (new old lat)
      (cond
       ((null? lat)(quote()))
       ((comp? old (car lat)) (cons new lat))
       (else (cons (car lat) ((insertL-f comp?) new old (cdr lat))))))))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat)) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
 (lambda (new old lat)
   (cond
    ((null? lat) (quote ()))
    ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define p
  (lambda (x y)
   (cond
    ((zero? y) x)
    (else (p (add1 x) (sub1 y))))))

(define s
  (lambda (x y)
    (cond
     ((zero? y) x)
     (else (s (sub1 x) (sub1 y))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (p (car tup) (addtup (cdr tup)))))))

(define m
  (lambda (x y)
    (cond
     ((zero? y) 0)
     (else (p x (m x (sub1 y)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (p (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (x y)
    (cond
     ((zero? x) #F)
     ((zero? y) #T)
     (else (gt (sub1 x) (sub1 y))))))

(define lt
  (lambda (x y)
    (cond
     ((zero? y) #F)
     ((zero? x) #T)
     (else (lt (sub1 x) (sub1 y))))))

(define eq= 
  (lambda (x y)
    (cond
     ((lt x y) #F)
     ((gt x y) #F)
     (else #T))))

(define exp
  (lambda (x y)
    (cond
     ((zero? y) 1)
     (else (m x (exp x (sub1 y)))))))

(define d
  (lambda (x y)
    (cond
     ((lt x y) 0)
     (else (p 1 (d (s x y) y))))))

(define length
  (lambda (lat)
    (cond 
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((eq= n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define nonums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((number? (car lat)) (nonums (cdr lat)))
       (else (cons (car lat) (nonums (cdr lat)))))))))

(define allnums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (allnums (cdr lat))))
       (else (allnums (cdr lat))))))))

(define eqan?
  (lambda (x y)
    (cond
     ((and (number? x) (number? y)) (eq= x y))
     ((or (number? x) (number? y)) #F)
     (else (eq? x y)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))


(define one?
  (lambda (x)
    (eqan? 1 x)))

(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? a (car lat)) (rember* a (cdr lat))) 
     ((atom? (car lat)) (cons (car lat) (rember* a (cdr lat))))
     (else (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

(define insertR*
  (lambda (old new lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? old (car lat)) (cons old (cons new (insertR* old new (cdr lat)))))
     ((atom? (car lat)) (cons (car lat) (insertR* old new (cdr lat))))
     (else (cons (insertR* old new (car lat)) (insertR* old new (cdr lat)))))))

(define occur*
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((atom? (car lat))
      (cond
       ((eq? a (car lat)) (add1 (occur* a (cdr lat))))
       (else (occur* a (cdr lat)))))
     (else (p (occur* a (car lat)) (occur* a (cdr lat)))))))

(define subst*
  (lambda (new old lat) 
    (cond
     ((null? lat) (quote ()))
     ((atom? (car lat))
      (cond
       ((eq? old (car lat)) (cons new (subst* new old (cdr lat))))
       (else (cons (car lat) (subst* new old (cdr lat))))))
     (else (cons (subst* new old (car lat)) (subst* new old (cdr lat)))))))

(define insertL*
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     ((atom? (car lat))
      (cond
       ((eq? old (car lat)) (cons new (cons old (insertL* new old (cdr lat)))))
       (else (cons (car lat) (insertL* new old (cdr lat))))))
     (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define member*
  (lambda (a lat)
    (cond
     ((null? lat) #F)
     ((atom? (car lat))
      (cond
       ((eq? a (car lat)) #T)
       (else (member* a (cdr lat)))))
     (else (or (member* a (car lat)) (member* a (cdr lat)))))))


(define leftmost
  (lambda (lat)
    (cond
     ((atom? lat) lat)
     (else (leftmost (car lat))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (or (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? (car aexp)) (number? aexp))
     (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define valueold
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) (quote +))
      (p (value (car nexp)) (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote x))
      (m (value (car nexp)) (value (car (cdr (cdr nexp))))))
     (else (exp (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (p (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote x))
      (m (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     (else (exp (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define p2
  (lambda (x y)
    (cond
     ((sero? (y)) x)
     (else (edd1 (p2 x (zub1 y)))))))

(define setfirst?
  (lambda (x)
    (cond
     ((or (null? x) (null? (cdr x))) #T)
     ((equal? (car x) (car (cdr x))) #F)
     (else (and (set? (cons (car x) (cdr (cdr x)))) (set? (cdr x)))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #T)
     ((member? (car lat) (cdr lat)) #F)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat) (makeset (multirember (car lat) lat)))))))

(define subset?
  (lambda (lat1 lat2)
    (cond
     ((null? lat1) #T)
     ((member? (car lat1) lat2) (subset? (cdr lat1) lat2))
     (else #F))))

(define eqset?
  (lambda (set1 set2)
    (cond
     ((and (subset? set1 set2) (subset? set2 set1)) #T)
     (else #F))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #F)
     (else
      (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (cons (car set1) (union (cdr set1) (multirember (car set1) set2))))
     (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (lat)
    (cond
     ((null? (cdr lat)) (car lat))
     (else (intersectall (cons (intersect (car lat) (car (cdr lat))) (cdr (cdr lat))))))))

(define a-pair
  (lambda (lat)
    (cond
     ((or
      (null? lat)
      (null? (car lat))
      (null? (cdr lat)))
      #F)
     ((null? (cdr (cdr lat))) #T)
     (else #F))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (a b)
    (cons a (cons b (quote ())))))

(define third
  (lambda (l)
    (cadr (cdr l))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define fullfun
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (comp)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((comp a (car lat)) (cdr lat))
       (else (cons (car lat) (rember-f comp a (cdr lat))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define seqL
  (lambda (a b c)
    (cons a (cons b c))))

(define seqR
  (lambda (a b c)
    (cons b (cons a c))))

(define subst
  (lambda (a b c)
    (cons a c)))

(define insert-g
  (lambda (insertion)
    (lambda (new old lat)
      (cond
       ((null? lat) (quote ()))
       ((eq? old (car lat)) (insertion new old (cdr lat)))
       (else (cons (car lat) ((insert-g insertion) new old (cdr lat))))))))

(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (((atom-to-function (operator nexp)) (value2 (1st-sub-exp nexp)) (value2 (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (rem lat)
      (cond
       ((null? lat) (quote ()))
       ((test? (car lat) rem)((multirember-f test?) rem (cdr lat)))
       (else (cons (car lat) ((multirember-f test?) rem (cdr lat))))))))


(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat)) (multiremberT test? (cdr lat)))
     (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col (quote ()) (quote ())))
     ((eq? (car lat) a)
      (multirember&co a (cdr lat)
		      (lambda (newlat seen)
			(col newlat
			     (cons (car lat) seen)))))
     (else
      (multirember&co a
		      (cdr lat)
		      (lambda (newlat seen)
			(col (cons (car lat) newlat)
			     seen)))))))

(define a-friend
  (lambda (x y)
    (cons x (cons y (quote())))))

(define counter
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? oldL (car lat)) (cons new (cons (car lat)(multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? oldR (car lat)) (cons (car lat) (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col (quote()) 0 0))
     ((eq? oldL (car lat)) (cons new (cons (car lat)(multiinsertLR&co new oldL oldR (cdr lat)
								   (lambda (newlat leftin rightin)
								     (col newlat (add1 leftin) rightin)
								   )))))
     ((eq? oldR (car lat)) (cons (car lat) (cons new (multiinsertLR&co new oldL oldR (cdr lat)
								    (lambda (newlat leftin rightin)
								      (col newlat leftin (add1 rightin))
								    )))))
     (else (cons (car lat) (multiinsertLR&co new oldL oldR (cdr lat)
					  (lambda (newlat leftin rightin)
					    (col (cons (car lat) newlat) leftin rightin))
					  ))))))

(define outputfunc
  (lambda (newlat leftin rightin)
    (cons newlat (cons (cons leftin (quote())) (cons rightin (quote()))))))

(define even?
  (lambda (x)
    (= (* (d x 2) 2) x)))

(define evens-only*
  (lambda (sexp)
    (cond
     ((null? sexp)(quote ()))
     ((atom? (car sexp))
      (cond
       ((even? (car sexp)) (cons (car sexp) (evens-only* (cdr sexp))))
       (else (evens-only* (cdr sexp)))))
     (else (cons (evens-only* (car sexp)) (evens-only* (cdr sexp)))))))

(define evens-only*&co
  (lambda (sexp col)
    (cond
     ((null? sexp)(col (quote ()) 1 0))
     ((atom? (car sexp))
      (cond
       ((even? (car sexp)) (evens-only*&co (cdr sexp) 
					(lambda (newlat even odd)
					  (col (cons (car sexp) newlat) (m (car sexp) even) odd))))
       (else (evens-only*&co (cdr sexp)
			  (lambda (newlat even odd)
			    (col newlat even (p (car sexp) odd)))))))
     (else (evens-only*&co (car sexp) 
			   (lambda (al ap as)
				  (evens-only*&co (cdr sexp)
						  (lambda (dl dp ds)
						    (col (cons al dl)
							 (m ap dp)
							 (p as ds))))))))))

(define evensout
  (lambda (newlat even odd)
    (cons odd (cons even newlat))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a num lat)
    (cond
     ((number? (pick num lat)) (keep-looking a (pick num lat) lat))
     (else (eq? a (pick num lat))))))

(define shift
  (lambda (pair)
   (build (first (first pair))
	  (build (second (first pair)) (second pair)))))
    
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f)x)))))))

(define new-entry build)

(define extend-table cons)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))

(define lookup-in-entry-help
  (lambda (name f s ef)
    (cond
     ((null? f) (ef name))
     ((eq? name (car f)) (car s))
     (else (lookup-in-entry-help name (cdr f) (cdr s) ef)))))


(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name (car table)(lambda (name)
		       (lookup-in-table name (cdr table) table-f)))))))

(define atom-to-function
  (lambda (nexp)
    (cond
     ((equal? nexp (quote +)) p)
     ((equal? nexp (quote -)) s)
     ((equal? nexp (quote x)) m)
     (else exp))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
	     (cond
	      ((eq? (car e) (quote quote)) *quote)
	      ((eq? (car e) (quote lambda)) *lambda)
	      ((eq? (car e) (quote cond)) *cond)
	      (else *application)))
     (else *application))))

(define valuef
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines))
	       table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (e)
    (cond
     ((atom? e)
      (eq? e (quote else)))
     (else #F))))

(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (lis table)
    (cond 
     ((null? lis) (quote()))
     (else (cons (meaning (car lis) table) (evlis (cdr lis) table))))))

(define *application
  (lambda (e table)
    (applyf
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define applyf
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons))
      (cons (first vals) (second vals)))
     ((eq? name (quote car))
      (car (first vals)))
     ((eq? name (quote cdr))
      (cdr (first vals)))
     ((eq? name (quote null?))
      (null? (first vals)))
     ((eq? name (quote eq?))
      (eq? (first vals) (second vals)))
     ((eq? name (quote atom?))
      (:atom? (first vals)))
     ((eq? name (quote zero?))
      (zero? (first vals)))
     ((eq? name (quote add1))
      (add1 (first vals)))
     ((eq? name (quote sub1))
      (sub1 (first vals)))
     ((eq? name (quote number?))
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of clusure)
	     (extend-table
	      (new-entry
	       (formals-of closure) vals)
	      (table-of closure)))))