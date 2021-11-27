(load "~/miniKanren-with-symbolic-constraints/mk.scm")

(define is-even-lengtho
  (lambda (xs)
    (conde
      [(== xs '())]
      [(fresh (first rest)
          (== xs (cons first rest))
          (is-odd-lengtho rest))])))

(define is-odd-lengtho
  (lambda (xs)
    (fresh (first rest)
      (== xs (cons first rest))
      (is-even-lengtho rest))))
      

(define neighbors
  (lambda (u edges)
    (cond
      [(null? edges) '()]
      [(eq? u (car (car edges)))
        (cons
          (car (cdr (car edges)))
          (neighbors u (cdr edges)))]
      [else (neighbors u (cdr edges))])))

(define without-desto
  (lambda (dest edges edges-without-dest)
    (conde
      [(== edges '()) (== edges-without-dest '())]
      
      [(fresh (v1 v2 rest-edges rest-edges-without-dest)

        (== edges (cons `(,v1 ,v2) rest-edges))
        (conde
          [(==  dest v2) (== edges-without-dest                  rest-edges-without-dest) ]
          [(=/= dest v2) (== edges-without-dest (cons `(,v1 ,v2) rest-edges-without-dest))])
          
        (without-desto dest rest-edges rest-edges-without-dest))])))

(define neighborso
  (lambda (u edges neighbors)
    (conde
      [(== edges '()) (== neighbors '())]

      [(fresh (v1 v2 rest-edges rest-neighbors)
        (== edges (cons `(,v1 ,v2) rest-edges))
		(conde
		  [(==  u v1) (== neighbors (cons v2 rest-neighbors))]
		  [(=/= u v1) (== neighbors          rest-neighbors)])

		(neighborso u rest-edges rest-neighbors))])))


;; Given an element, and a list of pairs,
;; partition the list of pairs into 4 lists, preserving order,
;; given that element is in the left, right, both, or neither of each pair.
(define in-pairso (lambda (x pairs in-both in-l in-r in-neither)
  (conde
    [(== pairs '()) (== in-both '()) (== in-l '()) (== in-r '()) (== in-neither '())]
    
    [(fresh (l r rest in-both-rec in-l-rec in-r-rec in-neither-rec)
     (== pairs (cons `(,l ,r) rest))
     (conde
       [(==  x l) (==  x r)
        (== in-both (cons `(,l ,r) in-both-rec))
        (== in-l in-l-rec) (== in-r in-r-rec) (== in-neither in-neither-rec)]
       
       [(==  x l) (=/= x r)
        (== in-both in-both-rec)
        (== in-l (cons `(,l ,r) in-l-rec))
        (== in-r in-r-rec) (== in-neither in-neither-rec)]
       
       [(=/= x l) (==  x r)
        (== in-both in-both-rec) (== in-l in-l-rec)
        (== in-r (cons `(,l ,r) in-r-rec))
        (== in-neither in-neither-rec)]
       
       [(=/= x l) (=/= x r)
        (== in-both in-both-rec) (== in-l in-l-rec) (== in-r in-r-rec)
        (== in-neither (cons `(,l ,r) in-neither-rec))])

     (in-pairso x rest in-both-rec in-l-rec in-r-rec in-neither-rec))])))


(define first-stcono
  (lambda (s-list t edges) ;; filter out any seen from edges
    (== s-list (cons s rest)))) ;; TODO

;;(define stcon?
;;  (lambda (edges s t)
;;    (cond
;;      [(eq? s t) #t]
;;      [else (let (
;;      	[ns (neighbors s edges)]
;;        [seen '(s)]
;;      ) (stcon-any? ns t edges seen))])))

;;(define stcon-any?
;;  (lambda (xs t edges seen)
;;    (cond
;;      [(null? xs) #f]
;;      [(stcon? 
