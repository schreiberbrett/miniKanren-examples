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
