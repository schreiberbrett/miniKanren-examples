(load "~/miniKanren-with-symbolic-constraints/mk.scm")


;; Step 1: Write the function(s) in Scheme
(define is-even-length
  (lambda (xs)
    (cond
      [(null? xs) #t]
      [else (is-odd-length (cdr xs))])))

(define is-odd-length
  (lambda (xs)
    (cond
      [(null? xs) #f]
      [else (is-even-length (cdr xs))])))


;; Step 2: Turn the `else` clauses into explicit denials

(define is-even-length1
  (lambda (xs)
    (cond
      [(null? xs) #t]
      [(pair? xs) (is-odd-length1 (cdr xs))])))

(define is-odd-length1
  (lambda (xs)
    (cond
      [(null? xs) #f]
      [(pair? xs) (is-even-length1 (cdr xs))])))
      

;; Step 3 -- Un-nest
(define is-even-length2
  (lambda (xs)
    (cond
      [(null? xs) #t]
      [(pair? xs)
        (let [[rest (cdr xs)]]
          (is-odd-length2 rest))])))

(define is-odd-length2
  (lambda (xs)
    (cond
      [(null? xs) #f]
      [(pair? xs)
        (let [[rest (cdr xs)]]
          (is-even-length2 rest))])))
      
;; Step 4: Convert to miniKanren
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



