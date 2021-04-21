(load "mk.scm")

(define appendo (lambda (a b out) ;; (append a b) == out
    (conde
        [(== a '()) (== b out)]
        [(fresh (x xs out2)
            (== a (cons x xs))
            (== (cons x out2) out)
            (appendo xs b out2))])))

(define append-allo (lambda (l1 out) ;; (append-all l1) == out
    (conde
        [(== l1 '()) (== out '())]
        [(fresh (x xs out2)
            (== l1 (cons x xs))
            (appendo x out2 out)
            (append-allo xs out2))])))

(define snoco (lambda (xs x out) ;; (reverse (cons x (reverse xs))) == out
    (conde
        [(== xs '()) (== out (cons x '()))]
        [(fresh (first xs-rest out-rest)
            (== xs  (cons first  xs-rest))
            (== out (cons first out-rest))
            (snoco xs-rest x out-rest))])))

(define same-lengtho (lambda (xs ys) ;; (length xs) == (length ys)
    (conde
        [(== xs '()) (== ys '())]
        [(fresh (xs-first xs-rest ys-first ys-rest)
            (== xs (cons xs-first xs-rest))
            (== ys (cons ys-first ys-rest))
            (same-lengtho xs-rest ys-rest))])))

(define reverse-and-appendo (lambda (xs ys out) ;; (append (reverse xs) ys) == out
    (conde
        [(== xs '()) (== ys out)]
        [(fresh (xs-first xs-rest)
            (== xs (cons xs-first xs-rest))
            (reverse-and-appendo xs-rest (cons xs-first ys) out))])))

(define reverseo (lambda (xs ys) ;; (reverse xs) == ys
    (reverse-and-concato xs '() ys)))

(define prefixo (lambda (prefix full)
    (fresh (suffix) (appendo prefix suffix full))))

(define suffixo (lambda (suffix full)
    (fresh (prefix) (appendo prefix suffix full))))

(define infixo (lambda (needle haystack)
    (fresh (prefix suffix) (append-allo (list prefix needle suffix) haystack))))

(define subsequenceo (lambda (xs ys)
    (conde
        [(== xs '())]
        [(fresh (xs-first xs-rest ys-first ys-rest)
            (== xs (cons xs-first xs-rest))
            (== ys (cons ys-first ys-rest))
            (conde
                [(== xs-first ys-first) (subsequenceo xs-rest ys-rest)]
                [(subsequenceo xs ys-rest)]))])))

(define containso (lambda (needle haystack)
    (fresh (prefix suffix)
        (appendo prefix (cons needle suffix) haystack))))

(define contains-allo (lambda (needles haystack)
    (conde
        [(== needles '())]
        [(fresh (first-needle rest-needles)
            (== needles (cons first-needle rest-needles))
            (containso first-needle haystack)
            (contains-allo rest-needles haystack))])))

(define not-containso (lambda (needle haystack)
    (conde
        [(== haystack '())]
        [(fresh (first rest)
            (== haystack (cons first rest))
            (=/= needle first)
            (not-containso needle rest))])))

(define contains-exactly-onceo (lambda (needle haystack)
    (fresh (prefix suffix)
        (appendo prefix (cons needle suffix) haystack)
        (not-containso needle prefix)
        (not-containso needle suffix))))

(define contains-each-exactly-onceo (lambda (needles haystack)
    (conde
        [(== needles '())]
        [(fresh (first rest)
            (== needles (cons first rest))
            (contains-exactly-onceo first haystack)
            (contains-each-exactly-onceo rest haystack))])))

(define permutationo (lambda (xs ys)
    (conde
        [(== xs '()) (== ys '())]
        [(fresh (first rest prefix suffix without-first)
            (== xs (cons first rest))
            (appendo prefix (cons first suffix) ys)
            (appendo prefix suffix without-first)
            (permutationo rest without-first))])))
