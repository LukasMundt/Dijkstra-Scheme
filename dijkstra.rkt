
#lang racket

(define ausgangsliste '((S N 5)(S M 2)(N M 3)(N Z 3)(M Z 7)))
(define ausgangsliste2 '((5 S N)(2 S M)(3 N M)(3 N Z)(7 M Z)))
(define ausgangsliste3 '((3 S A)(9 S B)(2 B C)(4 A C)(3 C E)(1 C D)(1 E Z)(9 D Z)))

; Prüft, ob in einer nicht verschachtelten Liste die gesuchte Zeichenkette vorhanden ist
(define (containsNode haystack needle [f '()]) ; der Default-Wert von f ist eine leere Liste
  (if
   (equal? (car haystack) needle)
   (append f haystack)
   (if
    (> (length haystack) 1)
    (containsNode (cdr haystack) needle (append f (list (car haystack))))
    #f
    ))
  )

(define (pathsWithNode liste node)
  (if
   (> (length liste) 0)
   (if
    (containsNode (car liste) node)
    (cons (containsNode (car liste) node) (pathsWithNode (cdr liste) node))
    '()
    )
   '()
   )
  )

(define (getIndizes liste)
  (if
   (> (length liste) 0)
   (cons (caar liste) (getIndizes (cdr liste)))
   '()
   )
  )

(define (sortList liste [ergebnisliste '()])
  (cond
    ((> (length liste) 0)
     (sortList (remove (assoc (car (sort (getIndizes liste) <)) liste) liste) (append ergebnisliste(list (assoc (car (sort (getIndizes liste) <)) liste))))
     )
    (else
     ergebnisliste
     )
    ))

(define (listContainsPathWithNode liste node)
  (cond
    ((> (length liste) 1) (listContainsPathWithNode (cdr liste) node))
    ((empty? liste) #f)
    ((not (equal? (containsNode (car liste) node) #f)) #t)
    (else #f)
    )
  )

(define (getNodeWhichIsNot path notNode [result #f])
  (cond
    ((and (not (number? (car path))) (= (string-length (symbol->string (car path))) 1) (not (equal? (car path) notNode))) (car path))
    ((and (= (length path) 0) (or (= (length result) 1) (equal? result #f))) result)

    (else (getNodeWhichIsNot (cdr path) notNode))
    )
  )

(define (funktion liste start end [templiste '()] [endliste '()])
  (cond
    ((equal? start end) endliste)
    (else
     (funktion (remove* (pathsWithNode liste start) liste) (getNodeWhichIsNot (car (sortList (pathsWithNode liste start))) start) end (car (sortList (pathsWithNode liste start))) (append endliste (list (car (sortList (pathsWithNode liste start)))))))
    )

  )
(funktion ausgangsliste2 'S 'Z)

