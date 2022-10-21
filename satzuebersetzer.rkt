
#lang racket

(define ausgangsliste '((Ich N 5)(Ich M 2)(N M 3)(N Z 3)(M Z 7)))
(define ausgangsliste2 '((5 S N)(2 S M Ich)(3 N M)(3 N Z)(7 M Z)))
(define ausgangsliste5 '((5 S N)))
(define ausgangsliste3 '((3 Ich A Hallo)(9 Ich B Hallo)(2 B C Halo)(4 A C)(3 C E)(1 C D)(1 E Z)(9 D Z)))
(define ausgangsliste4 '((5 Ich N Hallo)(2 Ich M Hallo)(3 N M Hallo)(3 N Z Hallo)(7 M Z Hallo)))
; (define ausgangsliste4 '((100 S A)(100 S B)(100 S C)(100 A D)(100 A G)(100 A H)(100 A I)(100 B F)(100)))

; Prüft, ob in einer nicht verschachtelten Liste die gesuchte Zeichenkette vorhanden ist
; der Default-Wert von f ist eine leere Liste
; Diese Funktion unterstützt Nodes mit mehr als einem Buchstaben.
(define (containsNode haystack needle [f '()])
  (if
   (equal? (car haystack) needle)
   (append f haystack)
   (if
    (> (length haystack) 1)
    (containsNode (cdr haystack) needle (append f (list (car haystack))))
    #f
    ))
  )

; Diese Funktion unterstützt Nodes mit mehr als einem Buchstaben.
(define (pathsWithNode liste node [endlist '()])
  (if
   (> (length liste) 0)
   (if
    (containsNode (car liste) node)
    (pathsWithNode (cdr liste) node (append (list (car liste)) endlist))
    (pathsWithNode (cdr liste) node endlist)
    )
   endlist
   )
  )

; Diese Funktion unterstützt Nodes mit mehr als einem Buchstaben.
(define (getIndizes liste)
  (if
   (> (length liste) 0)
   (cons (caar liste) (getIndizes (cdr liste)))
   '()
   )
  )

; Diese Funktion sortiert die Kanten in einer Liste nach dem Gewicht genannter Kanten aufsteigend.
; Diese Funktion unterstützt Nodes mit mehr als einem Buchstaben.
(define (sortList liste [ergebnisliste '()])
  (cond
    ((> (length liste) 0)
     (sortList (remove (assoc (car (sort (getIndizes liste) <)) liste) liste) (append ergebnisliste(list (assoc (car (sort (getIndizes liste) <)) liste))))
     )
    (else
     ergebnisliste
     )
    ))

; Diese Funktion liefert einen Wahrheitswert, ob sich in einer Liste von
; Pfaden ein Pfad befindet, der einen bestimmten Knoten mit einbezieht.
; Diese Funktion unterstützt Nodes mit mehr als einem Buchstaben.
(define (listContainsPathWithNode liste node)
  (if
   (equal? (pathsWithNode liste node) '())
   #f
   #t
   ))
; Diese Funktion liefert die beiden Nodes, welche durch einen Pfad verbunden werden in einer Liste.
; Diese Funktion unterstützt Pfade mit Knoten mit mehr als einem Buchstaben und dem folgenden Format: '(Gewicht Knoten Knoten Beschriftung)
(define (getNodesOfPath liste [endliste '()])
  (cond
    ((= (length liste) 4) (getNodesOfPath (cdr liste)))
    ((= (length liste) 3) (getNodesOfPath (cdr liste) (list (car liste))))
    ((= (length liste) 2) (getNodesOfPath (cdr liste) (append endliste (list (car liste)))))
    (else
     endliste
     )
    )
  )
; Diese Funktion liefert bei der Angabe eines Knotens eines Pfades den anderen der beiden Knoten.
(define (getNodeWhichIsNot path notNode)
  (cond
    ((not (equal? (car (getNodesOfPath path)) notNode)) (car (getNodesOfPath path)))
    ((not (equal? (cadr (getNodesOfPath path)) notNode)) (cadr (getNodesOfPath path)))
    (else #f)
    )
  )


(define (funktion liste start end [templiste '()] [endliste '()])
  (cond
    ((equal? start end) endliste)
    (else
     (funktion (remove* (pathsWithNode liste start) liste) (getNodeWhichIsNot (car (sortList (pathsWithNode liste start))) start) end (car (sortList (pathsWithNode liste start))) (append endliste (list (car (sortList (pathsWithNode liste start)))))))
    )
  )

(define (getSentence liste [string ""])
(cond
((>= (length liste) 1) (getSentence (cdr liste) (string-append string " " (symbol->string (cadddr (car liste))))))
(else
(substring string 1))
)
)

(getSentence (funktion ausgangsliste4 'Ich 'Z))