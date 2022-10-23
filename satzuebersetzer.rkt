
#lang racket

(define ausgangsliste0 '((100 S Ich I)(100 Ich liebe love)(100 Ich hasse hate)(100 hasse Burger burger)(100 liebe Burger burger)))
(define ausgangsliste '((Ich N 5)(Ich M 2)(N M 3)(N Z 3)(M Z 7)))
(define ausgangsliste2 '((5 S N)(2 S M Ich)(3 N M)(3 N Z)(7 M Z)))
(define ausgangsliste5 '((5 S N)))
(define ausgangsliste3 '((3 Ich "A" "Hallo")(9 Ich B Hallo)(2 B C Halo)(4 A C)(3 C E)(1 C D)(1 E Z)(9 D Z)))
(define ausgangsliste4 '((5 Ich N Hallo)(2 Ich M Hallo)(3 N M Hallo)(3 N Z Hallo)(7 M Z Hallo)))
(define ausgangsliste6 '((1 S T Hallo)(2 T U Hallo)(99 U Z Hallo)(100 S Z Hallo)))
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

(define (getWeightOfPaths liste [weight 0])
  (cond
    ((= (length liste) 1) (+ (car (getIndizes liste)) weight))
    (else
     (getWeightOfPaths (cdr liste) (+ (car (getIndizes liste)) weight))
     )
    )
  )

(define (funktion liste start end [templiste '()] [endliste '()])
  (display endliste)
  (display "-------")
  (cond
    ((equal? start end) endliste) ;endweder Endbedingung oder Verfahren überdenken, wenn ein kürzerer Pfad gefunden wird, muss das Ergebnis von zuvor revidiert werden, ggf. Funktion, die das Gewicht der Pfade addiert nutzen
    ; ((= number 10) endliste)
    (else
     (funktion (remove* (pathsWithNode liste start) liste) (getNodeWhichIsNot (car (sortList (pathsWithNode liste start))) start) end (car (sortList (pathsWithNode liste start))) (append endliste (list (car (sortList (pathsWithNode liste start)))))))
    )
  )

(define (erhoeheGewicht path gewicht)
  (cons (+ (car path) gewicht) (cdr path))
  )

(define (listOfPathContainsLastStart liste lastStart [index 0])
  (cond
    (listContainsPathWithNode (car (reverse (car liste))) index)
    ((empty? liste) #f)
    (else
     (listOfPathContainsLastStart (cdr liste) lastStart (+ index 1)))
    )
  )
; (listOfPathContainsLastStart '(((1 S T)(3 T U)) ((100 S Z))) 'T)

(define (getWeightOfWholePath wholePath)
  (caar (reverse wholePath))
  )
; (getWeightOfWholePath '((1 S T)(3 T U)))

; (define (funktion2 liste start end [weight 0] nodes)
;   ; (display weight)
;   ; (display templiste)
;   ; (display (erhoeheGewicht (car (sortList (pathsWithNode liste start))) weight))
;   (cond
;     ((equal? start end) endliste) ;endweder Endbedingung oder Verfahren überdenken, wenn ein kürzerer Pfad gefunden wird, muss das Ergebnis von zuvor revidiert werden, ggf. Funktion, die das Gewicht der Pfade addiert nutzen
;     ; ((and (equal? tempstart end) (not (empty? templiste))) (funktion2 liste start end (getNodeWhichIsNot (car templiste) tempstart) (cdr templiste) endliste))
;     (else
;      (funktion2
;       (remove* (pathsWithNode liste start) liste)
;       (getNodeWhichIsNot (car (sortList (append (pathsWithNode liste start)))) start)
;       end
;       (+ weight (caar (sortList (append templiste (pathsWithNode liste start)))))
;       (cdr (sortList (append templiste (list (erhoeheGewicht (car (pathsWithNode liste start)) weight)) (cdr (pathsWithNode liste start)))))
;       (cond
;       ()
;       (append endliste (list (car (sortList (append templiste (list (erhoeheGewicht (car (pathsWithNode liste start)) weight)) (cdr (pathsWithNode liste start)))))))
;       )
;       ))
;     )
;   )
(define (fkt pfade knoten [endknoten '()])
; (display knoten)
; (display "---------")
  (cond
    ((empty? knoten) endknoten)
    ((= (caar knoten) 0) (fkt pfade (cdr knoten) (list (car knoten))))
    ((equal? (listContainsPathWithNode pfade (cadr (cadar knoten))) #f) (fkt pfade (cdr knoten) (append endknoten (list (car knoten)))))
    ((< (caar (pathsWithNode pfade (cadr (cadar knoten)))) (caar knoten)) (fkt pfade (cdr knoten) (append endknoten (list (cons (caar (pathsWithNode pfade (cadr (cadar knoten)))) (cdar knoten))))))
    ((> (caar (pathsWithNode pfade (cadr (cadar knoten)))) (caar knoten)) (fkt pfade (cdr knoten) (append endknoten (list (car knoten)))))
    (else endknoten)
    )
  )
; (fkt (pathsWithNode ausgangsliste6 'S) '((0 'S '-)(+inf.0 'T '-)(+inf.0 'U '-)(+inf.0 'Z '-)))
; (sortList '((0 'S '-)(+inf.0 'T '-)(+inf.0 'U '-)(+inf.0 'Z '-)))

(define (funktion3 liste start end nodes tempstart [weight 0] [endliste '()])
  (cond
    ; ((equal? start tempstart) (funktion3 liste start end nodes (cons (cons 0 (cdar nodes))(cdr nodes)))
    ((equal? tempstart end) endliste) ;endweder Endbedingung oder Verfahren überdenken, wenn ein kürzerer Pfad gefunden wird, muss das Ergebnis von zuvor revidiert werden, ggf. Funktion, die das Gewicht der Pfade addiert nutzen
    ; ((and (equal? tempstart end) (not (empty? templiste))) (funktion2 liste start end (getNodeWhichIsNot (car templiste) tempstart) (cdr templiste) endliste))
    (else
     (funktion3
      (remove* (pathsWithNode liste start) liste)
      start
      end
      (sortList (fkt (pathsWithNode liste tempstart) nodes)) ; ab hier muss diese Funktion bearbeitet werden
      (cadr (cadar (sortList (fkt (pathsWithNode liste tempstart) nodes))))

      ; (cdr (sortList (append templiste (list (erhoeheGewicht (car (pathsWithNode liste start)) weight)) (cdr (pathsWithNode liste start)))))
      
      ))
    )
  )

(funktion3 ausgangsliste6 'S 'Z '((+inf.0 'S '-)(+inf.0 'T '-)(+inf.0 'U '-)(+inf.0 'Z '-)) 'S)
; (cons (cons 0 (cdar '((+inf.0 'S '-)(+inf.0 'T '-)(+inf.0 'U '-)(+inf.0 'Z '-))))(cdr '((+inf.0 'S '-)(+inf.0 'T '-)(+inf.0 'U '-)(+inf.0 'Z '-))))
; (car (reverse '((1 S T Hallo) (3 T U Hallo) (100 S Z Hallo))))
; (infinite? +inf.0)

(define (getSentence liste [string ""])
  (cond
    ((>= (length liste) 1) (getSentence (cdr liste) (string-append string " " (symbol->string (cadddr (car liste))))))
    (else
     (substring string 1))
    )
  )

(define (getWords sentence [liste '()] [position 0] [string2 ""])
  (cond
    ((= (string-length sentence) position) (append liste (list string2)))
    ((equal? (string-ref sentence position) #\space) (getWords sentence (append liste (list string2)) (+ position 1)))
    (else
     (getWords sentence liste (+ position 1) (string-append string2 (string (string-ref sentence position))))
     )
    )
  )

(define (wordsToSentence liste [result ""])
  (cond
    ((>= (length liste) 1) (wordsToSentence (cdr liste) (string-append result " " (car liste))))
    (else
     (substring result 1))
    )
  )

(define (translate satz liste [result ""])
  (display satz)
  (display "-")
  (display result)
  (display "------")
  (cond
    ((equal? result "") (translate satz liste (getSentence (funktion liste 'S (string->symbol (car (getWords satz)))))))
    ((>= (length (getWords satz)) 2) (string-append result " " (translate (wordsToSentence (cdr (getWords satz))) liste (getSentence (funktion liste (string->symbol (car (getWords satz))) (string->symbol (cadr (getWords satz))))))))
    ; ((= (length (getWords satz)) 1) (translate "" liste (getSentence (funktion liste (string->symbol satz) 'Z))))
    (else
     result)
    )
  )

; (getSentence (funktion ausgangsliste0 'S 'Z))

; (translate "Ich liebe Burger" ausgangsliste0)
; (funktion2 ausgangsliste6 'S 'Z)
; (funktion2 ausgangsliste0 'Ich 'liebe)
; (append '((Hallo)) '(Hallo) '(Hallo))

; Problem: wenn später noch ein kürzerer Pfad gefunden wird, wird dies nicht übernommen
; die Sortierung nach dem Gewicht einer Kante ist bei vielen Kanten, mit gleichem Gewicht nicht zielführend