#lang racket

; Prüft, ob in einer nicht verschachtelten Liste die gesuchte Zeichenkette vorhanden ist
; der Default-Wert von f ist eine leere Liste
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

; Gibt alle Kanten zurück, die den übergebenen Knoten beinhalten.
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

; Gibt die Gewichte aller Kanten in einer Liste in einer Liste zurück.
(define (getIndizes liste)
  (if
   (> (length liste) 0)
   (cons (caar liste) (getIndizes (cdr liste)))
   '()
   )
  )

; Diese Funktion sortiert die Kanten in einer Liste nach dem Gewicht genannter Kanten aufsteigend.
(define (sortList liste [ergebnisliste '()])
  (cond
    ((> (length liste) 0)
     (sortList (remove (assoc (car (sort (getIndizes liste) <)) liste) liste) (append ergebnisliste(list (assoc (car (sort (getIndizes liste) <)) liste))))
     )
    (else
     ergebnisliste
     )
    ))

(define (getSentence liste [string ""])
  (cond
    ((>= (length liste) 1) (getSentence (cdr liste) (string-append string " " (symbol->string (cadddr (car liste))))))
    (else
     (substring string 1))
    )
  )

; Diese Funktion liefert alle Wörter eines Strings in einer Liste in der richtigen Reihenvolge zurück.
(define (getWords sentence [liste '()] [position 0] [string2 ""])
  (cond
    ((= (string-length sentence) position) (append liste (list string2)))
    ((equal? (string-ref sentence position) #\space) (getWords sentence (append liste (list string2)) (+ position 1)))
    (else
     (getWords sentence liste (+ position 1) (string-append string2 (string (string-ref sentence position))))
     )
    )
  )

; Diese Funktion setzt Strings in einer Liste zu einem String zusammen.
(define (wordsToSentence liste [result ""])
  (cond
    ((>= (length liste) 1) (wordsToSentence (cdr liste) (string-append result " " (car liste))))
    (else
     (substring result 1))
    )
  )

; Diese Funktion übersetzt den übergebenen String.
(define (translate satz liste [result ""] [previousNode 'S] [endzeichen "."])
  (cond
    ((equal? result "") (displayln (string-append "Ausgangssatz : " satz)))
    )
  (cond
    ((equal? result "") (translate (wordsToSentence (cdr (getWords (substring satz 0 (- (string-length satz) 1))))) liste (getSentence (list (car (pathsWithNode (pathsWithNode liste previousNode) (string->symbol (car (getWords satz))))))) (string->symbol (car (getWords satz))) (substring satz (- (string-length satz) 1))))
    ((>= (length (getWords satz)) 2) (translate (wordsToSentence (cdr (getWords satz))) liste (string-append result " " (getSentence (list (car (pathsWithNode (pathsWithNode liste previousNode) (string->symbol (car (getWords satz)))))))) (string->symbol (car (getWords satz))) endzeichen))
    ((and (= (length (getWords satz)) 1) (not (= (string-length satz) 0))) (translate "" liste (string-append result " " (getSentence (list (car (pathsWithNode (pathsWithNode liste previousNode) (string->symbol (car (getWords satz)))))))) (string->symbol (car (getWords satz))) endzeichen))
    (else
     (displayln (string-append "Übersetzung  : " result endzeichen))
     ))
  )

(define ausgangsliste0 '((100 S Ich I)(100 S Sie She)(100 S Er He)(100 Sie mag likes)(100 Sie liebt loves)(100 liebt Burger loves)(100 Ich mag like)(100 Er mag likes)(100 Du magst like)(100 Er mag likes)(100 magst Burger burger)(100 mag Burger burger)(100 Ich liebe love)(100 Ich hasse hate)(100 hasse Burger burger)(100 liebe Burger burger)))
(translate "Ich liebe Burger!" ausgangsliste0)