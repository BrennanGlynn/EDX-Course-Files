;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; dropn-starter.rkt

; 
; PROBLEM:
; 
; Design a function that consumes a list of elements lox and a natural number
; n and produces the list formed by dropping every nth element from lox.
; 
; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)
; 


;; (listOf X) Natural -> (listOf X)
;; Consumes a list of X and a natural n and drops every nth element
(check-expect (dropn (list 1 2 3 4 5 6 7) 0) empty)
(check-expect (dropn (list 1 2 3 4 5 6 7) 1) (list 1 3 5 7))
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))

(define (dropn lox0 n)
  (local [(define (dropn lox acc)
            (if (empty? lox) empty
                (if (zero? acc) (dropn (rest lox) n)
                    (cons (first lox) (dropn (rest lox) (sub1 acc))))))]
    (dropn lox0 n)))