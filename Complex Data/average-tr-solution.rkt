;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname average-tr-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; average-starter.rkt

; 
; PROBLEM:
; 
; Design a function called average that consumes (listof Number) and produces the
; average of the numbers in the list.
; 


;; (listOf Number) -> Number
;; Consumes a number and produces the average of the numbers in the list
(check-expect (average (list 10 11 12)) 11)
(check-expect (average (list 1 3)) 2)

(define (average lox0)
  (local [(define (average lox acc1 acc2)
            (if (empty? lox) (/ acc1 acc2)
                (average (rest lox) (+ (first lox) acc1) (add1 acc2))))]
    (average lox0 0 0)))
