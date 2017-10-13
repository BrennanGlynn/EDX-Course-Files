;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname accumulators-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;  PROBLEM 1:
;  
;  Design a function that consumes a list of strings, and produces the length 
;  of the longest word in the list. 
;  


;; (listOf String) -> Integer
;; Returns the length of the longest word in a given list
(check-expect (longest empty) 0)
(check-expect (longest (list "1" "22")) 2)

(define (longest los)
  (local [(define (longest los rsf)
            (if (empty? los) rsf
                (longest (rest los) (if (> (string-length (first los)) rsf)
                                        (string-length (first los))
                                        rsf))))]
    (longest los 0)))



;  PROBLEM 2:
;  
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is 
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to 
;  n-2 + n-1. 
;  
;  Design a function that given a list of numbers at least two elements long, 
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every 
;  element in the list. The sequence does not have to start at zero, so for 
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule. 
;  


;; (listOf Number) -> Boolean
;; Returns true if the sequence follows the fibonacci sequence
(check-expect (fibbonacci? (list 1 2)) true)
(check-expect (fibbonacci? (list 1 2 4)) false)
(check-expect (fibbonacci? (list 1 2 3 5 8)) true)
(check-expect (fibbonacci? (list 1 2 3 4 5)) false)
(check-expect (fibbonacci? (list 4 5 9 14 23)) true)

(define (fibbonacci? lon)
  (local [(define (fibbonacci? lon nums i)
            (if (empty? lon) true
                (if (or (= (first lon) (+ (first nums) (first (rest nums))))
                        (< i 2))
                    (fibbonacci? (rest lon) (list (first lon) (first nums)) (add1 i))
                    false)))]
    (fibbonacci? lon (list 0 0) 0)))




;  PROBLEM 3:
;  
;  Refactor the function below to make it tail recursive.  
;  


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))

(define (fact n)
  (local [(define (fact n rsf)
            (if (zero? n) rsf
                (fact (sub1 n) (* n rsf))))]
    (fact n 1)))



;  PROBLEM 4:
;  
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist 
;  accumulator to design a tail recursive function that counts the number of regions 
;  within and including a given region. 
;  So (count-regions CANADA) should produce 7



(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))

;; Region -> Number
;; Counts the number of regions within and including a given region
(check-expect (count-regions VANCOUVER) 1)
(check-expect (count-regions CANADA) 7)

(define (count-regions r)
  (local [(define (fn-for-region r rsf todo)
            (fn-for-lor (append (region-subregions r) todo) (add1 rsf)))
          
          (define (fn-for-lor lor rsf)
            (if (empty? lor) rsf
                (fn-for-region (first lor) rsf (rest lor))))]
    (fn-for-region r 0 empty)))




