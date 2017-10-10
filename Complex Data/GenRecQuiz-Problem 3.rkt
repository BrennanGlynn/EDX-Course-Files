;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |GenRecQuiz-Problem 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

#;
(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))

(define B4 (list "X" "O" "X"       ; A completed board
                 "O" "O" "O"
                 "X" "X" "X"))

#;
(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))

;  PROBLEM 3:
;  
;  Now adapt your solution to filter out the boards that are impossible if 
;  X and O are alternating turns. You can continue to assume that they keep 
;  filling the board after someone has won though. 
;  
;  You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;  
;  NOTE: make sure you keep a copy of your solution from problem 2 to answer 
;  the questions on edX.
;  


;; Board -> (listOf Board)
;; Returns a list of all possible boards from the given board
;; Finds empty spot and creates a new board with that spot filled by both x and y
(check-expect (all-boards B2) (list (list "X"  "X"  "O"
                                          "O"  "X"  "O"
                                          "X"  "O"  "X")))

(check-expect (all-boards B3) (list (list "X" "O" "X"
                                          "O" "O" "X"
                                          "X" "X" "O")
                                    (list "X" "O" "X"
                                          "O" "O" "O"
                                          "X" "X" "X")))

(define (all-boards bd)
  (local [(define (finish--bd bd)
            (if (full-board? bd) (list bd)
                (finish--lobd (next-boards bd))))

          (define (finish--lobd lobd)
            (if (empty? lobd) lobd
                (append (finish--bd (first lobd))
                        (finish--lobd (rest lobd)))))]
    (filter valid-board? (finish--bd bd))))

;; Board -> Boolean
;; Returns true if the board is filled
(check-expect (full-board? B1) false)
(check-expect (full-board? B4) true)

(define (full-board? bd)
  (if (empty? bd)
      true
      (if (false? (first bd))
          false
          (full-board? (rest bd)))))

;; Board -> (listOf Board)
;; Finds the first blank spot on the board and returns two new boards with "X" and "O" filled in that spot
;; ASSUME: at least one blank space is on given board
(check-expect (next-boards B2) (list (list "X" "X" "O"
                                           "O" "X" "O"
                                           "X" "X" "X")
                                     (list "X" "X" "O"
                                           "O" "X" "O" 
                                           "X" "O" "X")))

(check-expect (next-boards B3) (list (list "X" "O" "X"
                                           "O" "O" "X"
                                           "X" "X" false) 
                                     (list "X" "O" "X"
                                           "O" "O" "O"
                                           "X" "X" false)))

(define (next-boards bd)
  (add-boards (find-blank bd) bd))

;; Board -> Natural[0 8]
;; Find first blank space on a given board
;; ASSUME: board has at least one blank space
(check-expect (find-blank B3) 5)
(check-expect (find-blank B2) 7)

(define (find-blank bd)
  (if (empty? bd) (error "find-blank was given an empty list")
      (if (false? (first bd))
          0
          (+ 1 (find-blank (rest bd))))))

;; Natural[0 8] Board -> (listOf Board)
;; Consumes the position of an empty spot on a given board and returns a list of two boards with an "X" and "O" in that spot
(check-expect (add-boards 5 B3) (list (list "X"  "O"  "X"
                                            "O"  "O"  "X"
                                            "X"  "X" false) 
                                      (list "X"  "O"  "X"
                                            "O"  "O"  "O"
                                            "X"  "X" false)))
(check-expect (add-boards 7 B2) (list (list "X"  "X"  "O"
                                            "O"  "X"  "O"
                                            "X"  "X"  "X")
                                      (list "X"  "X"  "O"
                                            "O"  "X"  "O" 
                                            "X"  "O"  "X")))
(define (add-boards pos bd)
  (list (list-set bd pos "X") (list-set bd pos "O")))

;; Board -> Boolean
;; Returns true if the sum of the board is either 1 or 0.
;; Else it is an unbalanced board
;; The sum of the board is calculated by adding 1 for an X and subtracting 1 for an O
;; ASSUME: The "X" player went first
(check-expect (valid-board? B0) true)
(check-expect (valid-board? B1) true)
(check-expect (valid-board? B4) true)
(check-expect (valid-board? (list "X"  "X"  "O"
                                  "O"  "X"  "O"
                                  "X"  "X"  "X")) false)
(check-expect (valid-board? (list "X"  "X"  "O"
                                  false "X" false
                                  false false false)) false)

(define (valid-board? bd)
  (local [(define sum (sum-board bd))]
    (or (= sum 1) (= sum 0))))

;; Board -> Number
;; Returns the sum of a given board adding 1 for an X and subtracting 1 for an O
(check-expect (sum-board B0) 0)
(check-expect (sum-board B1) 0)
(check-expect (sum-board B2) 2)
(check-expect (sum-board B4) 1)
(check-expect (sum-board (list "X"  "X"  "O"
                               "O"  "X"  "O"
                               "X"  "X"  "X")) 3)

(define (sum-board bd)
  (if (empty? bd) 0
      (+ (value->number (first bd))
         (sum-board (rest bd)))))


;; Value -> Number
;; Returns 1 for "X" -1 for "O" and 0 for false
(check-expect (value->number "X")  1)
(check-expect (value->number "O") -1)
(check-expect (value->number false)  0)

(define (value->number v)
  (cond [(false? v)        0]
        [(string=? v "X")  1]
        [(string=? v "O") -1]))