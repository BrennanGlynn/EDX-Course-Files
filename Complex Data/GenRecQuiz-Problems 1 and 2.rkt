;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |GenRecQuiz-Problems 1 and 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/list)
(require 2htdp/image)

;  PROBLEM 1:
;  
;  In the lecture videos we designed a function to make a Sierpinski triangle fractal. 
;  
;  Here is another geometric fractal that is made of circles rather than triangles:
;  
;  .
;  
;  Design a function to create this circle fractal of size n and colour c.
;  


(define CUTOFF 5)

;; Natural String -> Image
;; produce a circle fractal of size n and colour c
(check-expect (circle-fractal CUTOFF "black") (circle CUTOFF "outline" "black"))
(check-expect (circle-fractal (* CUTOFF 2) "black")
              (overlay (circle (* CUTOFF 2) "outline" "black")
                       (local [(define sub (circle CUTOFF "outline" "black"))]
                         (beside sub sub))))

(define (circle-fractal n c)
  (if (<= n CUTOFF) (circle n "outline" c)
      (overlay (circle n "outline" c)
               (local [(define sub (circle-fractal (/ n 2) c))]
                 (beside sub sub)))))

;  PROBLEM 2:
;  
;  Below you will find some data definitions for a tic-tac-toe solver. 
;  
;  In this problem we want you to design a function that produces all 
;  possible filled boards that are reachable from the current board. 
;  
;  In actual tic-tac-toe, O and X alternate playing. For this problem
;  you can disregard that. You can also assume that the players keep 
;  placing Xs and Os after someone has won. This means that boards that 
;  are completely filled with X, for example, are valid.
;  
;  Note: As we are looking for all possible boards, rather than a winning 
;  board, your function will look slightly different than the solve function 
;  you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
;  lecture questions. 
;  


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

;; Board -> (listOf Board)
;; Returns a list of all possible boards from the given board
;; Finds empty spot and creates a new board with that spot filled by both x and y
(check-expect (all-boards B2) (list (list "X"  "X"  "O"
                                          "O"  "X"  "O"
                                          "X"  "X"  "X")
                                    (list "X"  "X"  "O"
                                          "O"  "X"  "O" 
                                          "X"  "O"  "X")))
(check-expect (all-boards B3) (list (list "X" "O" "X"
                                          "O" "O" "X"
                                          "X" "X" "X")
                                    (list "X" "O" "X"
                                          "O" "O" "X"
                                          "X" "X" "O")
                                    (list "X" "O" "X"
                                          "O" "O" "O"
                                          "X" "X" "X")
                                    (list "X" "O" "X"
                                          "O" "O" "O"
                                          "X" "X" "O")))

(define (all-boards bd)
  (local [(define (finish--bd bd)
            (if (full-board? bd) (list bd)
                (finish--lobd (next-boards bd))))

          (define (finish--lobd lobd)
            (if (empty? lobd) lobd
                (append (finish--bd (first lobd))
                        (finish--lobd (rest lobd)))))]
    (finish--bd bd)))

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



