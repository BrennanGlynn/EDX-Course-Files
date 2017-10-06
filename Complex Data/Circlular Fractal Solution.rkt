;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Circlular Fractal Solution|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 10)

;; =================
;; Functions:

;; Number -> Image
;; Consumes a number and produces a circle of that size surrounded by smaller circles surrounded by smaller circles
;; Stops surrounding the circles when they are smaller than TRIVIAL-SIZE
(check-expect (fractal-circ (sub1 TRIVIAL-SIZE))
              (local [(define leaf (leaf-helper (* (sub1 TRIVIAL-SIZE) STEP)))]
                (above leaf
                       (beside (rotate 90 leaf)
                               (circle (sub1 TRIVIAL-SIZE) "solid" "blue")
                               (rotate 270 leaf))
                       (rotate 180 leaf))))

(check-expect (fractal-circ TRIVIAL-SIZE)
              (local [(define leaf (leaf-helper (* TRIVIAL-SIZE STEP)))]
                (above leaf
                       (beside (rotate 90 leaf) (circle TRIVIAL-SIZE "solid" "blue") (rotate 270 leaf))
                       (rotate 180 leaf))))

(define (fractal-circ n)
  (local [(define leaf (leaf-helper (* n STEP)))]
    (above leaf
           (beside (rotate 90 leaf) (circle n "solid" "blue") (rotate 270 leaf))
           (rotate 180 leaf))))

;  Termination Argument: leaf-helper
; 
; Base Case: (< n TRIVIAL-SIZE)
; 
; Reduction Step: (* n STEP)
; 
; As long as: TRIVIAL SIZE is > 0 &&
;             STEP is between 0 and 1 &&
;             n >= 0
; Repeated multiplication of n and step will eventually be less than CUTOFF


;; Number -> Image
;; Returns the top leaf of a fractal circle
(check-expect (leaf-helper (sub1 TRIVIAL-SIZE)) (circle (sub1 TRIVIAL-SIZE) "solid" "blue"))
(check-expect (leaf-helper TRIVIAL-SIZE)
              (local [(define leaf (circle (* TRIVIAL-SIZE STEP) "solid" "blue"))]
                (above leaf
                       (beside (rotate  90 leaf)
                               (circle TRIVIAL-SIZE "solid" "blue")
                               (rotate 270 leaf)))))

(define (leaf-helper n)
  (if (< n TRIVIAL-SIZE)
      (circle n "solid" "blue")
      (local [(define leaf (leaf-helper (* n STEP)))]
        (above leaf
               (beside (rotate 90 leaf)
                       (circle n "solid" "blue")
                       (rotate 270 leaf))))))


