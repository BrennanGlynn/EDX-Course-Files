;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname cantor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 
; PROBLEM:
; 
; A Cantor Set is another fractal with a nice simple geometry.
; The idea of a Cantor set is to have a bar (or rectangle) of
; a certain width w, then below that are two recursive calls each
; of 1/3 the width, separated by a whitespace of 1/3 the width.
; 
; So this means that the
;   width of the whitespace   wc  is  (/ w 3)
;   width of recursive calls  wr  is  (/ (- w wc) 2)
;   
; To make it look better a little extra whitespace is put between
; the bars.
; 
; PROBLEM A:
; 
; Design a function that consumes a width and produces a cantor set of 
; the given width.
;   


;;================
;; Constants:
(define WIDTH 500)
(define HEIGHT 500)
(define CUTOFF 5)
(define BAR-HEIGHT 20)
(define WHITE-HEIGHT (/ BAR-HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))

;;================
;; Functions:

;; Number -> Image
;; Consume a number and produce a cantor set with that width
(check-expect (cantor CUTOFF) (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))
(check-expect (cantor (* CUTOFF 3))
              (above (rectangle (* CUTOFF 3) BAR-HEIGHT "solid" "blue")
                     (rectangle (* CUTOFF 3) WHITE-HEIGHT "solid" "white")
                     (local [(define sub-bar (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))]
                       (beside sub-bar (rectangle CUTOFF BAR-HEIGHT "solid" "white") sub-bar))))

(define (cantor w)
  (if (<= w CUTOFF)
      (rectangle w BAR-HEIGHT "solid" "blue")
      (above (rectangle w BAR-HEIGHT "solid" "blue")
             (rectangle w WHITE-HEIGHT "solid" "white")
             (local [(define sub-bar (cantor (/ w 3)))]
               (beside sub-bar (rectangle (/ w 3) BAR-HEIGHT "solid" "white") sub-bar)))))

; 
; PROBLEM B:
; 
; Add a second parameter to your function that controls the percentage 
; of the recursive call that is white each time. Calling your new function
; with a second argument of 1/3 would produce the same images as the old 
; function.
; 


;; Number Number -> Image
;; Consume a width and a percentage and produce a cantor set with white space taking up the given percantage of the screen
(check-expect (cantor-b CUTOFF 1) (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))
(check-expect (cantor-b (* CUTOFF 3) (/ 1 3))
              (above (rectangle (* CUTOFF 3) BAR-HEIGHT "solid" "blue")
                     (rectangle (* CUTOFF 3) WHITE-HEIGHT "solid" "white")
                     (local [(define sub-bar (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))]
                       (beside
                        sub-bar (rectangle CUTOFF BAR-HEIGHT "solid" "white") sub-bar))))
(check-expect (cantor-b (* CUTOFF 4) (/ 1 2))
              (above (rectangle (* CUTOFF 4) BAR-HEIGHT "solid" "blue")
                     (rectangle (* CUTOFF 4) WHITE-HEIGHT "solid" "white")
                     (local [(define sub-bar (rectangle CUTOFF BAR-HEIGHT "solid" "blue"))]
                       (beside
                        sub-bar (rectangle (* CUTOFF 2) BAR-HEIGHT "solid" "white") sub-bar))))

(define (cantor-b w p)
  (if (<= w CUTOFF)
      (rectangle w BAR-HEIGHT "solid" "blue")
      (above (rectangle w BAR-HEIGHT "solid" "blue")
             (rectangle w WHITE-HEIGHT "solid" "white")
             (local [(define sub-bar (cantor-b (/ (- w (* w p)) 2) p))]
               (beside
                sub-bar (rectangle (* w p) BAR-HEIGHT "solid" "white") sub-bar)))))


; 
; PROBLEM C:
; 
; Now you can make a fun world program that works this way:
;   The world state should simply be the most recent x coordinate of the mouse.
;   
;   The to-draw handler should just call your new cantor function with the
;   width of your MTS as its first argument and the last x coordinate of
;   the mouse divided by that width as its second argument.
;   


;; WS -> WS
;; start the world with (main 0)
;; 
(define (main ws)
  (big-bang ws                         ; Number
            (on-tick   tock)           ; Number -> Number
            (to-draw   render)         ; Number -> Image
            (on-mouse  handle-mouse))) ; Number Integer Integer MouseEvent -> Number

;; Number -> Number
;; produce the next world state
(define (tock ws) ws)


;; Number -> Image
;; render a cantor where the width is WIDTH and the percent of whitespace is (ws divided by width)
(define (render ws)
  (cantor-b WIDTH (/ ws WIDTH)))

;; Number Integer Integer MouseEvent -> Number
;; Update WS when mouse moves
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "move") x]
        [else ws]))