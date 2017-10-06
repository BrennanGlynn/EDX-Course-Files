;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname SPACE-INVADERS-COMPLETED) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;;================================================================================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 10)
(define MISSILE-SPEED 10)

(define HIT-RANGE 15)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;;================================================================================================================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dx))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))                            ;center going right
(define T1 (make-tank 50 1))                                     ;going right
(define T2 (make-tank 50 -1))                                    ;going left
(define T3 (make-tank 0 1))                                      ;on left wall
(define T4 (make-tank WIDTH -1))                                 ;on right wall


#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))                  ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))              ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10))        ;> landed, moving right
(define I4 (make-invader (+ WIDTH 10) 300 12))         ;on right wall, moving right
(define I5 (make-invader 10 300 -12))                  ;on left wall, moving left


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 15)))  ;exactly hitting I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1
(define M4 (make-missile 150 5))                                 ;half off the screen
(define M5 (make-missile 200 -5))                                ;almost completely off the screen
(define M6 (make-missile 250 -10))
(define M7 (make-missile (invader-x I5) (+ (invader-y I5) 15)))  ;exactly hitting I5
(define M8 (make-missile (invader-x I4) (+ (invader-y I4)  5)))  ;> hit I4

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfInvaders is one of:
;;  - empty
;;  - (cons Invader ListOfInvaders)
;; interp. a list of Invaders
(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (list I1 I2 I3)) ;game over
(define LOI4 (list I1 I2))
(define LOI5 (list I1 I4 I5))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; ListOfMissiles is one of:
;;  - empty
;;  - (cons Invader ListOfMissiles)
;; interp. a list of Missiles
(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (list M1 M2 M3))
(define LOM4 (list M1 M2))
(define LOM5 (list M4 M5 M6))
(define LOM6 (list M5 M7 M8))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game LOI3 (list M1 M2) T1))
(define G5 (make-game LOI4 LOM4 T1))
(define G6 (make-game LOI2 LOM5 T1)) 

;;================================================================================================================
;; Functions:

;; Main
;; run with (main G0)
(define (main ws)
  (big-bang ws                           ; WS
            (on-tick    advance-game)    ; WS -> WS
            (to-draw    render-game)     ; WS -> Image
            (stop-when  game-over?)      ; WS -> Boolean
            (on-key     handle-key)))    ; WS KeyEvent -> WS

;; advance-game
;; WS -> WS
;; Consumes a world state and produces the next world state advancing the invaders and missiles
;;(define (advance-game ws) G0)

(define (advance-game ws)
  (make-game (update-invaders (game-invaders ws) (game-missiles ws))
             (update-missiles (game-missiles ws) (game-invaders ws))
             (game-tank ws)))

;; update-invaders
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Consumes a ListOfInvaders and a ListOfMissiles and returns a list of invaders that have moved down and across the screen.
;; Randomly generates new invaders to the screen
;; Removes invaders hit by missiles

;(define (update-invaders loi) loi) ;stub

(define (update-invaders loi lom)
  (add-invaders (safe-invaders (move-invaders loi) (move-missiles lom))))

;; safe-invaders
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Takes the current ListOfInvaders and the next tick of ListOfMissiles and removes invaders that will be hit by a missile
(check-expect (safe-invaders empty empty) empty)
(check-expect (safe-invaders (list I1) empty) (list I1))
(check-expect (safe-invaders (list I1 I4) (list M8)) (list I1))
(check-expect (safe-invaders (list I1 I4) (list M1)) (list I1 I4))

;; (define (safe-invaders loi lom) loi)
(define (safe-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-collides? (first loi) lom)
             (safe-invaders (rest loi) lom)
             (cons (first loi) (safe-invaders (rest loi) lom)))]))

;; invader-collides?
;; Invador ListOfMissiles -> Boolean
;; returns true if this invader is hit by the any of the list of missiles
(check-expect (invader-collides? I1 empty) false)
(check-expect (invader-collides? I1 (list M1 M8)) false)
(check-expect (invader-collides? I1 (list M1 M2)) true)

;; (define (invader-collides? i lom) false) ; stub
(define (invader-collides? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collision? (first lom) i)
             true
             (invader-collides? i (rest lom)))]))

;; add-invaders
;; ListOfInvaders -> ListOfInvaders
;; adds a random number of invaders to the ListOfInvaders
;; !!!
;; (define (add-invaders loi) loi) ; stub
(define (add-invaders loi)
  (random-invaders (random 10) loi))

;; random-invaders
;; Natural ListOfInvaders -> ListOfInvaders
;; Adds an invader if n is greater than 7
;; !!!
(define (random-invaders n loi)
  (if (> n 7)
      (cons (make-invader (+ (random (- WIDTH 10)) 10) 0 (if (>= 8 n) 1 -1)) loi)
      loi))

;; move-invaders
;; ListOfInvaders -> ListOfInvaders
;; Advances the position of every invader
(check-expect (move-invaders LOI1) empty)
(check-expect (move-invaders LOI2) (cons (make-invader (+ (invader-x I1) 12) (+ (invader-y I1) 12) (invader-dx I1)) empty))

;; (define (move-invaders loi) loi) ;stub
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; move-invader
;; Invader -> Invader
;; Moves an invaders x and y coordinates by dx
;; changes the sign of dx if off the left or right side of the screen

(define (move-invader i)
  (cond [(< (invader-x i) 10) (make-invader (+ (invader-x i) (* (invader-dx i) -1)) (+ (invader-y i) (abs (invader-dx i))) (* (invader-dx i) -1))]
        [(> (invader-x i) WIDTH) (make-invader (+ (invader-x i) (* (invader-dx i) -1)) (+ (invader-y i) (abs (invader-dx i))) (* (invader-dx i) -1))]
        [else (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) (abs (invader-dx i))) (invader-dx i))]))

;; update-missiles
;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Consumes a ListOfMissiles and a ListOfInvaders and returns a list of missiles that have moved up the screen.
;; Removes missiles hit by invaders
;; Removes missiles that have left the screen

;; (define (update-missiles lom loi) lom) ;stub

(define (update-missiles lom loi)
  (remove-missile-collisions(on-screen-only (move-missiles lom)) loi))

;; remove-missile-collisions
;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; Removes any missile from ListOfMissiles that kills an invader from ListOfInvaders
(check-expect (remove-missile-collisions LOM4 LOI1) (list M1 M2))
(check-expect (remove-missile-collisions LOM2 LOI2) (list M1))
(check-expect (remove-missile-collisions LOM4 LOI2) (list M1))
(check-expect (remove-missile-collisions LOM6 LOI5) (list M5))

;; (define (remove-missile-collisions lom loi) loi) ;stub
(define (remove-missile-collisions lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (missile-collides? (first lom) loi)
             (remove-missile-collisions (rest lom) loi)
             (cons (first lom) (remove-missile-collisions (rest lom) loi)))]))

;; missile-collides?
;; Missile ListOfInvaders -> Boolean
;; Checks if a specific missile is in the hitbox of any invader in the list
(check-expect (missile-collides? M1 empty) false)
(check-expect (missile-collides? M7 (list I5)) true)
(check-expect (missile-collides? M1 (list I5)) false)
(check-expect (missile-collides? M8 (list I4)) true)
(check-expect (missile-collides? M7 (list I4 I5)) true)

;; (define (missile-collides? m loi) false) ; stub
(define (missile-collides? m loi)
  (cond [(empty? loi) false]
        [else
         (if (collision? m (first loi))
             true
             (missile-collides? m (rest loi)))]))

;; collision?
;; Missile Invader -> Boolean
;; Returns true if the missile and invader are colliding
(check-expect (collision? M1 I4) false)
(check-expect (collision? M8 I4) true)
(check-expect (collision? M2 I1) true)
(check-expect (collision? M7 I5) true)

;; (define (collision? m i) false) ;stub
(define (collision? m i)
  (and
   (and
    (>= (missile-x m) (- (invader-x i) HIT-RANGE))
    (<= (missile-x m) (+ (invader-x i) HIT-RANGE)))
   (and
    (>= (missile-y m) (- (invader-y i) HIT-RANGE))
    (<= (missile-y m) (+ (invader-y i) HIT-RANGE)))))

;; on-screen-only
;; ListOfMissiles -> ListOfMissiles
;; consumes a list of missiles and produces a list without the missiles that have moved off the screen
(check-expect (on-screen-only (cons M1 empty)) (cons M1 empty))
(check-expect (on-screen-only (list M1 M2 M3 M4)) (list M1 M2 M3 M4))
(check-expect (on-screen-only (list M1 M2 M3 M4 M5)) (list M1 M2 M3 M4))

;; (define (on-screen-only lom) lom) ;stub
(define (on-screen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) 0)
             (cons (first lom) (on-screen-only (rest lom)))
             (on-screen-only (rest lom)))]))

;; ListOfMissiles -> ListOfMissiles
;; decreases the Y position of each missile by MISSILE-SPEED pixels; moving the missiles up the screen
(check-expect (move-missiles LOM1) empty)
(check-expect (move-missiles LOM2) (cons (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)) empty))
(check-expect (move-missiles (cons M4 LOM3)) (list
                                              (make-missile (missile-x M4) (- (missile-y M4) MISSILE-SPEED))
                                              (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
                                              (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED))
                                              (make-missile (missile-x M3) (- (missile-y M3) MISSILE-SPEED))))

;; (define (move-missiles lom) lom) ;stub
(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (move-missiles (rest lom)))]))


;; Missile -> Missile
;; decreases the Y position of a missile by MISSILE-SPEED pixels; moving the missile up the screen
(check-expect (move-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
(check-expect (move-missile M2) (make-missile (missile-x M2) (- (missile-y M2) MISSILE-SPEED)))
(check-expect (move-missile M4) (make-missile (missile-x M4) (- (missile-y M4) MISSILE-SPEED)))

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; WS -> Image
;; Consumes a world state and produces an image representing the current state of the game
(check-expect (render-game G0) (place-image TANK (tank-x (game-tank G0)) (- HEIGHT 10) BACKGROUND))
(check-expect (render-game G1) (place-image TANK (tank-x (game-tank G1)) (- HEIGHT 10) BACKGROUND))
(check-expect (render-game G2) (place-image INVADER (invader-x I1) (invader-y I1)
                                            (place-image MISSILE (missile-x M1) (missile-y M1)
                                                         (place-image TANK (tank-x (game-tank G2)) (- HEIGHT 10) BACKGROUND))))
;; (define (render-game ws) (square 0 "solid" "white")) ;stub
(define (render-game ws)
  (render-invaders (game-invaders ws) (render-missiles (game-missiles ws) (render-tank (game-tank ws)))))

;; ListOfInvaders Image -> Image
;; Adds a list of invaders to an image
(check-expect (render-invaders LOI1 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-expect (render-invaders LOI4 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1)
                                                             (place-image INVADER (invader-x I2) (invader-y I2) BACKGROUND)))
;;(define (render-invaders loi i) i) ; stub
(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [else
         (place-image INVADER
                      (invader-x (first loi)) (invader-y (first loi))
                      (render-invaders (rest loi) i))]))

;; ListOfMissiles Image -> Image
;; Adds a list of Missiles to an image
(check-expect (render-missiles LOM1 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-expect (render-missiles LOM4 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                             (place-image MISSILE (missile-x M2) (missile-y M2) BACKGROUND)))
;; (define (render-missiles lom i) i) ;stub

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE
                      (missile-x (first lom)) (missile-y (first lom))
                      (render-missiles (rest lom) i))]))

;; Tank -> Image
;; Adds a tank to the background
(check-expect (render-tank T0) (place-image TANK (tank-x T0) (- HEIGHT 10) BACKGROUND))

;; (define (render-tank t) BACKGROUND) ; stub
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT 10) BACKGROUND))

;; WS -> Boolean
;; Consumes a world state and returns true if the game should end
(check-expect (game-over? G0) false)
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) false)
(check-expect (game-over? G4) true)

;; (define (game-over? ws) false) ;stub

(define (game-over? ws)
  (invaders-at-bottom? (game-invaders ws)))

;; ListOfInvaders -> Boolean
;; Returns true if any of the invaders have hit the bottom of the screen
(check-expect (invaders-at-bottom? LOI1) false)
(check-expect (invaders-at-bottom? LOI2) false)
(check-expect (invaders-at-bottom? LOI3) true)
(check-expect (invaders-at-bottom? LOI4) false)

;; (define (invaders-at-bottom? loi) false) ;stub
(define (invaders-at-bottom? loi)
  (cond [(empty? loi) false]
        [else
         (if (> (+ 10 HEIGHT) (invader-y (first loi)))
             (invaders-at-bottom? (rest loi))
             true)]))

;; WS KeyEvent -> WS
;; Consumes a world state and a key event.
;;  - if KeyEvent is left or right arrow key move the tank
;;  - if KeyEvent is space bar fire a missile

(check-expect (handle-key G0 " ")     (make-game (game-invaders G0)
                                                 (cons (make-missile (tank-x (game-tank G0)) HEIGHT) (game-missiles G0))
                                                 (game-tank G0)))
(check-expect (handle-key G2 " ")     (make-game (game-invaders G2)
                                                 (cons (make-missile (tank-x (game-tank G2)) HEIGHT) (game-missiles G2))
                                                 (game-tank G2)))

(check-expect (handle-key G0 "left")  (make-game (game-invaders G0)
                                                 (game-missiles G0)
                                                 (make-tank (- (tank-x (game-tank G0)) TANK-SPEED) -1)))

(check-expect (handle-key G2 "left")  (make-game (game-invaders G2)
                                                 (game-missiles G2)
                                                 (make-tank (- (tank-x (game-tank G2)) TANK-SPEED) -1)))

(check-expect (handle-key G0 "right") (make-game (game-invaders G0)
                                                 (game-missiles G0)
                                                 (make-tank (+ (tank-x (game-tank G0)) TANK-SPEED)  1)))

(check-expect (handle-key G2 "right") (make-game (game-invaders G2)
                                                 (game-missiles G2)
                                                 (make-tank (+ (tank-x (game-tank G2)) TANK-SPEED)  1)))

;; (define (handle-key ws ke) ws) ; stub

(define (handle-key ws ke)
  (cond [(key=? " " ke) (make-game (game-invaders ws) (cons (make-missile (tank-x (game-tank ws)) HEIGHT) (game-missiles ws)) (game-tank ws))]
        [(key=? "left" ke) (make-game (game-invaders ws) (game-missiles ws) (move-tank-left (game-tank ws)))]
        [(key=? "right" ke) (make-game (game-invaders ws) (game-missiles ws) (move-tank-right (game-tank ws)))]
        [else ws]))

;; move-tank-right
;; Tank -> Tank
;; Takes a tank and and returns one with the tank moved to the right
(check-expect (move-tank-right T1) (make-tank (+ (tank-x T1) TANK-SPEED) 1))
(check-expect (move-tank-right T2) (make-tank (+ (tank-x T2) TANK-SPEED) 1))
(check-expect (move-tank-right T4) T4)

;; (define (move-tank-right t) t) ; stub

(define (move-tank-right t)
  (if (> WIDTH (tank-x t))
      (make-tank (+ (tank-x t) TANK-SPEED)  1)
      t))

;; move-tank-left
;; Tank -> Tank
;; Takes a tank and and returns one with the tank moved to the right
(check-expect (move-tank-left T1) (make-tank (- (tank-x T1) TANK-SPEED) -1))
(check-expect (move-tank-left T2) (make-tank (- (tank-x T2) TANK-SPEED) -1))
(check-expect (move-tank-left T3) T3)

;; (define (move-tank-left t) t) ; stub

(define (move-tank-left t)
  (if (> (tank-x t) 0)
      (make-tank (- (tank-x t) TANK-SPEED)  -1)
      t))


