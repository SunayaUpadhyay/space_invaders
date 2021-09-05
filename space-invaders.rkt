;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define CENTERX (/ WIDTH 2))
(define CENTERY (/ HEIGHT 2))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define INVADER-NUMBER 15)
(define LAST-PIC (place-image (text "YOU LOST!"
                                    20
                                    "black")
                              CENTERX
                              CENTERY
                              BACKGROUND))

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 60)


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



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ==========================================


;; Functions

;; Game -> Game
;; start the world with (main (make-game empty empty T0))

(define (main g)
  (big-bang g                 ; Game
    (on-tick progress)        ; Game -> Game
    (to-draw render)          ; Game -> Images
    (on-key key-handler)      ; Game KeyEvent -> Game
    (stop-when game-end final-image)))  ; Game -> Boolean, Game -> Image


;; progress
;; Game -> Game
;; - updates the position of Missile, and Invader.
;; - removes invader from game if invader is hit
;; - add new invader in the list 
;; - removes bullet from screen once it reaches the top or hits invader

;;(define (progress g) g)

(define (progress g)
  (collision (make-game (update-invader (new-invader (game-invaders g)))
                        (delete-missile (update-missile (game-missiles g)))
                        (move-tank g))))


;; collision
;; Game -> Game
;; - removes the missiles and invaders who have been hit by each other from the list
;; (define (collision g) g)

(define (collision g)
  (make-game (collision-invader (game-invaders g) (game-missiles g))
             (collision-missile (game-missiles g) (game-invaders g))
             (game-tank g)))

;; collision-invader
;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; - produces a new list by removing the invaders that have been hit
;; (define (collision-invader loi lom) loi)

(define (collision-invader loi lom)
  (cond [(empty? loi) empty]
        [else (if (was-invader-hit? (first loi) lom)
                  (collision-invader (rest loi) lom)
                  (cons (first loi) (collision-invader (rest loi) lom)))]))


;; was-invader-hit?
;; Invader ListOfMissiles -> Boolean
;; - check whether the single invader was hit by any of the missiles in the list

(define (was-invader-hit? i lom)
  (cond [(empty? lom) false]
        [else (if (were-both-hit? i (first lom))
                  true
                  (was-invader-hit? i (rest lom)))]))

;; were-both-hit?
;; Invader Missile -> Boolean
;; - check if both invader and missile touch each other
;; - true if they touch each other

(define (were-both-hit? i m)
  (and
   (and (< (missile-x m) (+ (invader-x i) HIT-RANGE))
        (> (missile-x m) (- (invader-x i) HIT-RANGE)))
   (and (< (missile-y m) (+ (invader-y i) HIT-RANGE))
        (> (missile-y m) (- (invader-y i) HIT-RANGE)))))



;; collision-missiles
;; ListOfMissile ListOfInvader -> ListOfMissile
;; - produces a new list of missile, removing the missile that hit invader

(define (collision-missile lom loi)
  (cond [(empty? lom) lom]
        [else (if (was-missile-hit? (first lom) loi)
                  (collision-missile (rest lom) loi)
                  (cons (first lom) (collision-missile (rest lom) loi)))]))


;; was-missile-hit?
;; Missile ListOfInvader -> Boolean
;; - check whether a missile was hit by any of the invader in the list

(define (was-missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (if (were-both-hit? (first loi) m)
                  true
                  (was-missile-hit? m (rest loi)))]))



;; update-invader
;; ListOfInvader -> ListOfInvader
;; - updates x,y and dx of each invader in the list
;; (define (update-invader loi) loi)

(check-expect (update-invader empty) empty)

(check-expect (update-invader (list (make-invader 10 20 -10) (make-invader 20 30 10)))
              (list (make-invader (- 10 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) -10)
                    (make-invader (+ 20 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) 10)))

(check-expect (update-invader (list (make-invader 0 20 -10) (make-invader WIDTH 30 10)))
              (list (make-invader (+ 0 INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) 10)
                    (make-invader (- WIDTH INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) -10)))

(check-expect (update-invader (list (make-invader WIDTH 20 -10) (make-invader 0 30 10)))
              (list (make-invader (- WIDTH INVADER-X-SPEED) (+ 20 INVADER-Y-SPEED) -10)
                    (make-invader (+ 0 INVADER-X-SPEED) (+ 30 INVADER-Y-SPEED) 10)))

(define (update-invader loi)
  (cond [(empty? loi) empty]
        [else (cons (update-single-invader (first loi))
                    (update-invader (rest loi)))]))


;; Invader -> Invader
;; - updates the x,y and dx of a single invader
;; (define (update-single-invader i) i)

(define (update-single-invader i)
  (cond [(and (<= (invader-x i) 0)
              (< (invader-dx i) 0))
         (make-invader (+ (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (-(invader-dx i)))]
        [(and (>= (invader-x i) WIDTH)
              (> (invader-dx i) 0))
         (make-invader (- (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (-(invader-dx i)))]
        [(< (invader-dx i) 0)
         (make-invader (- (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]
        [(> (invader-dx i) 0)
         (make-invader (+ (invader-x i) INVADER-X-SPEED)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; update-missile
;; ListOfMissile -> ListOfMissile
;; - updates the y coordinate of the missile

;; (define (update-missile lom) lom)

(check-expect (update-missile empty) empty)
(check-expect (update-missile (list (make-missile 3 4)
                                    (make-missile 34 45)))
              (list (make-missile 3 (- 4 MISSILE-SPEED))
                    (make-missile 34 (- 45 MISSILE-SPEED))))


(define (update-missile lom)
  (cond [(empty? lom) empty]
        [else (cons (update-single-missile (first lom))
                    (update-missile (rest lom)))]))


;; update-single-missile
;; Missile -> Missile
;; - decrease the y coordinae by MISSILE-SPEED of a single Missile
;; Missile -> Missile

(define (update-single-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))
                

;; delete-missile
;; ListOfMissile -> ListOfMissile
;; - removes the missiles from the list that has reached 0 (top)

(define (delete-missile lom)
  (cond [(empty? lom) empty]
        [else (if (delete-single-missile? (first lom))
                  (delete-missile (rest lom))
                  (cons (first lom) (delete-missile (rest lom))))]))

;; delete-single-missile?
;; Missile -> Boolean
;; - check whether single missile reached top
;; - true if the missile has reached top

(define (delete-single-missile? m)
  (<= (missile-y m) 0))

;; new-invader
;; ListOfInvader -> ListOfInvader
;; - produces a new Invader if ListOfInvader is empty or when random produces INVADE-RATE
;; (define (new-invader loi) loi)

(define (new-invader loi)
  (cond [(empty? loi) (cons (make-invader (random WIDTH) 0 10) empty)]
        [(and (= (random (+ INVADE-RATE 1)) INVADE-RATE)
              (<= (numbers loi) INVADER-NUMBER))
         (cons (make-invader (random WIDTH) 0 10) loi)]
        [else loi]))


;; numbers
;; ListOfInvaders -> Number
;; - produces the numbers of invaders in the list

(define (numbers loi)
  (cond [(empty? loi) 0]
        [else (+ 1 (numbers (rest loi)))]))

  
                              
;; render
;; Game -> Image
;; - draws the image of Missile, Invader and Tank in the correct x, y position in BACKGROUND
;; (define (render g) empty-image)

(check-expect (render G0) (place-image TANK
                                       CENTERX
                                       (- HEIGHT TANK-HEIGHT/2)
                                       BACKGROUND))
(check-expect (render G2) (place-image TANK
                                       50
                                       (- HEIGHT TANK-HEIGHT/2)
                                       (place-image INVADER
                                                    150
                                                    100
                                                    (place-image MISSILE
                                                                 150
                                                                 300
                                                                 BACKGROUND))))


(define (render g)
  (place-image TANK
               (tank-x (game-tank g))
               (- HEIGHT TANK-HEIGHT/2)
               (render-invader (game-invaders g)
                               (render-missile (game-missiles g)))))

;; render-invader
;; ListOfInvader Image -> Image
;; - draws the image of Invader (correct x and y coordinate) on img
;; (define (render-invader loi img) img)

(check-expect (render-invader empty BACKGROUND) BACKGROUND)
(check-expect (render-invader (cons (make-invader 40 40 10) empty) BACKGROUND)
              (place-image INVADER
                           40
                           40
                           BACKGROUND))
(check-expect (render-invader (list (make-invader 50 50 10) (make-invader 100 100 10)) BACKGROUND)
              (place-image INVADER
                           50
                           50
                           (place-image INVADER
                                        100
                                        100
                                        BACKGROUND)))

(define (render-invader loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER
                           (invader-x (first loi))
                           (invader-y (first loi))
                           (render-invader (rest loi) img))]))

;; render-missile
;; ListOfMissile -> Image
;; - draws the image of Missile on BACKGROUND
;; (define (render-missile lom) BACKGROUND)

(define (render-missile lom)
  (cond [(empty? lom) BACKGROUND]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (render-missile (rest lom)))]))
  
;; key-handler
;; Game KeyEvent -> Game
;; - changes the x posn of Tank depending on input from user (moves right if -> pressed, moves left if <- pressed)
;; - adds bullet on pressing space key
;; (define (key-handler g k) g)

(define (key-handler g k)
  (cond [(key=? " " k) (add-missile g)]
        [(key=? "left" k) (make-game (game-invaders g)
                 (game-missiles g)
                 (make-tank (tank-x (game-tank g)) -1))]
        [(key=? "right" k) (make-game (game-invaders g)
                 (game-missiles g)
                 (make-tank (tank-x (game-tank g)) 1))]
        [else g]))


;; add-missile
;; Game -> Game
;; - add a new missile in ListOfMissile

;; (define (add-missil g) g)
(check-expect (add-missile G0) (make-game empty
                                          (cons (make-missile (tank-x T0) (- HEIGHT TANK-HEIGHT/2)) empty)
                                          T0))

(define (add-missile g)
  (make-game (game-invaders g)
             (cons (make-missile (tank-x (game-tank g))
                                 (- HEIGHT TANK-HEIGHT/2))
                   (game-missiles g))
             (game-tank g)))



;; move-tank
;; Game -> Tank
;; - moves the tank per tick
(define (move-tank g)
   (if (= (tank-dir (game-tank g)) 1)
       (move-right (game-tank g))
       (move-left (game-tank g))))

;; move-left
;; Tank -> Tank
;; - move the tank to the left if tank's x coordinate isn;t <= 0
;; (define (move-left t) t)

(define (move-left t)
  (if (<= (tank-x t) 0)
                 (make-tank (tank-x t) -1)
                 (make-tank (- (tank-x t) TANK-SPEED) -1)))



;; move-right
;; Tank -> Tank
;; - move the tank to the right if tank's x-coordinate isn't >= HEIGHT

(define (move-right t)
  (if (>= (tank-x t) WIDTH)
                 (make-tank (tank-x t) 1)
                 (make-tank (+ (tank-x t) TANK-SPEED) 1)))



;; game-end
;; Game -> Boolean
;; - produces true when one of the invaders reaches the bottom

(define (game-end g)
  (reached-bottom? (game-invaders g)))

;; reached-bottom?
;; ListOfInvaders -> Boolean
;; - produces true when one of the invaders reaches the bottom

(define (reached-bottom? loi)
  (cond [(empty? loi) false]
        [else (if (>= (invader-y (first loi)) HEIGHT)
                  true
                  (reached-bottom? (rest loi)))]))

;; final-imgae
;; Game -> Image
;; produce the final image once game is finished (lost)

(define (final-image g)
  LAST-PIC)
