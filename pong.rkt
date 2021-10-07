;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pong) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)



;; TO START, RUN
#;
(main (random-velocity
       (make-pong (/ WIDTH 2) (/ HEIGHT 2) (/ HEIGHT 2) (/ HEIGHT 2) 0 0 0 0)))
;; OR UNCOMMENT THE LINE AT THE VERY BOTTOM TO AUTO START



(@htdw Pong)

;; =================
;; Constants:

(define WIDTH 1280)
(define HEIGHT 720)
(define P1-X 20)
(define P2-X (- WIDTH 20))
(define BALL-START-X (/ WIDTH 2))
(define BALL-START-Y (/ HEIGHT 2))
(define BALL-RADIUS 10)
(define BALL (circle BALL-RADIUS "solid" "black"))
(define TOP (+ 0 BALL-RADIUS))
(define BOT (- HEIGHT BALL-RADIUS))
(define LEF (+ 0 BALL-RADIUS))
(define RIG (- WIDTH BALL-RADIUS))
(define SCORE-Y (+ TOP 20))
(define PADDLE-REAL-HEIGHT 100)
(define PADDLE-HEIGHT (/ PADDLE-REAL-HEIGHT 2))
(define PADDLE-REAL-WIDTH 4)
(define PADDLE-WIDTH (/ PADDLE-REAL-WIDTH 2))
(define PADDLE (rectangle PADDLE-REAL-WIDTH PADDLE-REAL-HEIGHT "solid" "black"))
(define MAX-RANDOM-SPEED 5)
(define PADDLE-SPEED 10)
(define SCENE (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 40)
(define TEXT-COLOR "black")

;; =================
;; Data definitions:

(@htdd Pong)
(define-struct pong (ball-x ball-y p1 p2 vx vy score1 score2))
;; Pong is (make-pong Number Number Number Number)
;; interp. ball-x represents the x position of the pong ball
;;         ball-y represents the y position of the pong ball
;;         p1 represents the y position of the first player's paddle
;;         p2 represents the y position of the second player's paddle
;;         vx represents the velocity of the ball in the x coordinate
;;         vy represents the velocity of the ball in the y coordinate
;;         score1 and score2 represent the scores of player 1 and 2 respectively
;; CONSTRAINTS: ball-x is on the interval of [0,WIDTH]
;;              ball-y, p1, p2 is on the interval of [0,HEIGHT]
;;              vx, vy is on the interval of [0,SPEED]
;;              score1, score2 >= 0

(define P1 (make-pong (/ WIDTH 2) (/ HEIGHT 2) (/ HEIGHT 2) (/ HEIGHT 2)
                      0 0 0 0))

(@dd-template-rules compound) ; 8 fields

(define (fn-for-pong p)
  (... (pong-ball-x p)   ; Number
       (pong-ball-y p)   ; Number
       (pong-p1 p)       ; Number
       (pong-p2 p)       ; Number
       (pong-vx p)       ; Number
       (pong-vy p)       ; Number
       (pong-score1 p)   ; Number
       (pong-score2 p))) ; Number

;; =================
;; Functions:

(@htdf main)
(@signature Pong -> Pong)
;; start the world with the x and y positions of the pong ball and the start
;; positions of the players' paddles

(@template htdw-main)

(define (main p)
  (big-bang p                           ; Pong
    (on-tick   tock)            ; Pong -> Pong
    (to-draw   render)          ; Pong -> Image
    (on-key    handle-key)))    ; Pong KeyEvent -> Pong

;---------------------------------
(@htdf tock)
(@signature Pong -> Pong)
;; produce the next position of the pong ball
(check-expect (tock (make-pong (/ WIDTH 2) (/ HEIGHT 2) 40 60 -2 -3 0 0))
              (make-pong (+ (/ WIDTH 2) -2) (+ (/ HEIGHT 2) -3)40 60 -2 -3 0 0))
(check-expect (tock (make-pong (/ WIDTH 2) (/ HEIGHT 2) 0 0 -1 1 1 1))
              (make-pong (+ (/ WIDTH 2) -1) (+ (/ HEIGHT 2) 1) 0 0 -1 1 1 1))

;(define (tock p) p) ; stub
(@template Pong)
(define (tock p)
  (cond [(touching-top? p) (bounce-top p)]
        [(touching-bot? p) (bounce-bot p)]
        [(touching-lef? p) (score-p2 p)]
        [(touching-rig? p) (score-p1 p)]
        [(touching-p1? p) (bounce-p1 p)]
        [(touching-p2? p) (bounce-p2 p)]
        [else
         (make-pong (+ (pong-ball-x p) (pong-vx p))
                    (+ (pong-ball-y p) (pong-vy p))
                    (pong-p1 p)
                    (pong-p2 p)
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]))

;---------------------------------
(@htdf render)
(@signature Pong -> Image)
;; render the pong ball and paddles at the given positions
(check-expect (render (make-pong 0 0 0 0 0 0 1 2))
              (place-image
               BALL
               0
               0
               (place-image
                PADDLE
                P1-X
                0
                (place-image
                 PADDLE
                 P2-X
                 0
                 (place-image
                  (text (string-append
                         (number->string 1)
                         ":"
                         (number->string 2))
                        TEXT-SIZE
                        TEXT-COLOR)
                  (/ WIDTH 2)
                  SCORE-Y
                  SCENE)))))
                        
(check-expect (render (make-pong 120 200 400 70 -2 4 12 1))
              (place-image
               BALL
               120
               200
               (place-image
                PADDLE
                P1-X
                400
                (place-image
                 PADDLE
                 P2-X
                 70
                 (place-image
                  (text (string-append
                         (number->string 12)
                         ":"
                         (number->string 1))
                        TEXT-SIZE
                        TEXT-COLOR)
                  (/ WIDTH 2)
                  SCORE-Y
                  SCENE)))))

;(define (render p) empty-image) ; stub
(@template Pong)
(define (render p)
  (place-image
   BALL
   (pong-ball-x p)
   (pong-ball-y p)
   (place-image
    PADDLE
    P1-X
    (pong-p1 p)
    (place-image
     PADDLE
     P2-X
     (pong-p2 p)
     (place-image
      (text (string-append
             (number->string (pong-score1 p))
             ":"
             (number->string (pong-score2 p)))
            TEXT-SIZE
            TEXT-COLOR)
      (/ WIDTH 2)
      SCORE-Y
      SCENE)))))

;---------------------------------
(@htdf handle-key)
(@signature Pong KeyEvent -> Pong)
;; handle space to restart pong at center, w/s for up/down p1 and
;; i/k for up/down p2
(check-expect (handle-key (make-pong 0 0 0 0 0 0 0 0) "e")
              (make-pong 0 0 0 0 0 0 0 0))
(check-expect (handle-key (make-pong 20 12 400 18 2 -1 12 2) "q")
              (make-pong 20 12 400 18 2 -1 12 2))
;; CANNOT MAKE EXPECTS FOR RANDOM VALUES, BUT SPACE RESETS VALUE TO RANDOM SPEED
;; AND INIT POS
(check-expect (handle-key (make-pong 12 22 490 280 -5 2 2 2) "w")
              (move-p1 (make-pong 12 22 490 280 -5 2 2 2) -1))
(check-expect (handle-key (make-pong 90 98 90 22 3 2 11 1) "s")
              (move-p1 (make-pong 90 98 90 22 3 2 11 1) 1))
(check-expect (handle-key (make-pong 43 780 900 234 -1 -1 3 2) "i")
              (move-p2 (make-pong 43 780 900 234 -1 -1 3 2) -1))
(check-expect (handle-key (make-pong 500 300 200 200 1 1 0 0) "k")
              (move-p2 (make-pong 500 300 200 200 1 1 0 0) 1))

;(define (handle-key p ke) p) ; stub
(@template KeyEvent)
(define (handle-key p ke)
  (cond [(key=? ke " ") (random-velocity p)]             ; space reset
        [(key=? ke "w") (move-p1 p -1)]                  ; up p1
        [(key=? ke "s") (move-p1 p 1)]                   ; down p1
        [(key=? ke "i") (move-p2 p -1)]                  ; up p2
        [(key=? ke "k") (move-p2 p 1)]                   ; down p2
        [else p]))


;---------------------------------
(@htdf touching-top?)
(@signature Pong -> Boolean)
;; True if the pong ball is touching the top side of the screen
(check-expect (touching-top? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        0
                                        0
                                        0
                                        0))
              false)
(check-expect (touching-top? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        -1
                                        0
                                        0))
              false)
(check-expect (touching-top? (make-pong (/ WIDTH 2)
                                        (+ TOP 3)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        -2
                                        0
                                        2))
              false)
(check-expect (touching-top? (make-pong (/ WIDTH 2)
                                        (+ TOP 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        -2
                                        1
                                        1))
              true)
(check-expect (touching-top? (make-pong (/ WIDTH 2)
                                        (+ TOP 1)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        -2
                                        1
                                        0))
              true)

;(define (touching-top? p) false) ; stub
(@template Pong)
(define (touching-top? p)
  (<= (+ (pong-ball-y p) (pong-vy p)) TOP))

;---------------------------------
(@htdf touching-bot?)
(@signature Pong -> Boolean)
;; True if the pong ball is touching the bottom side of the screen
(check-expect (touching-bot? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        0
                                        0
                                        12
                                        2))
              false)
(check-expect (touching-bot? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        1
                                        0
                                        0))
              false)
(check-expect (touching-bot? (make-pong (/ WIDTH 2)
                                        (- BOT 3)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        2
                                        1
                                        1))
              false)
(check-expect (touching-bot? (make-pong (/ WIDTH 2)
                                        (- BOT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        2
                                        1
                                        1))
              true)
(check-expect (touching-bot? (make-pong (/ WIDTH 2)
                                        (- BOT 1)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        2
                                        11
                                        11))
              true)

;(define (touching-bot? p) false) ; stub
(@template Pong)
(define (touching-bot? p)
  (>= (+ (pong-ball-y p) (pong-vy p)) BOT))

;---------------------------------
(@htdf touching-lef?)
(@signature Pong -> Boolean)
;; True if the pong ball is touching the left side of the screen
(check-expect (touching-lef? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        0
                                        0
                                        3
                                        2))
              false)
(check-expect (touching-lef? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        1
                                        2
                                        2))
              false)
(check-expect (touching-lef? (make-pong (+ LEF 3)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        -2
                                        -2
                                        1
                                        1))
              false)
(check-expect (touching-lef? (make-pong (+ LEF 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        -2
                                        -2
                                        1
                                        1))
              true)
(check-expect (touching-lef? (make-pong (+ LEF 1)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        -2
                                        -2
                                        12
                                        3))
              true)

; (define (touching-lef? p) false) ; stub
(@template Pong)
(define (touching-lef? p)
  (<= (+ (pong-ball-x p) (pong-vx p)) LEF))


;---------------------------------
(@htdf touching-rig?)
(@signature Pong -> Boolean)
;; True if the pong ball is touching the right side of the screen
(check-expect (touching-rig? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        0
                                        0
                                        6
                                        5))
              false)
(check-expect (touching-rig? (make-pong (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        1
                                        1
                                        2
                                        3))
              false)
(check-expect (touching-rig? (make-pong (- RIG 3)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        2
                                        -2
                                        5
                                        5))
              false)
(check-expect (touching-rig? (make-pong (- RIG 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        2
                                        -2
                                        5
                                        5))
              true)
(check-expect (touching-rig? (make-pong (- RIG 1)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        (/ HEIGHT 2)
                                        2
                                        -2
                                        0
                                        0))
              true)

;(define (touching-rig? p) false) ; stub
(@template Pong)
(define (touching-rig? p)
  (>= (+ (pong-ball-x p) (pong-vx p)) RIG))

;---------------------------------
(@htdf bounce-top)
(@signature Pong -> Pong)
;; Bounces the pong ball 1px off the top of the screen
(check-expect (bounce-top (make-pong 0 0 0 0 0 0 0 0))
              (make-pong 0 1 0 0 0 0 0 0))
(check-expect (bounce-top (make-pong 200 300 12 12 -1 -1 12 2))
              (make-pong 200 301 12 12 -1 1 12 2))
(check-expect (bounce-top (make-pong 300 500 200 100 10 -4 1 1))
              (make-pong 300 501 200 100 10 4 1 1))

;(define (bounce-top p) p) ; stub
(@template Pong)
(define (bounce-top p)
  (make-pong (pong-ball-x p)
             (+ (pong-ball-y p) 1)
             (pong-p1 p)
             (pong-p2 p)
             (pong-vx p)
             (- (pong-vy p))
             (pong-score1 p)
             (pong-score2 p)))

;---------------------------------
(@htdf bounce-bot)
(@signature Pong -> Pong)
;; Bounces the pong ball 1px off the bottom of the screen
(check-expect (bounce-bot (make-pong 0 1 0 0 0 0 0 0))
              (make-pong 0 0 0 0 0 0 0 0))
(check-expect (bounce-bot (make-pong 200 300 12 12 -1 1 1 1))
              (make-pong 200 299 12 12 -1 -1 1 1))
(check-expect (bounce-bot (make-pong 300 500 200 100 10 4 2 3))
              (make-pong 300 499 200 100 10 -4 2 3))

;(define (bounce-bot p) p) ; stub
(@template Pong)
(define (bounce-bot p)
  (make-pong (pong-ball-x p)
             (- (pong-ball-y p) 1)
             (pong-p1 p)
             (pong-p2 p)
             (pong-vx p)
             (- (pong-vy p))
             (pong-score1 p)
             (pong-score2 p)))

;---------------------------------
;(@htdf bounce-lef)
;(@signature Pong -> Pong)
;;; Bounces the pong ball 1px off the left of the screen
;(check-expect (bounce-lef (make-pong 0 0 0 0 0 0))
;              (make-pong 0 0 0 0 0 0))
;(check-expect (bounce-lef (make-pong 200 300 12 12 -1 1))
;              (make-pong 200 300 12 12 1 1))
;(check-expect (bounce-lef (make-pong 300 500 200 100 -10 4))
;              (make-pong 300 500 200 100 10 4))
;
;;(define (bounce-lef p) p) ; stub
;(@template Pong)
;(define (bounce-lef p)
;  (make-pong (pong-ball-x p)
;             (pong-ball-y p)
;             (pong-p1 p)
;             (pong-p2 p)
;             (- (pong-vx p))
;             (pong-vy p)))
;
;;---------------------------------
;(@htdf bounce-rig)
;(@signature Pong -> Pong)
;;; Bounces the pong ball 1px off the right of the screen
;(check-expect (bounce-rig (make-pong 0 0 0 0 0 0))
;              (make-pong 0 0 0 0 0 0))
;(check-expect (bounce-rig (make-pong 200 300 12 12 1 1))
;              (make-pong 200 300 12 12 -1 1))
;(check-expect (bounce-rig (make-pong 300 500 200 100 10 4))
;              (make-pong 300 500 200 100 -10 4))
;
;;(define (bounce-rig p) p) ; stub
;(@template Pong)
;(define (bounce-rig p)
;  (make-pong (pong-ball-x p)
;             (pong-ball-y p)
;             (pong-p1 p)
;             (pong-p2 p)
;             (- (pong-vx p))
;             (pong-vy p)))

;---------------------------------
(@htdf score-p1)
(@signature Pong -> Pong)
;; Scores 1 point for player 1
;(check-expect (score-p1 (make-pong 0 0 0 0 0 0 0 0))
;              (make-pong 0 0 0 0 0 0 1 0))
;(check-expect (score-p1 (make-pong 12 2 100 300 2 1 2 1))
;              (make-pong 12 2 100 300 2 1 3 1))
;; CANNOT CHECK EXPECT AS IT GENERATES A RANDOM VELOCITY

;(define (score-p1 p) p) ; stub
(@template Pong)
(define (score-p1 p)
  (random-velocity (make-pong (pong-ball-x p)
                              (pong-ball-y p)
                              (pong-p1 p)
                              (pong-p2 p)
                              (pong-vx p)
                              (pong-vy p)
                              (+ (pong-score1 p) 1)
                              (pong-score2 p))))

;---------------------------------
(@htdf score-p2)
(@signature Pong -> Pong)
;;; Scores 1 point for player 2
;(check-expect (score-p2 (make-pong 0 0 0 0 0 0 0 0))
;              (make-pong 0 0 0 0 0 0 0 1))
;(check-expect (score-p2 (make-pong 12 2 100 300 2 1 2 1))
;              (make-pong 12 2 100 300 2 1 2 2))
;; CANNOT CHECK-EXPECT AS IT GENERATES A RANDOM VELOCITY

;(define (score-p2 p) p) ; stub
(@template Pong)
(define (score-p2 p)
  (random-velocity (make-pong (pong-ball-x p)
                              (pong-ball-y p)
                              (pong-p1 p)
                              (pong-p2 p)
                              (pong-vx p)
                              (pong-vy p)
                              (pong-score1 p)
                              (+ (pong-score2 p) 1))))

;---------------------------------
(@htdf move-p1)
(@signature Pong Number -> Pong)
;; Moves the first player's paddle by the given velocity (+ for down, - for up)
(check-expect (move-p1 (make-pong 200 200 (/ HEIGHT 2) (/ HEIGHT 2) 0 0 0 0) -1)
              (make-pong 200 200
                         (+ (/ HEIGHT 2) (* -1 PADDLE-SPEED))
                         (/ HEIGHT 2) 0 0 0 0))
(check-expect (move-p1 (make-pong 300 80
                                  (- HEIGHT PADDLE-HEIGHT (+ PADDLE-SPEED 1))
                                  (/ HEIGHT 2)
                                  2 2 2 1) 1)
              (make-pong 300 80
                         (- HEIGHT PADDLE-HEIGHT 1)
                         (/ HEIGHT 2)
                         2 2 2 1))
(check-expect (move-p1 (make-pong 300 80
                                  (- HEIGHT PADDLE-HEIGHT PADDLE-SPEED)
                                  (/ HEIGHT 2)
                                  2 2 2 1) 1)
              (make-pong 300 80
                         (- HEIGHT PADDLE-HEIGHT)
                         (/ HEIGHT 2)
                         2 2 2 1))
(check-expect (move-p1 (make-pong 200 90
                                  (- HEIGHT PADDLE-HEIGHT (- PADDLE-SPEED 1))
                                  (/ HEIGHT 2)
                                  3 -1 2 2) 1)
              (make-pong 200 90
                         (- HEIGHT PADDLE-HEIGHT)
                         (/ HEIGHT 2)
                         3 -1 2 2))
(check-expect (move-p1 (make-pong 300 80
                                  (+ 0 PADDLE-HEIGHT (+ PADDLE-SPEED 1))
                                  (/ HEIGHT 2)
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (+ 0 PADDLE-HEIGHT 1)
                         (/ HEIGHT 2)
                         3 -1 2 2))
(check-expect (move-p1 (make-pong 300 80
                                  (+ 0 PADDLE-HEIGHT PADDLE-SPEED)
                                  (/ HEIGHT 2)
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (+ 0 PADDLE-HEIGHT)
                         (/ HEIGHT 2)
                         3 -1 2 2))
(check-expect (move-p1 (make-pong 300 80
                                  (+ 0 PADDLE-HEIGHT (- PADDLE-SPEED 1))
                                  (/ HEIGHT 2)
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (+ 0 PADDLE-HEIGHT)
                         (/ HEIGHT 2)
                         3 -1 2 2))

;(define (move-p1 p n) p) ; stub
(@template Pong)
(define (move-p1 p n)
  (cond [(>= (+ (pong-p1 p) (* PADDLE-SPEED n)) (- HEIGHT PADDLE-HEIGHT))
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    (- HEIGHT PADDLE-HEIGHT)
                    (pong-p2 p)
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]
        [(<= (+ (pong-p1 p) (* PADDLE-SPEED n)) PADDLE-HEIGHT)
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    PADDLE-HEIGHT
                    (pong-p2 p)
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]
        [else
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    (+ (pong-p1 p) (* PADDLE-SPEED n))
                    (pong-p2 p)
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]))

;---------------------------------
(@htdf move-p2)
(@signature Pong Number -> Pong)
;; Moves the second player's paddle by the given velocity (+ for down, - for up)
(check-expect (move-p2 (make-pong 200 200 (/ HEIGHT 2) (/ HEIGHT 2) 0 0 0 0) -1)
              (make-pong 200 200
                         (/ HEIGHT 2)
                         (+ (/ HEIGHT 2) (* -1 PADDLE-SPEED))
                         0 0 0 0))
(check-expect (move-p2 (make-pong 300 80
                                  (/ HEIGHT 2)
                                  (- HEIGHT PADDLE-HEIGHT (+ PADDLE-SPEED 1))
                                  2 2 2 1) 1)
              (make-pong 300 80
                         (/ HEIGHT 2)
                         (- HEIGHT PADDLE-HEIGHT 1)
                         2 2 2 1))
(check-expect (move-p2 (make-pong 300 80
                                  (/ HEIGHT 2)
                                  (- HEIGHT PADDLE-HEIGHT PADDLE-SPEED)
                                  2 2 2 1) 1)
              (make-pong 300 80
                         (/ HEIGHT 2)
                         (- HEIGHT PADDLE-HEIGHT)
                         2 2 2 1))
(check-expect (move-p2 (make-pong 200 90
                                  (/ HEIGHT 2)
                                  (- HEIGHT PADDLE-HEIGHT (- PADDLE-SPEED 1))
                                  3 -1 2 2) 1)
              (make-pong 200 90
                         (/ HEIGHT 2)
                         (- HEIGHT PADDLE-HEIGHT)
                         3 -1 2 2))
(check-expect (move-p2 (make-pong 300 80
                                  (/ HEIGHT 2)
                                  (+ 0 PADDLE-HEIGHT (+ PADDLE-SPEED 1))
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (/ HEIGHT 2)
                         (+ 0 PADDLE-HEIGHT 1)
                         3 -1 2 2))
(check-expect (move-p2 (make-pong 300 80
                                  (/ HEIGHT 2)
                                  (+ 0 PADDLE-HEIGHT PADDLE-SPEED)
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (/ HEIGHT 2)
                         (+ 0 PADDLE-HEIGHT)
                         3 -1 2 2))
(check-expect (move-p2 (make-pong 300 80
                                  (/ HEIGHT 2)
                                  (+ 0 PADDLE-HEIGHT (- PADDLE-SPEED 1))
                                  3 -1 2 2) -1)
              (make-pong 300 80
                         (/ HEIGHT 2)
                         (+ 0 PADDLE-HEIGHT)
                         3 -1 2 2))

;(define (move-p2 p n) p) ; stub
(@template Pong)
(define (move-p2 p n)
  (cond [(>= (+ (pong-p2 p) (* PADDLE-SPEED n)) (- HEIGHT PADDLE-HEIGHT))
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    (pong-p1 p)
                    (- HEIGHT PADDLE-HEIGHT)
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]
        [(<= (+ (pong-p2 p) (* PADDLE-SPEED n)) PADDLE-HEIGHT)
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    (pong-p1 p)
                    PADDLE-HEIGHT
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]
        [else
         (make-pong (pong-ball-x p)
                    (pong-ball-y p)
                    (pong-p1 p)
                    (+ (pong-p2 p) (* PADDLE-SPEED n))
                    (pong-vx p)
                    (pong-vy p)
                    (pong-score1 p)
                    (pong-score2 p))]))


;---------------------------------
(@htdf touching-p1?)
(@signature Pong -> Boolean)
;; Produces true of the ball is touching player 1's paddle
(check-expect (touching-p1? (make-pong (/ WIDTH 2)
                                       (/ HEIGHT 2)
                                       (/ HEIGHT 2)
                                       (/ HEIGHT 2)
                                       0 0 0 0))
              false)
(check-expect (touching-p1? (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                       (/ WIDTH 2)
                                       (/ WIDTH 2)
                                       PADDLE-HEIGHT
                                       0 0 0 0))
              true)
(check-expect (touching-p1? (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                       400
                                       (+ (+ 400 BALL-RADIUS) PADDLE-HEIGHT)
                                       PADDLE-HEIGHT
                                       0 0 0 0))
              false)
(check-expect (touching-p1? (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                       (+ 400 BALL-RADIUS)
                                       (+ (+ 400 BALL-RADIUS)
                                          (- PADDLE-HEIGHT 1))
                                       PADDLE-HEIGHT
                                       0 0 0 0))
              true)
(check-expect (touching-p1? (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                       400
                                       (- (- 400 BALL-RADIUS) PADDLE-HEIGHT)
                                       PADDLE-HEIGHT
                                       0 0 0 0))
              false)
(check-expect (touching-p1? (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                       (- 400 BALL-RADIUS)
                                       (- (- 400 BALL-RADIUS)
                                          (- PADDLE-HEIGHT 1))
                                       PADDLE-HEIGHT
                                       0 0 0 0))
              true)


;(define (touching-p1? p) false) ; stub
(@template Pong)
(define (touching-p1? p)
  (if (or (and (> (- (pong-ball-x p) BALL-RADIUS) (+ P1-X PADDLE-WIDTH))
               (<= (+ (- (pong-ball-x p) BALL-RADIUS) (pong-vx p))
                   (+ P1-X PADDLE-WIDTH)))
          (= (+ P1-X PADDLE-WIDTH) (- (pong-ball-x p) BALL-RADIUS)))
      (cond [(< (pong-ball-y p) (pong-p1 p))
             (> (+ (pong-ball-y p) BALL-RADIUS) (- (pong-p1 p) PADDLE-HEIGHT))]
            [(> (pong-ball-y p) (pong-p1 p))
             (< (- (pong-ball-y p) BALL-RADIUS) (+ (pong-p1 p) PADDLE-HEIGHT))]
            [else
             true])
      false))

;---------------------------------
(@htdf touching-p2?)
(@signature Pong -> Boolean)
;; Produces true of the ball is touching player 2's paddle
(check-expect (touching-p2? (make-pong (/ WIDTH 2)
                                       (/ HEIGHT 2)
                                       (/ HEIGHT 2)
                                       (/ HEIGHT 2)
                                       0 0 0 0))
              false)
(check-expect (touching-p2? (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                       (/ WIDTH 2)
                                       PADDLE-HEIGHT
                                       (/ WIDTH 2)
                                       0 0 0 0))
              true)
(check-expect (touching-p2? (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                       400
                                       PADDLE-HEIGHT
                                       (+ (+ 400 BALL-RADIUS) PADDLE-HEIGHT)
                                       0 0 0 0))
              false)
(check-expect (touching-p2? (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                       (+ 400 BALL-RADIUS)
                                       PADDLE-HEIGHT
                                       (+ (+ 400 BALL-RADIUS)
                                          (- PADDLE-HEIGHT 1))
                                       0 0 0 0))
              true)
(check-expect (touching-p2? (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                       400
                                       PADDLE-HEIGHT
                                       (- (- 400 BALL-RADIUS) PADDLE-HEIGHT)
                                       0 0 0 0))
              false)
(check-expect (touching-p2? (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                       (- 400 BALL-RADIUS)
                                       PADDLE-HEIGHT
                                       (- (- 400 BALL-RADIUS)
                                          (- PADDLE-HEIGHT 1))
                                       0 0 0 0))
              true)

;(define (touching-p2? p) false) ; stub
(@template Pong)
(define (touching-p2? p)
  (if (or (and (< (+ (pong-ball-x p) BALL-RADIUS) (- P2-X PADDLE-WIDTH))
               (>= (+ (+ (pong-ball-x p) BALL-RADIUS) (pong-vx p))
                   (- P2-X PADDLE-WIDTH)))
          (= (- P2-X PADDLE-WIDTH) (+ (pong-ball-x p) BALL-RADIUS)))
      (cond [(< (pong-ball-y p) (pong-p2 p))
             (> (+ (pong-ball-y p) BALL-RADIUS) (- (pong-p2 p) PADDLE-HEIGHT))]
            [(> (pong-ball-y p) (pong-p2 p))
             (< (- (pong-ball-y p) BALL-RADIUS) (+ (pong-p2 p) PADDLE-HEIGHT))]
            [else
             true])
      false))
;---------------------------------
(@htdf bounce-p1)
(@signature Pong -> Pong)
;; Bounces the ball off player1's paddle by flipping the x velocity
(check-expect (bounce-p1 (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                    (- 400 BALL-RADIUS)
                                    (- (- 400 BALL-RADIUS)
                                       (- PADDLE-HEIGHT 1))
                                    PADDLE-HEIGHT
                                    -1 2 0 0))
              (make-pong (+ (+ P1-X PADDLE-WIDTH BALL-RADIUS) 1)
                         (- 400 BALL-RADIUS)
                         (- (- 400 BALL-RADIUS)
                            (- PADDLE-HEIGHT 1))
                         PADDLE-HEIGHT
                         1 2 0 0))
(check-expect (bounce-p1 (make-pong (+ P1-X PADDLE-WIDTH BALL-RADIUS)
                                    (+ 400 BALL-RADIUS)
                                    (+ (+ 400 BALL-RADIUS)
                                       (- PADDLE-HEIGHT 1))
                                    PADDLE-HEIGHT
                                    -2 1 0 0))
              (make-pong (+ (+ P1-X PADDLE-WIDTH BALL-RADIUS) 1)
                         (+ 400 BALL-RADIUS)
                         (+ (+ 400 BALL-RADIUS)
                            (- PADDLE-HEIGHT 1))
                         PADDLE-HEIGHT
                         2 1 0 0))


;(define (bounce-p1 p) p) ; stub
(@template Pong)
(define (bounce-p1 p)
  (make-pong (+ (pong-ball-x p) 1)
             (pong-ball-y p)
             (pong-p1 p)
             (pong-p2 p)
             (- (pong-vx p))
             (pong-vy p)
             (pong-score1 p)
             (pong-score2 p)))

;---------------------------------
(@htdf bounce-p2)
(@signature Pong -> Pong)
;; Bounces the ball off player2's paddle by flipping the x velocity
(check-expect (bounce-p2 (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                    (- 400 BALL-RADIUS)
                                    PADDLE-HEIGHT
                                    (- (- 400 BALL-RADIUS)
                                       (- PADDLE-HEIGHT 1))
                                    2 2 0 0))
              (make-pong (- (- P2-X PADDLE-WIDTH BALL-RADIUS) 1)
                         (- 400 BALL-RADIUS)
                         PADDLE-HEIGHT
                         (- (- 400 BALL-RADIUS)
                            (- PADDLE-HEIGHT 1))
                         -2 2 0 0))
(check-expect (bounce-p2 (make-pong (- P2-X PADDLE-WIDTH BALL-RADIUS)
                                    (/ WIDTH 2)
                                    PADDLE-HEIGHT
                                    (/ WIDTH 2)
                                    3 5 0 0))
              (make-pong (- (- P2-X PADDLE-WIDTH BALL-RADIUS) 1)
                         (/ WIDTH 2)
                         PADDLE-HEIGHT
                         (/ WIDTH 2)
                         -3 5 0 0))

;(define (bounce-p2 p) p) ; stub
(@template Pong)
(define (bounce-p2 p)
  (make-pong (- (pong-ball-x p) 1)
             (pong-ball-y p)
             (pong-p1 p)
             (pong-p2 p)
             (- (pong-vx p))
             (pong-vy p)
             (pong-score1 p)
             (pong-score2 p)))

;---------------------------------
(@htdf random-velocity)
(@signature Pong -> Pong)
;; Produces a random velocity for a pong struct
;; can't really make check-expects for this...

(define (random-velocity p)
  (make-pong BALL-START-X
             BALL-START-Y
             (pong-p1 p)
             (pong-p2 p)
             (if (= (random 2) 1)
                 (- (- (random MAX-RANDOM-SPEED) 1))
                 (+ (random MAX-RANDOM-SPEED) 1))
             (if (= (random 2) 1)
                 (- (- (random MAX-RANDOM-SPEED)) 1)
                 (+ (random MAX-RANDOM-SPEED) 1))
             (pong-score1 p)
             (pong-score2 p)))
#;
(main (random-velocity
       (make-pong (/ WIDTH 2) (/ HEIGHT 2) (/ HEIGHT 2) (/ HEIGHT 2) 0 0 0 0)))
