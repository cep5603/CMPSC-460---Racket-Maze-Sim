#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; size in pixels of each cell
(define cell-size 30)

;; Cell types
;; 0 - Empty (White)
;; 1 - Wall (Black)
;; 2 - Agent (Blue)
;; 3 - Goal (Red)
;; 4 - Visited (Gray)

;; square-for-bit : Number -> Image
;; turn cell types into colored squares
(define (square-for-bit bit)
  (cond
    [(= bit 0) (rectangle cell-size cell-size "solid" "white")]  ; Empty
    [(= bit 1) (rectangle cell-size cell-size "solid" "black")]  ; Wall
    [(= bit 2) (rectangle cell-size cell-size "solid" "blue")]   ; Agent
    [(= bit 3) (rectangle cell-size cell-size "solid" "red")]    ; Goal
    [(= bit 4) (rectangle cell-size cell-size "solid" "gray")]   ; Visited
    [else (rectangle cell-size cell-size "solid" "white")]))     ; Default

;; row-image : (Listof Number) -> Image
;; beside‐compose one row of bits into a single image
(define (row-image row)
  (if (empty? row)
      (empty-scene 0 cell-size)
      (foldl
       (lambda (b img) (beside img (square-for-bit b)))
       (square-for-bit (first row))
       (rest row))))

;; grid-image : (Listof (Listof Number)) -> Image
;; above‐compose each row-image into the full grid
(define (grid-image grid)
  (if (empty? grid)
      (empty-scene 0 0)
      (foldl
       (lambda (row img) (above img (row-image row)))
       (row-image (first grid))
       (rest grid))))

;; list-update : List Nat (X -> X) -> List
;; Replace the nth element in a list using a function
(define (list-update lst idx f)
  (if (zero? idx)
      (cons (f (first lst)) (rest lst))
      (cons (first lst) (list-update (rest lst) (- idx 1) f))))

;; Define agent position for tracking movement (mutable var)
(define agent-pos (cons 1 1))

;; Handle key events to move agent
(define (move-grid grid key)
  (define rows (length grid))
  (define cols (length (first grid)))
  
  ;; Calculate new position based on key
  (define new-pos
    (cond
      [(key=? key "up") (cons (- (car agent-pos) 1) (cdr agent-pos))]
      [(key=? key "down") (cons (+ (car agent-pos) 1) (cdr agent-pos))]
      [(key=? key "left") (cons (car agent-pos) (- (cdr agent-pos) 1))]
      [(key=? key "right") (cons (car agent-pos) (+ (cdr agent-pos) 1))]
      [else agent-pos]))
  
  ;; Check if new position is valid and not a wall
  (if (and (>= (car new-pos) 0) 
           (< (car new-pos) rows)
           (>= (cdr new-pos) 0) 
           (< (cdr new-pos) cols)
           (not (= (list-ref (list-ref grid (car new-pos)) (cdr new-pos)) 1)))
      (let* (;; Mark old position as visited (4)
             [updated-grid 
              (list-update grid (car agent-pos)
                           (lambda (row)
                             (list-update row (cdr agent-pos) 
                                          (lambda (_) 4))))]
             ;; Place agent at new position (2)
             [new-grid 
              (list-update updated-grid (car new-pos)
                           (lambda (row)
                             (list-update row (cdr new-pos) 
                                          (lambda (_) 2))))])
        ;; Update agent position for next move
        (set! agent-pos new-pos)
        new-grid)
      ;; If can't move, return unchanged grid
      grid))

(define directions '("up" "down" "left" "right"))

; Random walk
; We re-use the same move-grid method as used in manual input
(define (random-step-grid grid)
  (let ([dir (list-ref directions (random (length directions)))])
    (move-grid grid dir)))

; Method to find x,y pos of goal
; Currently, it's always in the same place, but this is so we can change it
(define (find-goal grid)
  (let loop-rows ([rows grid] [r 0])
    (if (empty? rows)
        #f
        (let ([row (first rows)])
          (let loop-cols ([cells row] [c 0])
            (cond
              [(empty? cells)
               (loop-rows (rest rows) (+ r 1))]
              [(= (first cells) 3) (cons r c)]
              [else (loop-cols (rest cells) (+ c 1))]))))))

; BFS main method!
; Makes a list of dirs from agent-pos to goal
(define (bfs-path grid)
  (define start agent-pos)
  (define goal (find-goal grid))
  (define rows (length grid))
  (define cols (length (first grid)))

  ; Helper func to check if pos is inside grid
  (define (in-bounds? pos)
    (and (>= (car pos) 0) (<  (car pos) rows)
         (>= (cdr pos) 0) (<  (cdr pos) cols)))

  ; Helper func to check if pos is wall
  (define (passable? pos)
    (not (= 1 (list-ref (list-ref grid (car pos))
                       (cdr pos)))))

  ; 4 adjacent valid cells
  (define (neighbors pos)
    (filter (lambda (p) (and (in-bounds? p) (passable? p)))
            (list
             (cons (- (car pos) 1) (cdr pos))
             (cons (+ (car pos) 1) (cdr pos))
             (cons (car pos) (- (cdr pos) 1))
             (cons (car pos) (+ (cdr pos) 1)))))

  ; queue -> cells to explore
  ; visited -> set of already seen cells
  ; parents -> list dict to map each visited cell pos to parent pos
  ; BFS returns a list (dictionary) mapping child to parent, when goal is found
  (define (bfs queue visited parents)
    (cond
      [(empty? queue) #f] ; Base case: No more cells to visit + goal not found
      [(equal? (first queue) goal) parents] ; Found goal
      [else
       ; Expansion
       (let* ([curr (first queue)]
              [restq (rest queue)]
              [nbrs (filter (lambda (p) (not (member p visited))) (neighbors curr))]
              [new-q (append restq nbrs)] ; New to queue
              [new-v (append visited nbrs)] ; New to visited
              [new-p (append parents (map (lambda (p) (cons p curr)) nbrs))]) ; New to parents
         (bfs new-q new-v new-p))]))

  ; Run BFS from start
  (let ([parent-map (bfs (list start) (list start) '())])
    (if (not parent-map)
        '() ; No path so empty
        ; Otherwise, walk back from goal to start, building direction list
        (letrec ([build
                  (lambda (pos path)
                    (if (equal? pos start)
                        path
                        (let* ([pr  (cdr (assoc pos parent-map))]
                               [dir (cond
                                      [(equal? pr 
                                               (cons (- (car pos) 1)
                                                     (cdr pos))) "down"]
                                      [(equal? pr 
                                               (cons (+ (car pos) 1)
                                                     (cdr pos))) "up"]
                                      [(equal? pr 
                                               (cons (car pos)
                                                     (- (cdr pos) 1))) "right"]
                                      [else "left"])])
                          ; Prepend dir then continue
                          (build pr (cons dir path)))))])
          (build goal '())))))

; States: manual, random, search
; Stores: (list grid mode path)
(define (state-to-image state)
  (grid-image (first state)))

(define (handle-key state key)
  (define grid  (first state))
  (define mode  (second state))
  (cond
    [(key=? key "m")
     (list grid 'manual '())]
    [(key=? key "r")
     (list grid 'random '())]
    [(key=? key "s")
     (let ([p (bfs-path grid)])
       (list grid 'search p))]
    [(and (eq? mode 'manual) (member key directions))
     (list (move-grid grid key) 'manual '())]
    [else state]))

(define (handle-tick state)
  (define grid (first state))
  (define mode (second state))
  (define path (third state))
  (cond
    [(eq? mode 'random)
     (list (random-step-grid grid) 'random '())]
    [(eq? mode 'search)
     (if (empty? path)
         ; Path done (or no goal remaining); go back to manual
         (list grid 'manual '())
         ; Step along next dir
         (let ([g (move-grid grid (first path))])
           (list g 'search (rest path))))]
    [else state]))

; 16x24 pre-defined grid
(define main-grid
  (list
   (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (list 1 2 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)
   (list 1 1 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 0 1 0 0 0 1)
   (list 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0 0 1 1 0 0 0 1 0 1)
   (list 1 1 0 0 0 0 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 0 1)
   (list 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1)
   (list 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 1 0 0 1 1)
   (list 1 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 0 1 0 1 1 1)
   (list 1 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 0 0 0 1 0 0 0 1)
   (list 1 0 1 1 1 0 1 1 0 1 1 1 0 0 0 1 0 1 1 1 0 1 0 1)
   (list 1 0 0 0 1 1 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1)
   (list 1 1 1 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1 1 1 0 1)
   (list 1 0 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 0 0 0 1)
   (list 1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 0 0 1 1 0 1)
   (list 1 0 0 0 1 1 1 1 1 0 1 0 0 0 0 1 1 1 1 1 0 0 3 1)
   (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   ))

(big-bang
  (list main-grid 'manual '())
  (to-draw state-to-image)
  (on-key handle-key)
  (on-tick handle-tick 0.1))