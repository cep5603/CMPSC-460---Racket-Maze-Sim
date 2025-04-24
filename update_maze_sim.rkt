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

;; make-grid: generate a random grid with walls, agent, and goal
(define (make-grid rows cols)
  (define base-grid
    (build-list rows
                (lambda (_)
                  (build-list cols
                              (lambda (_)
                                (random 2)))))) ;; 0 or 1
  
  ;; Place agent at top-left (replacing whatever was there)
  (define with-agent
    (list-update base-grid 0
                 (lambda (row)
                   (list-update row 0 (lambda (_) 2)))))
  
  ;; Place goal at bottom-right (replacing whatever was there)
  (list-update with-agent (- rows 1)
               (lambda (row)
                 (list-update row (- cols 1) (lambda (_) 3)))))

;; Define agent position for tracking movement
(define agent-pos (cons 0 0))

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

;; Initialize world grid
(define world-grid (make-grid 12 20))

;; Start the big-bang with the world
(big-bang world-grid
  [to-draw  grid-image]
  [on-key   move-grid]
  [on-tick  (lambda (g) g) 1])  ;; no change on tick for now
