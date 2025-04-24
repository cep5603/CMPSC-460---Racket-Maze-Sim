#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; size in pixels of each cell
(define cell-size 30)

;; square-for-bit : Number -> Image
;; turn a 0/1 into a white/black square
(define (square-for-bit bit)
  (square
   cell-size
   'solid
   (if (= bit 1) "black" "white")))

;; row-image : (Listof Number) -> Image
;; beside‑compose one row of bits into a single image
(define (row-image row)
  (if (empty? row)
      (empty-scene 0 cell-size)
      (foldl
       (lambda (b img) (beside img (square-for-bit b)))
       (square-for-bit (first row))
       (rest row))))

;; grid-image : (Listof (Listof Number)) -> Image
;; above‑compose each row-image into the full grid
(define (grid-image grid)
  (if (empty? grid)
      (empty-scene 0 0)
      (foldl
       (lambda (row img) (above img (row-image row)))
       (row-image (first grid))
       (rest grid))))

; 16x24 grid
(define main-grid
  (list
   (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (list 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1)
   (list 1 1 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 0 1 0 0 0 1)
   (list 1 1 1 0 1 0 1 1 1 1 1 0 1 1 0 0 1 1 0 0 0 1 0 1)
   (list 1 1 0 0 0 0 1 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0 0 1)
   (list 1 1 0 1 1 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 1 1)
   (list 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 0 1 0 0 1 1)
   (list 1 0 0 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 0 1 0 1 1 1)
   (list 1 0 1 0 1 0 1 1 1 1 0 1 0 1 0 1 0 0 0 1 0 0 0 1)
   (list 1 0 1 1 1 0 1 1 0 1 1 1 0 1 0 1 0 1 1 1 0 1 0 1)
   (list 1 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
   (list 1 1 1 0 1 1 1 1 1 1 1 0 0 1 1 1 0 1 0 1 1 1 0 1)
   (list 1 0 1 0 1 1 1 0 0 0 1 1 0 1 1 1 0 1 0 1 0 0 0 1)
   (list 1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 0 0 1 1 0 1)
   (list 1 0 0 0 1 1 1 1 1 0 1 0 0 0 0 1 1 1 1 1 0 0 0 1)
   (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   ))

(define (make-random-grid rows cols)
  (build-list rows
              (lambda (_)
                (build-list cols
                            (lambda (_)
                              (random 2))))))

(define world-grid (make-random-grid 16 24))

(big-bang main-grid
  [to-draw grid-image]
  [on-tick (lambda (g) g)  ;; identity
   1])
