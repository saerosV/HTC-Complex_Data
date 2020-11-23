;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fractals-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)

;; fractals-starter.rkt
 
; PROBLEM: 
; 
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.


;; Constants:

; Not relevant for the solution, only visualization:  
(define WIDTH 500)
(define HEIGHT WIDTH)
(define MTS (empty-scene WIDTH HEIGHT ))    

; Used in the program:
(define TOO-SMALL 2)

;; Number -> Image
;; Produce Sierpinski Triangle of a given size (s)

(check-expect (sier-tri TOO-SMALL) (triangle TOO-SMALL "outline" "red"))
(check-expect (sier-tri (* TOO-SMALL 2))
              (overlay (triangle (* 2 TOO-SMALL) "outline" "red")
                       (local [(define subt
                                 (triangle TOO-SMALL "outline" "red"))]
                         (above subt
                                (beside subt subt)))))


;(define (sier-tri s) (square 0 "solid" "white"))

(define (sier-tri s)
  (cond [(<= s TOO-SMALL) (triangle s "outline" "red")]
        [else
         (overlay (triangle s "outline" "red") 
                  (local [(define subt (sier-tri (/ s 2)))]
                          (above subt
                                (beside subt subt))))]))

 
; PROBLEM:
; Design a function to produce a Sierpinski carpet of size s.


;; Number -> Image
;; Produce a Sierpinski Carpet of a given size (s)

(check-expect (sier-squa TOO-SMALL) (square TOO-SMALL "outline" "red"))
(check-expect (sier-squa (* TOO-SMALL 3))
              (overlay (square (* 3 TOO-SMALL) "outline" "red")
                       (local [(define subq
                                 (square TOO-SMALL "outline" "red"))
                               (define blkq
                                 (square TOO-SMALL "solid" "white"))]
                         (above (beside subq subq subq)
                                (beside subq blkq subq)
                                (beside subq subq subq)))))

;(define (sier-quad s) (square 0 "solid" "white"));stub

(define (sier-squa s)
  (cond [(<= s TOO-SMALL) (square s "outline" "red")]
        [else
         (overlay (square s "outline" "red") 
              (local [(define subq (sier-squa (/ s 3)))
                      (define blkq (square (/ s 3) "solid" "white"))]
                (above (beside subq subq subq)
                       (beside subq blkq subq)
                       (beside subq subq subq))))]))

(place-image (sier-tri  490) (/ WIDTH 2) (/ HEIGHT 2) MTS) ; Renders a Sierpinski Triangle 
(place-image (sier-squa 490) (/ WIDTH 2) (/ HEIGHT 2) MTS) ; Renders a Sierpinski Carpet
