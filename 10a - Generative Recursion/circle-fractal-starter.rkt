;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname circle-fractal-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; circle-fractal-starter.rkt

 
; PROBLEM:
; 
; Design a function that will create the following fractal:
; 
; https://i.stack.imgur.com/3MWb7.gif
; 
; 
; Each circle is surrounded by circles that are two-fifths smaller. 
; 
; You can build these images using the convenient beside and above functions
; if you make your actual recursive function be one that just produces the
; top leaf shape. You can then rotate that to produce the other three shapes.
; 
; You don't have to use this structure if you are prepared to use more
; complex place-image functions and do some arithmetic. But the approach
; where you use the helper is simpler.

;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)

;; Functions:

;; Number -> Image
;; Creates a circle fractal, from a given size

(check-expect (circle-fractal 10) (above (circle (* 10 STEP) "solid" "blue")
                                         (beside (circle (* 10 STEP) "solid" "blue")
                                                 (circle 10 "solid" "blue")
                                                 (circle (* 10 STEP) "solid" "blue"))
                                         (circle (* 10 STEP) "solid" "blue")))

;(define (circle-fractal s) 0);stub

(define (circle-fractal s)
  (local [(define top (render-leaf (* s STEP)))
          (define center-circle (circle s "solid" "blue"))]
    (above top
           (beside (rotate 90 top) center-circle (rotate -90 top))
                   (rotate 180 top))))

;; Number -> Image
;; Creates the top leaf

(check-expect (render-leaf TRIVIAL-SIZE)
              (circle TRIVIAL-SIZE "solid" "blue"))
(check-expect (render-leaf (/ TRIVIAL-SIZE STEP))
              (local [(define center-circle
                        (circle (/ TRIVIAL-SIZE STEP) "solid" "blue"))
                      (define leaf (circle TRIVIAL-SIZE "solid" "blue"))]
                (above leaf
                       (beside leaf center-circle leaf))))

;(define (render-leaf s) 0)

(define (render-leaf s)
  (cond [(<= s TRIVIAL-SIZE) (circle s "solid" "blue")]
        [else
         (local [(define center-circle (circle s "solid" "blue"))
                 (define leaf (render-leaf (* s STEP)))]
           (above leaf
                  (beside (rotate 90 leaf) center-circle (rotate -90 leaf))))]))

        
