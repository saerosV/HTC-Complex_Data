;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract-some-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; abstract-some-starter.rkt

 
; PROBLEM:
; 
; Design an abstract function called some-pred? (including signature, purpose, 
; and tests) to simplify the following two functions. When you are done
; rewrite the original functions to use your new some-pred? function.


;; ListOfNumber -> Boolean
;; produce true if some number in lon is positive
(check-expect (some-positive? empty) false)
(check-expect (some-positive? (list 2 -3 -4)) true)
(check-expect (some-positive? (list -2 -3 -4)) false)

(define (some-positive? lon) (some-pred? positive? lon))


;; ListOfNumber -> Boolean
;; produce true if some number in lon is negative
(check-expect (some-negative? empty) false)
(check-expect (some-negative? (list 2 3 -4)) true)
(check-expect (some-negative? (list 2 3 4)) false)

(define (some-negative? lon) (some-pred? negative? lon))

;; (X -> Boolean) (listof X) -> Boolean
;; Produce true if some component in (pred lon) produces true 

(check-expect (some-pred? real? empty) false)
(check-expect (some-pred? real? (list 3 1-2i)) true)

(define (some-pred? pred lox)
  (cond [(empty? lox) false]
        [else
         (or (pred (first lox))
             (some-pred? pred (rest lox)))]))

