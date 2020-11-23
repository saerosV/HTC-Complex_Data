;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract-sum-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; abstract-sum-starter.rkt


; PROBLEM A:
; 
; Design an abstract function (including signature, purpose, and tests) to 
; simplify the two sum-of functions. 
 

;; (listof Number) -> Number
;; produce the sum of the squares of the numbers in lon

(check-expect (sum-of-squares empty) 0)
(check-expect (sum-of-squares (list 2 4)) (+ 4 16))

(define (sum-of-squares lon) (abstract-sum sqr lon))

;; (listof String) -> Number
;; produce the sum of the lengths of the strings in los

(check-expect (sum-of-lengths empty) 0)
(check-expect (sum-of-lengths (list "a" "bc")) 3)

(define (sum-of-lengths los) (abstract-sum string-length los))

;; (X -> Number) (listof X) -> Number
;; Produce the sum of the fn of the components in lox

(check-expect (abstract-sum string-length empty) 0)
(check-expect (abstract-sum sqr (list 2 9)) (+ 4 81))

(define (abstract-sum fn lox)
  (cond [(empty? lox) 0]
        [else
         (+ (fn (first lox))
            (abstract-sum fn (rest lox)))]))

 
; PROBLEM B:
; 
; Re-define the original functions to use abstract-sum. 
; 
; Remember, the signature and tests should not change from the original 
; functions.
 
