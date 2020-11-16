;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pattern-match-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; pattern-match-starter.rkt

;;  A function that consumes a pattern and a list, producing 
;;  true if the pattern matches the given list

;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4")) 

;; ==========
;; Functions:

;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;; P                                ListOf1String
;; a                            empty      (cons 1String ListOf1String) 
;; t                                     |
;; t   empty                     true    | true  
;; e                        ---------------------------------
;; r   (cons A Pattern)                  | (and (alphabetic? (first lo1s))
;;                                       |      (match-head? <rests>))
;; n                            false    |    
;;     (cons N Pattern)                  | (and (numeric? (first lo1s))
;;                                       |      (match-head? <rests>))
;;                                       |     

;; Pattern ListOf1String -> Boolean
;; Produces true if the pattern matches ListOf1String

(check-expect (pattern-match? empty empty) true)
(check-expect (pattern-match? empty (list "B" "3" "2")) true)

(check-expect (pattern-match? (list "A" "A") empty) false)
(check-expect (pattern-match? (list "N" "N") empty) false)

(check-expect (pattern-match? (list "A" "A" "N" "N")
                              (list "2" "Q" "Q" "5")) false)
(check-expect (pattern-match? (list "A" "A" "N" "N")
                              (list "Q" "R" "7" "9")) true)

(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "N")
                              (list "P" "1" "K" "0" "2" "4")) false)
(check-expect (pattern-match? (list "A" "N" "A" "N" "A" "N")
                              (list "V" "6" "T" "1" "Z" "4")) true)

;(define (pattern-match? p lo1s) false);stub

; Template taken from the cross product table
; 6 cases simplifed to 4

; Refactored and final function

(define (pattern-match? p lo1s)
  (cond [(empty? p) #true]
        [(empty? lo1s) #false]
        [(string=? (first p) "A")
         (and (alphabetic? (first lo1s))
              (pattern-match? (rest p) (rest lo1s)))]
        [(string=? (first p) "N")
         (and (numeric? (first lo1s))
              (pattern-match? (rest p) (rest lo1s)))]))


; First solution, works in all tests, but the last two clauses are
; not consistent with each other, despite perfoming similar tasks.
#|
(define (pattern-match? p lo1s)
  (cond [(empty? p) #true]
        [(empty? lo1s) #false]
        [(and (string=? (first p) "A")
              (alphabetic? (first lo1s)))
              (pattern-match? (rest p) (rest lo1s))]
        [else
         (and (string=? (first p) "N")
              (numeric? (first lo1s))
              (pattern-match? (rest p) (rest lo1s)))]))
|#

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric

(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)

(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))



