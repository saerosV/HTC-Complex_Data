;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname prefix-equal-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; prefix-equal-starter.rkt

;; Data Definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else 
         (... (first los)
              (fn-for-los (rest los)))]))

;; Functions:

;; ListOfString ListOfString -> Boolean
;; Produce true if list1 is a prefix of listb

(check-expect (prefix=? empty empty) true)
(check-expect (prefix=? (list "a" "b") empty) false)
(check-expect (prefix=? empty (list "a" "b")) true)
(check-expect (prefix=? (list "a") (list "b")) false)

(check-expect (prefix=? (list "a") (list "a")) true)
(check-expect (prefix=? (list "e" "f") (list "d" "e" "f")) false)
(check-expect (prefix=? (list "a" "b") (list "a" "b" "c" "d")) true)
(check-expect (prefix=? (list "a" "b" "c" "d") (list "a" "b")) false)


;(define (prefix=? lista listb) false);stub

(define (prefix=? lsta lstb)
  (cond [(empty? lsta) #true]
        [(empty? lstb) #false]
        [else
         (if (string=? (first lsta) (first lstb))
             (prefix=? (rest lsta) (rest lstb))
             #false)]))

;;Kiczales' solution, slightly different, "more recursive"
#|
(define (prefix=? lsta lstb)
  (cond [(empty? lsta) #true]
        [(empty? lstb) #false]
        [else
         (and (string=? (first lsta) (first lstb))
              (prefix=? (rest lsta) (rest lstb)))]))
|#

;; Before refactoring (my solution)
#|
(define (prefix=? lsta lstb)
  (cond [(and (empty? lsta)
              (empty? lstb)) #true]
        [(and (cons? lsta)
              (empty? lstb)) #false]
        [(and (empty? lsta)
              (cons? lstb)) #true]
        [else
         (if (string=? (first lsta) (first lstb))
             (prefix=? (rest lsta) (rest lstb))
             #false)]))
|#


