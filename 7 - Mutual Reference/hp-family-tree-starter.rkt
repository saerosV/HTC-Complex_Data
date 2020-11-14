;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |hp-family-tree-starter(1)|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

;; hp-family-tree-starter.rkt

#|
 In this problem set you will represent information about descendant family 
 trees from Harry Potter and design functions that operate on those trees.
 
 To make your task much easier we suggest two things:
   - you only need a DESCENDANT family tree
   - read through this entire problem set carefully to see what information 
     the functions below are going to need. Design your data definitions to
     only represent that information.
   - you can find all the information you need by looking at the individual 
     character pages like the one we point you to for Arthur Weasley.
|# 

; PROBLEM 1:
#| 
 Design a data definition that represents a family tree from the Harry Potter 
 wiki, which contains all necessary information for the other problems.  You 
 will use this data definition throughout the rest of the homework.
|#

; PROBLEM 2: 
#|
 Define a constant named ARTHUR that represents the descendant family tree for 
 Arthur Weasley. You can find all the infomation you need by starting 
 at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
 
 You must include all of Arthur's children and these grandchildren: Lily, 
 Victoire, Albus, James.
 
 Note that on the Potter wiki you will find a lot of information. But for some 
 people some of the information may be missing. Enter that information with a 
 special value of "" (the empty string) meaning it is not present. Don't forget
 this special value when writing your interp.
|# 

;; PROBLEM 3:
#|
Design a function that produces a pair list (i.e. list of two-element lists)
of every person in the tree and his or her patronus. For example, assuming 
that HARRY is a tree representing Harry Potter and that he has no children
(even though we know he does) the result would be:

(list (list "Harry" "Stag")).

You must use ARTHUR as one of your examples.
|#

;; PROBLEM 4:
#|
Design a function that produces the names of every person in a given tree 
whose wands are made of a given material. 

You must use ARTHUR as one of your examples.
|#

;; Structs

(define-struct wizard (name patronus wand family-tree))

;;===========
;; Constants:

(define ALBUS-P (make-wizard "Albus" "" "" empty))
(define JAMES-P-II (make-wizard "James" "" "" empty))
(define LILLY-P (make-wizard "Lilly" "" "" empty))
(define VICTOIRE (make-wizard "Victoire" "" "" empty))
(define HUGO (make-wizard "Hugo" "" "" empty))
(define ROSE (make-wizard "Rose" "" "" empty))

(define BILL (make-wizard "Bill" "" "" (list VICTOIRE)))
(define CHARLIE (make-wizard "Charlie" "" "Ash" empty))
(define PERCY (make-wizard "Percy" "" "" empty))
(define FRED (make-wizard "Fred" "" "" empty))
(define GEORGE (make-wizard "George" "" ""  empty))

(define RON (make-wizard "Ron"
                         "Jack Russell Terrier"
                         "Ash"
                         (list ROSE HUGO)))

(define GINNY (make-wizard "Ginny"
                           "Horse"
                           "Holly"
                           (list ALBUS-P JAMES-P-II LILLY-P)))

;; Data definition:

;(define-struct wizard (name patronus wand family-tree))

;; Wizard is (make-wizard name patronus wand FamilyTree)
;; A wizards key attributes: their name, patronus, wand
;; and family tree

;; Name is String
;; A wizards first name

;; Patronus is one of:
;;  - empty string ("")
;;  - String
;; A wizards patronus

;; Wand is one of
;;  - empty string ("")
;;  - String
;; The material a wand is made of

;; FamilyTree is one of:
;;  - empty
;;  - (cons wizard FamilyTree)
;; A wizards family tree (descendants only)

(define W1 (make-wizard "Albus Dumbledore"
                        "Phoenix"
                        "Elder"
                        empty))

(define W2 (make-wizard "Albus Potter" "" "" empty))

(define W3 (make-wizard "Harry Potter"
                        "Stag"
                        "Holly"
                        (list "Albus Potter" "James Potter II" "Lilly Potter")))

(define W4 (make-wizard "James Potter I"
                        "Stag"
                        "Mahogany"
                        (list W3)))

;; Templates:

#;
(define (fn-for-wizard w)
  (... (wizard-name w)
       (wizard-patronus w)
       (wizard-wand w)
       (fn-for-low (wizard-family-tree w))))
#;
(define (fn-for-lod low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-wizard (first low))
              (fn-for-low (rest low)))]))

(define ARTHUR (make-wizard "Arthur"
                            "Weasel"
                            ""
                            (list BILL CHARLIE PERCY FRED GEORGE RON GINNY)))
 
;; Functions:

;; Wizard -> ListOfPair
;; ListOfWizard -> ListOfPair
;; Produces a list of pair lists, containing both
;; the name and the patronus of a wizard

(check-expect (patroni-low empty) empty)
(check-expect (patroni-wiz (make-wizard "c" "u" "k" empty))
              (list (list "c" "u")))
(check-expect (patroni-wiz ARTHUR)
              (list
               (list "Arthur" "Weasel")
               (list "Bill" "")
               (list "Victoire" "")
               (list "Charlie" "")
               (list "Percy" "")
               (list "Fred" "")
               (list "George" "")
               (list "Ron" "Jack Russell Terrier")
               (list "Rose" "")
               (list "Hugo" "")
               (list "Ginny" "Horse")
               (list "Albus" "")
               (list "James" "")               
               (list "Lilly" "")))

;(define (patroni-wiz w) empty)   ;stubs
;(define (patroni-low low) empty) 

(define (patroni-wiz w)
  (cons (list (wizard-name w)
              (wizard-patronus w))
        (patroni-low (wizard-family-tree w))))

(define (patroni-low low)
  (cond [(empty? low) empty]
        [else
         (append (patroni-wiz (first low))
                 (patroni-low (rest low)))])) 

;; Wizard String -> ListOfString
;; ListOfWizard String -> ListOfString
;; Produces the name of every wizard, in a tree, that has a wand of a
;; given material

(check-expect (find-wand--low "Holly" empty) empty)
(check-expect (find-wand--wiz "Elder" W1) (list "Albus Dumbledore"))
(check-expect (find-wand--wiz "Ash" ARTHUR) (list "Charlie" "Ron"))

;(define (find-wand--wiz wand wiz) empty)   ;stubs
;(define (find-wand--low wand low) empty)

(define (find-wand--wiz wand wiz)
  (if (string=? (wizard-wand wiz) wand)
      (cons (wizard-name wiz)
            (find-wand--low wand (wizard-family-tree wiz)))
      (find-wand--low wand (wizard-family-tree wiz))))

(define (find-wand--low wand low)
  (cond [(empty? low) empty]
        [else
         (append (find-wand--wiz wand (first low))
                 (find-wand--low wand (rest low)))]))

