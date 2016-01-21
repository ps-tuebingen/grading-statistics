#lang racket/base

(require racket/list)
(require racket/dict)

(require (planet dherman/csv-write:1:2/csv-write))

(require "student-eval-utils.rkt")

(define TUTORS-VIEW-FOLDER "tutors-view")

; TODO: read those from a file
(define CHECKER-GOODNESSES #hash(["01-Erste-Schritte" . #t]
                                 ["02-Ausdruecke" . #t]
                                 ["03-Konstanten-und-Funktionen" . #t]
                                 ["04-Entwurfsrezept" . #t]
                                 ["05-Top-Down-Entwurf" . #t]
                                 ["06-Datentypen" . #f]
                                 ["07-Big-Bang-Mehr-Datentypen" . #t]
                                 ["08-Rekursive-Datentypen" . #t]
                                 ["09-Breakout" . #f]
                                 ["10-Patternmatching-S-Expressions" . #t]))

; A table for grading data is a list of table entries for grading data.
; A table entry for grading data (grading-data) consists of:
; - a Student ID (String)
; - a Homework ID (String)
; - a Number of submissions (Integer)
; - a Grade (Number between 0 and 100)
; - a "Good Checker?" flag (Boolean)
; - a Tutor ID (String)
(define-struct grading-data (student homework number-handins grade good-checker? tutor))

; Collect the grading data for the given student s, homework hw and tutor t in the tutors' view tv
; The argument cgs is a hash map from strings to booleans, which represents whether a checker is "good" or not
(define ((collect-student-grading-data cgs tv hw t) s)
  (define score (retrieve-student-score (build-path tv hw t s)))
  (grading-data s
                hw
                (number-of-handins s tv hw t)
                (student-score-points score)
                (dict-ref cgs hw)
                t))

; Collect the grading data for the given homework hw and tutor t in the tutors' view tv
; For argument cgs, see collect-student-grading-data.
; Path String -> List-of grading-data
; TODO: adapt students-with-graded-handins and related functions to optionally allow a tutor argument for working in the tutors' view
(define ((collect-tutor-grading-data cgs tv hw) t)
  (map (collect-student-grading-data cgs tv hw t) (students-with-graded-handins tv hw
                                                                                       #:tutor t)))

; Collect the grading data for the given homework hw in the tutors' view tv
; For argument cgs, see collect-student-grading-data.
; Path String -> List-of grading-data
(define ((collect-hw-grading-data cgs tv) hw)
  (append-map (collect-tutor-grading-data cgs tv hw) (directory-list (build-path tv hw))))

; Collect all grading data from the wd (assumed to immediately contain the tutors' view)
; For argument cgs, see collect-student-grading-data.
; Path -> List-of grading-data
(define (collect-grading-data cgs wd)
  (define tutors-view (build-path wd TUTORS-VIEW-FOLDER))
  (append-map (collect-hw-grading-data cgs tutors-view) (homework-folders tutors-view)))

; Write collected (from the wd) grading data to the given output port out
(define (write-grading-data-csv cgs wd out)
  (write-table (map (lambda (gd) (list (grading-data-student gd)
                                        (grading-data-homework gd)
                                        (grading-data-number-handins gd)
                                        (grading-data-grade gd)
                                        (grading-data-good-checker? gd)
                                        (grading-data-tutor gd)))
                     (collect-grading-data cgs wd))
               out))
