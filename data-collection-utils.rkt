#lang racket/base

(require racket/list)
(require racket/dict)
(require file/md5)

(require (planet dherman/csv-write:1:2/csv-write))

(require "student-eval-utils.rkt")

(define TUTORS-VIEW-FOLDER "tutors-view")

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
                (path->string t)))

; Collect the grading data for the given homework hw and tutor t in the tutors' view tv
; For argument cgs, see collect-student-grading-data.
; Path String -> List-of grading-data
; TODO: adapt students-with-graded-handins and related functions to optionally allow a tutor argument for working in the tutors' view
(define ((collect-tutor-grading-data cgs tv hw) t)
  (map (collect-student-grading-data cgs tv hw t) (students-with-graded-handins tv hw
                                                                                #:tutor t)))

; TODO: find a better place for this
; Collects the number of student folders for all tutors for homework hw in the tutors' view tv
(define (collect-student-numbers tv hw)
  (map (lambda (t) (string-append (path->string t) ": "
                    (number->string (length (directory-list (build-path tv hw t))))))
       (directory-list (build-path tv hw))))

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
; Argument cgs-file is a file from which to load the checker goodnesses
(define (write-grading-data-csv cgs-file wd out)
  (let ((cgs (call-with-input-file* cgs-file
               (λ (input-port)(read input-port)))))
    (write-table (cons
                  (list "student-id" "hw-id" "n-handins" "grade" "good-checker" "tutor-id")
                  (map (lambda (gd) (list (md5 (grading-data-student gd))
                                          (grading-data-homework gd)
                                          (grading-data-number-handins gd)
                                          (grading-data-grade gd)
                                          (if (grading-data-good-checker? gd)
                                              1
                                              0)
                                          (md5 (grading-data-tutor gd))))
                       (collect-grading-data cgs wd)))
                 out)))

; (write-grading-data-csv "../info1-teaching-material/statistics/checker-goodnesses.rktd" "../LocalPathForAllHandins/production" "../test.csv")