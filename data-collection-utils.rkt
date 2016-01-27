#lang racket/base

(require racket/list)
(require racket/dict)
(require file/md5)

(require (planet dherman/csv-write:1:2/csv-write))

(require "student-eval-utils.rkt")
(require (only-in "exercise-eval-utils.rkt" max-points-for-wd))

; For statistics use (csv dump generation)
; ========================================

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
                  (map (lambda (gd) (list (subbytes (md5 (grading-data-student gd)) 0 6)
                                          (grading-data-homework gd)
                                          (grading-data-number-handins gd)
                                          (grading-data-grade gd)
                                          (if (grading-data-good-checker? gd)
                                              1
                                              0)
                                          (subbytes (md5 (grading-data-tutor gd)) 0 6)))
                       (collect-grading-data cgs wd)))
                 out)))

; (write-grading-data-csv "../info1-teaching-material/statistics/checker-goodnesses.rktd" "../LocalPathForAllHandins/production" (open-output-file "../test.csv"))

; For documentation (to show to students, ...)
; ============================================

; A table for grading documentation is a list of grading-doc-rows.
; A grading-doc-row consists of:
; - student-id: a String
; - homework-or-all: either a String that identifies the homework or 'all which indicates that the row contains all hw points summed up
; - points: the points of the student for the respective homework or in sum, depending on homework-or-all
; - max-points: the maximum points possible, likewise depending on homework-or-all
; - handin?: a Boolean, indicates whether the student handed something in (only relevant when homework-or-all is not 'all)
(define-struct grading-doc-row (student homework-or-all points max-points handin?))

; The maximum points for the given homework hw (in working directory wd)
; (often 100, but not always)
; Path String -> Points
(define (sum-max-points-for-hw wd hw)
  (apply + (max-points-for-wd (build-path wd hw))))

; Collect all grading documentation for the given student s from the working directory wd
; String Path -> List-of grading-doc-row
(define (collect-grading-doc s wd)
  (let ((scores (student-scores s wd)))
    (append (map (lambda (scr) (grading-doc-row s
                                              (path->string (student-score-path scr))
                                              (if (student-score-points scr)
                                                  (student-score-points scr)
                                                  0)
                                              (sum-max-points-for-hw wd (student-score-path scr))
                                              (student-score-handin? scr)))
               scores)
            (list (grading-doc-row s
                           'all
                           (apply + (map (lambda (score) (if (student-score-points score)
                                                             (student-score-points score)
                                                             0))
                                         scores))
                           (apply + (map (lambda (score) (sum-max-points-for-hw wd (student-score-path score)))
                                         scores))
                           #t))))) ; Note: irrelevant data, don't interpret this as "handed something in"

; Write collected (from the wd) grading documentation to the given output port out
(define (write-grading-docs-csv wd out)
  (write-table (cons
                (list "student-id" "homework-or-all" "points" "max-points" "handin?")
                (append-map (lambda (s)
                              (map (lambda (gd) (list (grading-doc-row-student gd)
                                                      (grading-doc-row-homework-or-all gd)
                                                      (grading-doc-row-points gd)
                                                      (grading-doc-row-max-points gd)
                                                      (if (grading-doc-row-handin? gd)
                                                          "true"
                                                          "false")))
                                   (collect-grading-doc s wd)))
                            (students-with-any-graded-handin wd)))
                out))

; (write-grading-docs-csv "../LocalPathForAllHandins/production" (open-output-file "../docs.csv"))

; TODO: implement the below function, keeping in mind:
; - Follow users.rkt for the csv reading and refactor
; - Trim down to only columns relevant for the report generator. Supposedly, these are "username" and "email"
; (the idea is that students are uniquely identified both by "username" and by "email", "username" identifies students in
; the collected homework data and "email" identifies them everywhere else (?))

; Trim down a users.csv (student overview from forum json dump) as given by in and write the result to out
;(define (trim-users-csv in out)
;  (let ((overview-table (call-with-input-file* in
;                          (λ (input-port)(read input-port)))))
;    ))
