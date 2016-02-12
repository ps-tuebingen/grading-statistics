#lang racket/base

(require racket/list)
(require unstable/list)
(require racket/dict)
(require file/md5)

(require (planet dherman/csv-write:1:2/csv-write))

(require (prefix-in users: "../statistics-tool/users.rkt"))
(require "../statistics-tool/student-eval-utils.rkt")
(require (only-in "../statistics-tool/exercise-eval-utils.rkt" max-points-for-wd))

(provide write-grading-data-csv
         write-grading-docs-csv
         write-sid-table)

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

; Collect grading documentation for the given score scr for the given student s in the working directory wd
; student-score -> grading-doc-row
(define ((collect-grading-doc-for-score s wd) scr)
  (grading-doc-row s
                   (path->string (student-score-path scr))
                   (if (student-score-points scr)
                       (student-score-points scr)
                       0)
                   (sum-max-points-for-hw wd (student-score-path scr))
                   (student-score-handin? scr)))

; Consolidate rows from subtasks of the same original task
; The rows may not contain a row where homework-or-all is set to 'all
; List-of grading-doc-row -> List-of grading-doc-row
(define (consolidate-subtasks rs)
  (define grading-doc-row-homework grading-doc-row-homework-or-all)
  (define (hw-id r)
    (take (string->list (grading-doc-row-homework r)) 2))
  (define (consolidate rs)
    (grading-doc-row (grading-doc-row-student (car rs))
                     (if (> (length rs) 1)
                         (list->string (hw-id (car rs)))
                         (grading-doc-row-homework (car rs)))
                     (apply + (map (lambda (r) (grading-doc-row-points r)) rs))
                     (apply + (map (lambda (r) (grading-doc-row-max-points r)) rs))
                     (ormap (lambda (r) (grading-doc-row-handin? r)) rs)))
  (reverse (map consolidate (group-by (lambda (r) (hw-id r)) rs))))

; Produce a grading doc row for all homework by summing the relevant fields from the given grading documentation
; List-of grading-doc-row -> grading-doc-row
(define (grading-doc-sum rs)
  (grading-doc-row (grading-doc-row-student (car rs))
                   'all
                   (apply + (map grading-doc-row-points rs))
                   (apply + (map grading-doc-row-max-points rs))
                   #t)) ; Note: irrelevant data, don't interpret this as "handed something in"

; Collect all grading documentation for the given student s from the working directory wd
; String Path -> List-of grading-doc-row
(define (collect-grading-doc s wd)
  (let* ((scores (student-scores s wd))
         (hw-grading-doc (consolidate-subtasks (map (collect-grading-doc-for-score s wd) scores))))
    (append hw-grading-doc
            (list (grading-doc-sum hw-grading-doc)))))

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

; A student identification table (to be used for report generation) is a list of student-identification-row
; A student-identification-row consists of:
; - hw-username: a String that identifies a student in homework grading documentation
; - matrikelnr: a String that is the Matrikelnr. of the student
; - medizintechnik?: a Boolean, true iff the student studies Medizintechnik at Uni Stuttgart
; TODO: generalize this to custom strings
(define-struct student-identification-row (hw-username matrikelnr medizintechnik?))

; Some constants for use in reading in student id tables from users.csv files
(define MEDIZINTECHNIK-STRING "Medizintechnik (Uni Stuttgart)")
(define USERNAME-COLUMN 2)
(define MNR-COLUMN 43)
(define MEDIZINTECHNIK-COLUMN 36)

; Read a users.csv (student overview from forum json dump) as given by in
; any/c -> List-of student-identification-row
(define (users-csv->sid-table in)
  (define (trim-row row)
    (student-identification-row (string-downcase (list-ref row USERNAME-COLUMN)) (list-ref row MNR-COLUMN) (string=? (list-ref row MEDIZINTECHNIK-COLUMN) MEDIZINTECHNIK-STRING)))
  (users:start-reading-csv in)
  (map trim-row users:all))

; Write the student id table generated from users.csv as given by in to the given output port out
(define (write-sid-table in out)
  (write-table (cons
                (list "username" "matrikelnr" "medizintechnik?")
                (map (lambda (row) (list (student-identification-row-hw-username row)
                                         (student-identification-row-matrikelnr row)
                                         (if (student-identification-row-medizintechnik? row)
                                             "true"
                                             "false")))
                     (users-csv->sid-table in)))
               out))
