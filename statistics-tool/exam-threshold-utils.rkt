#lang racket/base

(require racket/list)

(require "../data-collection-tool/data-collection-utils.rkt")
(require "student-eval-utils.rkt")

(define WD "../../../all-handins/checkers")

; Final homework grade of the student in percent assuming the rest of the hw is perfect (100%).
; Argument r is the number of pending hws (must all be scaled with coefficient 1).
; String Path Int -> Points
(define (hw-grade-ideal s wd r)
  (let ((gdr (last (collect-grading-doc s wd))))
    (/ (+ (grading-doc-row-points gdr) (* r 100)) (+ (grading-doc-row-max-points gdr) (* r 100)))))

; (currently hardcoded for pragmatic reasons)
; remaining hw to be corrected 
(define REMAINING 2)
; exam threshold
(define THRESHOLD 0.5)

; Students which have enough points to participate in the exam, assuming perfect grades for the remaining hw.
; Path -> List-of String
(define (students-over-exam-threshold wd)
  (filter (lambda (s) (>= (hw-grade-ideal s wd REMAINING) THRESHOLD))
          (students-with-any-graded-handin wd)))
