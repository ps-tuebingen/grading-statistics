#lang racket/base

(provide (all-defined-out))
(require racket/list)
;(require unstable/list)
(require math/statistics)

(require handin-server/format-grade)
(require "grade-eval-utils.rkt")

; Evaluation for individual exercises
; ===================================

(define (exercise-score i gt)
  (second (list-ref gt i)))

(define (extract-max-points gt)
  (map third (cdr gt)))

(define (normalized-exercise-score i gt template)
  (let ([max-score (list-ref (extract-max-points template) (- i 1))])
    (/ (exercise-score i gt) max-score)))

(define (means-per-exercise wd)
  (define template (read-grading-table (build-path wd TEMPLATE-FILENAME)))
  (define grading-records (all-finished-grading-tables wd))
  (define (max-score i) (third (list-ref template i)))
  (for ([i (range 1 (length template))])
    (let ([scores (map (lambda (gt) (exercise-score i gt)) grading-records)])
      (if (empty? scores)
          (display (format "No grading for exercise #~a\n" i))
          (display (format "Mean for exercise #~a : ~a %\n"
                           i
                           (real->decimal-string
                            (percentify
                             (mean
                              (map
                               (lambda (gt) (normalized-exercise-score i gt template))
                               grading-records))))))))))

(define (histo-for-exercise i wd)
  (define max-template (read-grading-table (build-path wd TEMPLATE-FILENAME)))
  (for [(q (normalized-grade-histogram (map (lambda (gt) (percentify (normalized-exercise-score i gt max-template)))
                                            (all-finished-grading-tables wd))))]
    (display (format "Point range ~a : ~a %\n" (car q) (real->decimal-string (cdr q))))))
