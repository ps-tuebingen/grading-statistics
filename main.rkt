#lang racket/base

(require racket/list
         racket/match
         racket/string
         handin-server/format-grade
         "grade-eval-utils.rkt"
         "student-eval-utils.rkt"
         "exercise-eval-utils.rkt")

(define args (current-command-line-arguments))

(define options
  (list "stats" "list" "unfinished" "erroneous" "verify" "histogram" "histo-by-studiengang" "stats-by-studiengang" "means-list"
        "student-scores" "performance-drops" "pdrop-students" "mean-handin-numbers"
        "means-per-exercise" "histo-for-exercise"))

(define (usage)
  (display (string-append "usage: racket -l grading-statistics ("
                          (string-join options "|")
                          ") <path> <arg> ... \n"
                          "for the arguments see the respective option \n")))

(define (display-error e)
  (begin
    (display (string-append e "\n"))
    (usage)
    (exit 1)))

(if (< (vector-length args) 2)
    (display-error "Wrong number of arguments")
    (void))

(define working-directory
  (if (directory-exists? (string->path (vector-ref args 1)))
      (string->path (vector-ref args 1))
      (display-error (format "Path not found: ~a" (vector-ref args 1)))))

(define max-points
  (let ([template-file (build-path working-directory TEMPLATE-FILENAME)])
    (and (file-exists? template-file)
         (or (extract-max-points (read-grading-table template-file))
             (display-error (format "Problem loading max. points from template: ~a" template-file))))))

(define student
  (if (< (vector-length args) 3)
      #f
      (vector-ref args 2)))

(define (pdrop-threshold i)
  (if (< (vector-length args) (+ i 1))
      #f
      (string->number (vector-ref args i))))

(define exercise-no
  (if (< (vector-length args) 3)
      #f
      (string->number (vector-ref args 2))))

(define (if-student proc)
  (if student
      (proc)
      (display-error "Please specify which student")))

(define (if-pdrop-threshold i proc)
  (if (pdrop-threshold i)
      (proc (pdrop-threshold i))
      (display-error "Please specify threshold")))

(match (vector-ref args 0)
  ["stats" (stats working-directory)]
  ["list" (list-grades working-directory)]
  ["unfinished" (list-unfinished working-directory)]
  ["erroneous" (list-erroneous working-directory)]
  ["histogram" (histo working-directory)]
  ["histo-by-studiengang" (histo-by-studiengang working-directory)]
  ["stats-by-studiengang" (stats-by-studiengang working-directory)]
  ["means-list" (means-list working-directory)]
  
  ["student-scores" (if-student (λ () (display-student-scores student working-directory)))]
  ["performance-drops" (if-student
                        (λ () (if-pdrop-threshold
                               3
                               (λ (t) (display-performance-drops student t working-directory)))))]
  ["pdrop-students" (if-pdrop-threshold
                     2
                     (λ (t) (display-pdrop-students t working-directory)))]

  ["mean-handin-numbers" (display-mean-handin-numbers working-directory)]
  
  ["means-per-exercise" (means-per-exercise working-directory)]
  ["histo-for-exercise" (if exercise-no
                            (histo-for-exercise exercise-no working-directory)
                            (display-error "Please specify which exercise"))]
  
  
  ["verify" (if max-points
                (verify working-directory max-points)
                (display-error
                 (string-append "Template file not found, searched for " TEMPLATE-FILENAME " in " (path->string working-directory))))]
  [else (usage)])
