#lang racket/base

(require racket/match
         racket/string
         "data-collection-utils.rkt"
         "../statistics-tool/utils.rkt")

(define options
  (list "stats-dump" "grading-doc" "student-identification"))

(define (usage)
  (display (string-append "usage: racket -l data-collection-tool ("
                          (string-join options "|")
                          ") <path> <arg> ... \n"
                          "for the arguments see the respective option \n")))

(define (display-error e)
  (begin
    (display (string-append e "\n"))
    (usage)
    (exit 1)))

(define args (current-command-line-arguments))

(if (< (vector-length args) 2)
    (display-error "Wrong number of arguments")
    (void))

(define working-directory
  (if (directory-exists? (string->path (vector-ref args 1)))
      (string->path (vector-ref args 1))
      (display-error (format "Path not found: ~a" (vector-ref args 1)))))

(define cgs-file
  (if (< (vector-length args) 3)
      #f
      (vector-ref args 2)))

(define out (current-output-port))

(match (vector-ref args 0)
  ["stats-dump" (if cgs-file
                    (write-grading-data-csv cgs-file working-directory out)
                    (display-error "Please specify which \"checker goodnesses\" file to use"))]
  ["grading-doc" (write-grading-docs-csv working-directory out)]
  ["student-identification" (write-sid-table #f out)]) ; TODO: actually read users.csv file in (see TODO for write-sid-table)
