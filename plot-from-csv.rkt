#lang racket

(require plot/no-gui
         plot
         csv-reading)

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define nil (list))
(define != (lambda (a b) (not (= a b))))
(define <> (lambda (a b) (not (= a b))))
(define unequal (lambda (a b) (not (= a b))))
(define zip (lambda (l1 l2) (map list l1 l2)))

;; https://lists.racket-lang.org/users/archive/2014-September/064203.html
(define (repeater func count)
  (for
    ([i (in-range count)])
    (func)))

(define get-type
  (lambda (x)
    (cond [(number? x) "Number"]
          [(pair? x) "Pair"]
          [(string? x) "String"]
          [(list? x) "List"]
          [(char? x) "Character"]
          [(vector? x) "Vector"])))

(define random-color
  (lambda ()
    (- (random 1 128) 1)))

(define (debug-print string-list #:sep [sep " "] #:end [end "\n"])
  (define (iter remaining-strings result-string)
    (cond
      [(empty? remaining-strings)
        (string-append result-string end)]
      [else
        (iter
          (cdr remaining-strings)
          (string-append result-string (car remaining-strings)))]))
  (display (iter string-list "")))

(debug-print (list "a" "b" "c" "d") #:sep " " #:end "\n")
(debug-print (list "a" "b" "c" "d"))

(define (read-rows-from-csv csv-file-path)
  (define next-row
    (make-csv-reader
      (open-input-file csv-file-path)
      '((separator-chars ",")
        (strip-leading-whitespace? . #t)
        (strip-trailing-whitespace? . #t))))
  (define (iter row current-sublist)
    (cond
      [(not (empty? row))
        (iter (next-row) (cons row current-sublist))]
      [else current-sublist]))
  (reverse (iter (next-row) nil)))


(define (plot-bar-chart vector-lists labels)
  (define (create-discrete-histogram a-vector-list label x-min)
    (let ([a-rgb-color (list (random-color)
                             (random-color)
                             (random-color))]
          [a-color (random-color)])
         (discrete-histogram a-vector-list
                             ;; skip is how much space is in between starts of groups of bars
                             #:skip 3.3
                             ;; x-min is the starting point for positioning the bars
                             ;; if all x-mins are equal, they will be stacked (stacked bar chart)
                             #:x-min x-min
                             #:label label
                             #:color a-color
                             #:line-color a-color)))

  ;; procedure for building the list of vectors for the plot
  (define (iter sub-vector-lists sub-label-list result-list nth-row)
    (cond
      [(empty? sub-vector-lists) result-list]
      [else
        (let
          ([a-color (random-color)]
           [current-vector-list (car sub-vector-lists)]
           [current-label (car sub-label-list)])
          (iter
            (cdr sub-vector-lists)
            (cdr sub-label-list)
            (cons (create-discrete-histogram current-vector-list
                                             current-label
                                             nth-row)
                  result-list)
            (+ nth-row 1)))]))


  (display (get-type (iter vector-lists labels nil 0))) (newline)
  (display (iter vector-lists labels nil 0)) (newline)
  (time (parameterize
    ([plot-width 600]
     [plot-height 400]
     [plot-x-label "x"]
     [plot-y-label "sin"]
     [plot-new-window? false])

     (plot (iter vector-lists labels nil 0)
           #:x-label "attributes"
           #:y-label "count"
           #:title "test"
           #:out-file "test.png"))))

(define (convert-to-vector-lists an-array labels)
  (define (array-iter current-subarray result-list)
    (if (empty? current-subarray)
        result-list
        (let ([current-row (car current-subarray)])
             (cond [(empty? current-row) nil]
                   [else (array-iter (cdr current-subarray)
                                     (cons (reverse (row-iter current-row labels nil))
                                           result-list))]))))
  ;; iterate over a row
  (define (row-iter current-subrow labels result-row)
    (cond [(= (length labels) (length current-subrow))
            (if (empty? current-subrow)
                result-row
                (let
                  ([current-element (string->number (car current-subrow) 10)])
                  (cond [(not current-element) nil]
                        [else (row-iter (cdr current-subrow)
                                        (cdr labels)
                                        (cons (vector (string->symbol (car labels)) current-element) result-row))])))]
          [else
            (display "labels and row length not equal")(newline)
            (exit)]))
  ;; start with the empty list
  (reverse (array-iter an-array nil)))

(define (get-column-labels csv-content)
  (cdar csv-content))

(define (get-row-labels csv-content)
  (define (iter remaining-rows row-labels)
    (cond
      [(empty? remaining-rows) row-labels]
      [else (iter (cdr remaining-rows) (cons (caar remaining-rows) row-labels))]))

  (reverse (iter (cdr csv-content) nil)))

(define (remove-labels csv-content)
  (define (remove-col-labels csv-content)
    (cdr csv-content))

  (define (remove-row-labels csv-content result)
    (cond
      [(empty? csv-content) result]
      [else
        (remove-row-labels (cdr csv-content)
                           (cons (cdar csv-content) result))]))

  (reverse (remove-row-labels (remove-col-labels csv-content) nil)))

(get-column-labels (list (list "" "a" "b" "c" "d")
                         (list "row1" 0 1 2 3)
                         (list "row2" 4 5 6 7)
                         (list "row3" 8 9 10 11)))

(get-row-labels (list (list "" "a" "b" "c" "d")
                      (list "row1" 0 1 2 3)
                      (list "row2" 4 5 6 7)
                      (list "row3" 8 9 10 11)))

(remove-labels (list (list "" "a" "b" "c" "d")
                     (list "row1" 0 1 2 3)
                     (list "row2" 4 5 6 7)
                     (list "row3" 8 9 10 11)))

(define (plot csv-file-path)
  (let*
    ([csv-content (read-rows-from-csv csv-file-path)]
     [column-labels (get-column-labels csv-content)]
     [row-labels (get-row-labels csv-content)]
     [raw-data (remove-labels csv-content)])
    (plot-bar-chart (convert-to-vector-lists raw-data column-labels) row-labels)))
