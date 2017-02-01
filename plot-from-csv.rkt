#lang racket

(require plot/no-gui
         plot
         csv-reading)

;; setting a memory limit
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(custodian-limit-memory (current-custodian) MAX-BYTES)

(define nil (list))

(define random-color
  (lambda ()
    (- (random 1 128) 1)))

(define (debug-print strings
                     #:sep [sep " "]
                     #:end [end "\n"])
  (define (iter remaining-strings
                result-string)
    (cond
      [(and (not (list? strings)) (string? strings))
        (display (string-append remaining-strings end))]
      [(empty? remaining-strings)
        (string-append result-string end)]
      [else
        (iter (cdr remaining-strings)
              (string-append result-string (first remaining-strings)))]))
  (display (iter strings
                 "")))

(define (read-rows-from-csv csv-file-path)
  (define next-row
    (make-csv-reader
      (open-input-file csv-file-path)
      '((separator-chars ",")
        (strip-leading-whitespace? . #t)
        (strip-trailing-whitespace? . #t))))
  (define (iter row
                current-sublist)
    (cond
      [(not (empty? row))
        (iter (next-row) (cons row current-sublist))]
      [else current-sublist]))
  (reverse (iter (next-row) nil)))


(define (get-maximum-of-vector-lists vector-lists)
  (define (get-max-of-vector-list vector-list)
    (define (iter sublist maximum)
      (cond
        [(empty? sublist) maximum]
        [else
          (cond
            [(> (vector-ref (first sublist) 1) maximum)
              (iter (rest sublist) (vector-ref (first sublist) 1))]
            [else
              (iter (rest sublist) maximum)])]))
    (iter vector-list -inf.0))
  (apply max (map get-max-of-vector-list vector-lists)))

(define (plot-bar-chart vector-lists
                        labels
                        #:output-filename [output-filename "out.png"]
                        #:title [title "plot"]
                        #:x-axis-label [x-axis-label "x"]
                        #:y-axis-label [y-axis-label "y"]
                        #:total-width [total-width 600]
                        #:total-height [total-height 400])
  (define skip-value (+ (length vector-lists) 0.3))
  (define (create-discrete-histogram a-vector-list
                                     label
                                     x-min)
    (let ([a-rgb-color (list (random-color)
                             (random-color)
                             (random-color))]
          [a-color (random-color)])
         (discrete-histogram a-vector-list
                             ;; skip is how much space is in between starts of groups of bars
                             #:skip skip-value
                             ;; x-min is the starting point for positioning the bars
                             ;; if all x-mins are equal, they will be stacked (stacked bar chart)
                             #:x-min x-min
                             #:label label
                             #:color a-color
                             #:line-color a-color)))
  ;; procedure for building the list of vectors for the plot
  (define (iter sub-vector-lists
                sub-label-list
                result-list
                nth-row)
    (cond
      [(empty? sub-vector-lists) result-list]
      [else
        (let
          ([a-color (random-color)]
           [current-vector-list (first sub-vector-lists)]
           [current-label (first sub-label-list)])
          (iter
            (cdr sub-vector-lists)
            (cdr sub-label-list)
            (cons (create-discrete-histogram current-vector-list
                                             current-label
                                             nth-row)
                  result-list)
            (+ nth-row 1)))]))

  (time
    (parameterize ([plot-width total-width]
                   [plot-height total-height]
                   [plot-x-label x-axis-label]
                   [plot-y-label y-axis-label]
                   [plot-new-window? false])
                  (plot (iter vector-lists labels nil 0)
                        #:out-file output-filename
                        #:y-max (* 1.2 (get-maximum-of-vector-lists vector-lists))))))

;; converts a list of lists of integers to a
;; list of lists of vectors of a label and the integers
(define (convert-to-vector-lists an-array
                                 labels)
  (define (array-iter current-subarray
                      result-list)
    (if (empty? current-subarray)
        result-list
        (let ([current-row (first current-subarray)])
             (cond [(empty? current-row) nil]
                   [else (array-iter (cdr current-subarray)
                                     (cons (reverse (row-iter current-row labels nil))
                                           result-list))]))))
  ;; iterate over a row
  (define (row-iter current-subrow
                    labels
                    result-row)
    (cond
      [(= (length labels) (length current-subrow))
        (cond
          [(empty? current-subrow) result-row]
          [else
            (let
              ([current-element (string->number (first current-subrow) 10)])
              (cond [(not current-element) nil]
                    [else (row-iter (cdr current-subrow)
                                    (cdr labels)
                                    (cons (vector (string->symbol (first labels)) current-element) result-row))]))])]
      [else (debug-print (list "labels and row length not equal"))
            (exit)]))
  ;; start with the empty list
  (reverse (array-iter an-array nil)))

;; extracts the column labels, assuming they are in the first row
(define (get-column-labels csv-content)
  (cdar csv-content))

;; extracts the row labels, assuming they are in the first column
(define (get-row-labels csv-content)
  (define (iter remaining-rows
                row-labels)
    (cond
      [(empty? remaining-rows) row-labels]
      [else (iter (rest remaining-rows) (cons (caar remaining-rows) row-labels))]))
  (reverse (iter (rest csv-content) nil)))

;; removes the first row and first column, assuming they contain the labels
(define (remove-labels csv-content)
  (define (remove-col-labels csv-content)
    (cdr csv-content))

  ;; removes the first row, assuming it contains the labels
  (define (remove-row-labels csv-content
                             result)
    (cond
      [(empty? csv-content) result]
      [else
        (remove-row-labels (rest csv-content)
                           (cons (cdar csv-content) result))]))
  (reverse (remove-row-labels (remove-col-labels csv-content) nil)))

(define (list->string string-list)
  (string-join string-list ""))

(define (get-output-file input-file-path
                         #:file-format [file-format "png"])
  (let
    ([parts (string-split input-file-path ".")])
    (cond
      [(empty? parts) (debug-print (list "Please specify a file with a file name other than \".\"."))]
      [else (string-append (list->string (rest (reverse parts))) "." file-format)])))

;; creates a bar chart from a csv file
(define (plot-csv csv-file-path
                  output-filename
                  #:title [title "plot"]
                  #:x-axis-label [x-axis-label "x"]
                  #:y-axis-label [y-axis-label "y"]
                  #:total-width [total-width 600]
                  #:total-height [total-height 400])
  (let*
    ([csv-content (read-rows-from-csv csv-file-path)]
     [column-labels (get-column-labels csv-content)]
     [row-labels (get-row-labels csv-content)]
     [raw-data (remove-labels csv-content)])
    (plot-bar-chart (convert-to-vector-lists raw-data column-labels)
                    row-labels
                    #:output-filename output-filename
                    #:title title
                    #:x-axis-label x-axis-label
                    #:y-axis-label y-axis-label
                    #:total-width total-width
                    #:total-height total-height))
  (debug-print (list (string-append "Plot written to file " output-filename "."))))

(plot-csv "sheet.csv"
          (get-output-file "sheet.csv" #:file-format "svg")
          #:title "Sheet Plot"
          #:x-axis-label "Attributes"
          #:y-axis-label "Count"
          #:total-width 600
          #:total-height 400)

(plot-csv "sheet.csv"
          (get-output-file "sheet.csv" #:file-format "pdf")
          #:title "Sheet Plot"
          #:x-axis-label "Attributes"
          #:y-axis-label "Count"
          #:total-width 600
          #:total-height 400)
