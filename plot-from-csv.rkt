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

(define (debug-print string-list #:sep sep #:end end)
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

;; another zip solution
;(define (zip l1 l2)
;    (cond ((null? l1) l2)
;          ((null? l2) l1)
;          (else (cons ??? (cons ??? (zip ??? ???))))))

; (define (square x)
;     (* x x))
; 
; ;; #:bgcolor, #:fgcolor, #:height, #:legend-anchor, #:lncolor, #:out-file, #:out-kind, #:title, #:width, #:x-label, #:x-max, #:x-min, #:y-label, #:y-max, and #:y-min
; (parameterize
;     ([plot-width 600]
;     [plot-height 400]
;     [plot-x-label "x"]
;     [plot-y-label "sin"]
;     [plot-new-window? true])
; 
;     (plot
;         (list
;             (function sin (- 4) 4 #:label "y = sin(x)" #:color (- (random 1 128) 1))
;             (function cos (- 4) 4 #:label "y = cos(x)" #:color (- (random 1 128) 1))
;             (function tan (- 4) 4 #:label "y = tan(x)" #:color (- (random 1 128) 1))
;             (function square (- 4) 4 #:label "y = x²" #:color (- (random 1 128) 1)))
;         #:title "Functions"
;         #:x-min (- 4)
;         #:x-max 4
;         #:y-min (- 4)
;         #:y-max 4
;         #:out-file "square.pdf")
; 
;     (plot
;         (list
;             (discrete-histogram
;                 (list #(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5))
;                 #:skip 2.5
;                 #:x-min 0
;                 #:label "AMD")
;             (discrete-histogram
;                 (list #(Eggs 1.4) #(Bacon 2.3) #(Pancakes 3.1))
;                 #:skip 2.5
;                 #:x-min 1
;                 #:label "Intel"
;                 #:color 2
;                 #:line-color 2))
; 
;         #:x-label "Breakfast Food"
;         #:y-label "Cooking Time (minutes)"
;         #:title "Cooking Times For Breakfast Food, Per Processor"
;         #:out-file "bar-chart.png"))


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
  (define (create-discrete-histogram a-vector-list label)
    (let ([a-color (random-color)])
         (discrete-histogram a-vector-list #:skip 2.5 #:x-min 0 #:label label #:color a-color #:line-color a-color)))

  ;; procedure for building the list of vectors for the plot
  (define (iter sub-vector-lists sub-label-list result-list)
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
                                             current-label)
                  result-list)))]))


  (display (get-type (iter vector-lists labels nil))) (newline)
  (display (iter vector-lists labels nil)) (newline)
  (time (parameterize
    ([plot-width 600]
     [plot-height 400]
     [plot-x-label "x"]
     [plot-y-label "sin"]
     [plot-new-window? false])

     (plot (reverse (iter vector-lists labels nil))
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
                                        (cons (vector (car labels) current-element) result-row))])))]
          [else
            (display "labels and row length not equal")(newline)
            (exit)]))
  ;; start with the empty list
  (reverse (array-iter an-array nil)))

(plot-bar-chart
  (convert-to-vector-lists
    (read-rows-from-csv "sheet.csv")
    (list "A" "B" "C" "D"))
  (list "row1" "row2" "row3"))
