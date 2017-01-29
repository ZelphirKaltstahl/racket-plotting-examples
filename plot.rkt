#lang racket

(require plot/no-gui
         plot)

(define (square x)
    (* x x))

;; #:bgcolor, #:fgcolor, #:height, #:legend-anchor, #:lncolor, #:out-file, #:out-kind, #:title, #:width, #:x-label, #:x-max, #:x-min, #:y-label, #:y-max, and #:y-min
(parameterize
    ([plot-width 600]
    [plot-height 400]
    [plot-x-label "x"]
    [plot-y-label "sin"]
    [plot-new-window? true])

    (plot
        (list
            (function sin (- 4) 4 #:label "y = sin(x)" #:color (- (random 1 128) 1))
            (function cos (- 4) 4 #:label "y = cos(x)" #:color (- (random 1 128) 1))
            (function tan (- 4) 4 #:label "y = tan(x)" #:color (- (random 1 128) 1))
            (function square (- 4) 4 #:label "y = x²" #:color (- (random 1 128) 1)))
        #:title "Functions"
        #:x-min (- 4)
        #:x-max 4
        #:y-min (- 4)
        #:y-max 4
        #:out-file "square.pdf")

    (plot
        (list
            (discrete-histogram
                (list #(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5))
                #:skip 2.5
                #:x-min 0
                #:label "AMD")
            (discrete-histogram
                (list #(Eggs 1.4) #(Bacon 2.3) #(Pancakes 3.1))
                #:skip 2.5
                #:x-min 1
                #:label "Intel"
                #:color 2
                #:line-color 2))

        #:x-label "Breakfast Food"
        #:y-label "Cooking Time (minutes)"
        #:title "Cooking Times For Breakfast Food, Per Processor"
        #:out-file "bar-chart.png"))


