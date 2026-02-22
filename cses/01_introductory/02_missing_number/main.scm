; Copyright (C) 2026 Toby Thain, toby@telegraphics.net

; Solution to CSES Introductory Problems Missing Number
; https://cses.fi/problemset/task/1083

; To run:
; csi -s main.scm < input.txt

(define (missing-number n)
  (let ((expected-sum (/ (* n (+ n 1)) 2)))
    (define (accum count sum)
      (if (zero? count)
        sum
        (accum (- count 1) (+ sum (read)))))
    (- expected-sum (accum (- n 1) 0))))

(print (missing-number (read)))

