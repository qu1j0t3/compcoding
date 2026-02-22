; Copyright (C) 2026 Toby Thain, toby@telegraphics.net

; Solution to CSES Introductory Problems Missing Number
; https://cses.fi/problemset/task/1083

; To run:
; csi -s main.scm < input.txt

(define (missing-number n)
  (let ((expected-sum (/ (* n (add1 n)) 2)))
    (define (accum count sum)
      (if (zero? count)
        sum
        (accum (sub1 count) (+ sum (read)))))
    (- expected-sum (accum (sub1 n) 0))))

(print (missing-number (read)))

