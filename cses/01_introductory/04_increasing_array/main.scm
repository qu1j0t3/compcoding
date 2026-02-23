; Copyright (C) 2026 Toby Thain, toby@telegraphics.net

; Solution to CSES Introductory Problems Increasing Array
; https://cses.fi/problemset/task/1094

; To run:
; csi -s main.scm < input.txt

(define (increasing count)
  (define (step total count level)
    (if (zero? count)
      total
      (let* ((next (read))
             (gap (max 0 (- level next))))
        (step (+ total gap) (sub1 count) (max level next)))))
  (step 0 (sub1 count) (read)))



(print (increasing (read)))

