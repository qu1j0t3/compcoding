; Copyright (C) 2026 Toby Thain, toby@telegraphics.net

; Solution to CSES Introductory Problems Weird Algorithm
; https://cses.fi/problemset/task/1068

(define (step n)
  (print* n " ")
  (if (not (= n 1))
    (step
      (if (even? n)
        (/ n 2)
        (+ (* n 3) 1)))))

(step (read))
(newline)
