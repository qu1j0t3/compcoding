; Copyright (C) 2026 Toby Thain, toby@telegraphics.net

; Solution to CSES Introductory Problems Repetitions
; https://cses.fi/problemset/task/1069

; To run:
; csi -s main.scm < input.txt

(define (step count longest current-char)
  (let ((next (read-char)))
    (cond ((eof-object? next) longest)
          ((char=? next #\newline) longest)
          ((char=? next current-char)
            (step (add1 count) longest current-char))
          (else (step 1 (max count longest) next)))))

(print (step 1 0 (read-char)))
