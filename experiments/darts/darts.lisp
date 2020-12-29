#!/usr/bin/sbcl --script

(defun estimate-pi (n)
  (let ((in-circle-count
          (loop repeat n
                sum (if (apply #'is-in-circle-p (random-xy)) 1 0))))
    (* 4 (/ in-circle-count throws))))

(defun random-xy ()
  (loop repeat 2
        collect (- 1/2 (random (* 2.0 1/2)))))

(defun is-in-circle-p (x y)
  (<= (sqrt (+ (square x) (square y))) 1/2))

(defun square (x)
  (* x x))

(setf *random-state* (make-random-state t))
(princ (float (estimate-pi (parse-integer (second *posix-argv*)))))
(terpri)
