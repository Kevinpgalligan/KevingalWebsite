(defparameter +r+ 1/2)

(defun square (x)
  (* x x))

(defun random-xy ()
  (loop repeat 2
        collect (- +r+ (random (* 2.0 +r+)))))

(defun is-in-circle-p (x y)
  (<= (sqrt (+ (square x) (square y)))
      +r+))

(defun estimate-pi (throws)
  (let ((in-circle-count 0))
    (loop repeat throws
          do (destructuring-bind (x y)
                 (random-xy)
               (when (is-in-circle-p x y)
                 (incf in-circle-count))))
    (* 4 (/ in-circle-count throws))))
