(defpackage :phew
  (:use :cl :sb-introspect))

(in-package :phew)

(defun clean-symbol (sym)
  (substitute #\2 #\> (remove #\- (format nil "~A" sym))))

(defun has-prefix-p (symbol prefix)
  "Check if the symbol's name starts with the given prefix."
  (let ((symbol-name (symbol-name symbol)))
    (and (>= (length symbol-name) (length prefix))  ; Ensure the symbol name is long enough
         (string= prefix (subseq symbol-name 0 (length prefix))))))

;; I'm sure SBCL has a clean way to do this...but I'm not going to take the
;; time right now to figure that out
(defun accessor-p (symbol)
  (or  (has-prefix-p symbol "BLOCKCHAIN-")
       (has-prefix-p symbol "CBLOCK-")))

(defun inspect-package (package-name)
  (let ((lst ())
        (fn-hash (make-hash-table)))
    (do-symbols (jj package-name)
      (when (and (eq (find-package package-name) (symbol-package jj))
                 (not (accessor-p jj)))
        (progn
          (mapcar (lambda (n)
                    (if (not (member (car n) (gethash jj fn-hash)))
                        (setf (gethash jj fn-hash)
                              (push (car n) (gethash jj fn-hash)))))
                  (sb-introspect:who-calls jj))
          (push jj lst))))

    (format t "digraph mytest {~%rankdir=\"LR\"~%")
    (maphash (lambda (fun used)
               (loop for x in used
                     do
                        (format t "~A -> ~A;~%" (clean-symbol x) (clean-symbol fun))))
             fn-hash)
    (format t "}~%")))

(inspect-package :blockchain)
