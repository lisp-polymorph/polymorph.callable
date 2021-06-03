;;; Unit tests for polymorph.callable

(in-package #:polymorph.callable/test)

;;; Test suite definition

(def-suite polymorph.callable
    :description "Master test suite for polymorph.callable")

(in-suite polymorph.callable)

(defun test-polymorph.callable ()
  (run! 'polymorph.callable))


;;; Definitions

(defun my-add (x &rest xs)
  (reduce #'+ xs :initial-value x))

(defun my-car (cons)
  (car cons))

(defun (setf my-car) (value cons)
  (setf (car cons) value))

(declaim (inline my-add my-car))

;;; Tests

;;;; FUNCALL

(test-optimize funcall-function
  "Test FUNCALL on FUNCTION object"

  (let ((f #'my-car))
    (is (= 6 (funcall #'my-add 1 2 3)))
    (is (eq 'x (funcall f '(x y))))))

(test-optimize funcall-setf-function
  "Test FUNCALL on (SETF ...) function"

  (let ((cell (list 1 2 3)))
    (is (eq 'a (funcall #'(setf my-car) 'a cell)))
    (is (equal '(a 2 3) cell))))

(test-optimize funcall-lambda
  "Test FUNCALL on LAMBDA function"

  (is (= 110 (funcall #'(lambda (a b) (* a b)) 10 11)))
  (is (= 3 (funcall (lambda (x) (/ x 4)) 12))))

(test-optimize funcall-symbol
  (declare (inline my-car))

  (let ((my-add 'my-add))
    (flet ((my-car (x)
             (error "Gotcha. Lexical MY-CAR function called instead of global function.")))
      (declare (inline my-car))

      (is (= 15 (funcall 'my-add 4 5 6)))
      (is (= 30 (funcall my-add 10 20)))
      (is (eq 'c (funcall 'my-car '(c d e)))))))
