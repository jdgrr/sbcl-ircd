;;;; Shared helpers for standalone live-socket smoke tests.

(in-package :sbcl-ircd)

(defparameter *smoke-port* 0)

(defmacro with-smoke-checks ((fails) &body body)
  `(let ((,fails 0))
     (flet ((check (name ok)
              (format t "~:[FAIL~;PASS~]: ~A~%" ok name)
              (unless ok (incf ,fails))))
       ,@body)))

(defun smoke-connect ()
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-connect socket (make-inet-address "127.0.0.1") *smoke-port*)
    (socket-make-stream socket :input t :output t :buffering :line
                               :element-type 'character :external-format :utf-8)))

(defun smoke-send (stream line)
  (handler-case
      (progn
        (write-string line stream)
        (write-char #\Return stream)
        (write-char #\Newline stream)
        (finish-output stream))
    (stream-error () nil)))

(defun smoke-drain (stream &optional (ms 250))
  (let ((deadline (+ (get-internal-real-time)
                     (* ms (/ internal-time-units-per-second 1000))))
        (lines nil))
    (loop
      (when (> (get-internal-real-time) deadline) (return))
      (handler-case
          (if (listen stream)
              (let ((line (read-line stream nil nil)))
                (if line (push line lines) (return)))
              (sleep 0.02))
        (stream-error () (return))))
    (nreverse lines)))

(defun smoke-has (lines substring)
  (some (lambda (line) (search substring line)) lines))

(defun smoke-count (lines substring)
  (count-if (lambda (line) (search substring line)) lines))

(defun smoke-numeric-p (line numeric)
  (let ((parsed (parse-irc-message line)))
    (and parsed (string= (second parsed) numeric))))

(defun smoke-numeric-line (lines numeric)
  (find-if (lambda (line) (smoke-numeric-p line numeric)) lines))

(defun smoke-has-numeric (lines numeric)
  (and (smoke-numeric-line lines numeric) t))

(defun smoke-register (stream nick)
  (smoke-send stream (format nil "NICK ~A" nick))
  (smoke-send stream (format nil "USER ~A 0 * :~A" nick nick))
  (smoke-drain stream))

(defun smoke-finish (fails label)
  (if (zerop fails)
      (format t "~%ALL ~A CHECKS PASSED~%" label)
      (format t "~%~D ~A CHECK(S) FAILED~%" fails label))
  (sb-ext:exit :code (if (zerop fails) 0 1)))
