;;;; util.lisp
;;;; includes basic utility code such as file io

(defun write-file (data path)
  "Writes the string data to file"
  (format t "Writing file .... ~a~%" path)
  (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists :supersede :external-format :utf-8)
    (write data :stream out :escape nil))
  )
