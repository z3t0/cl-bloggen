;;;; post.lisp
;;;; all logic specific to processing posts

(defvar *input-dir* "~/dev/blog-cl/")
(defvar *output-dir* "~/dev/blog-output/")
(defvar *separator* "---")

(ql:quickload :markdown.cl)

(defvar *test-file* "~/dev/blog-cl/hello-world.md")

(defun parse (file) "Parses the file at filepath into a list of lines"
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
	  until (eq line nil)
	    collect line)))

(defun get-files (pattern)
  "Returns a list of matching files"
  (directory pattern))

(defun parse-metadata (stream)
  (flet ((get-next-line (input)
	   (read-line input nil)))
    (unless (string= (get-next-line stream) *separator*)
      (print "bad metadata"))
    (let ((metadata (loop for line = (get-next-line stream)
			  until (string= line *separator*)
			   collect line)))
      metadata)))

(defun parse-content (content)
  "Parses the markdown in the string content to html"
  (markdown.cl:parse content))

(defun parse-file (path)
  "Parses a markdown post"
  (flet ((slurp-remainder (stream)
	   (let ((seq (make-string (- (file-length stream)
				      (file-position stream)))))
	     (read-sequence seq stream)
	     (remove #\Nul seq))))
    (with-open-file (in path)
      (let ((metadata (parse-metadata in))
	    (content (parse-content (slurp-remainder in))))
	; TODO: :extension is hardcored
	(list :metadata metadata :content content :path path :extension ".md"))
      ))))


(dolist
    (file (get-files (concatenate 'string *input-dir* "/*.md")))
  (let ((parsed (parse-file file)))
    ; TODO: .html is hardcoded
    (write-file (getf parsed :content) (get-path (getf parsed :path) (getf parsed :extension) ".html" *output-dir*))
    ))

(defun get-path (path ext-orig ext-new new-path)
  "Return a new path where the ext-orig is replaced by ext-new"
  (concatenate 'string new-path (subseq
			(file-namestring path) 0 (- (length (file-namestring path)) (length ext-orig)))
	       ext-new))
