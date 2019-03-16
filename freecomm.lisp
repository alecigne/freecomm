;;;; freecom.lisp

(in-package #:freecomm)

;;; The databases

;; Contacts
(defvar *contacts* nil)

;; Communications
(defvar *comm* nil)

;;; Converting the communication database from a text file to a csv
;;; string.

(defun csvify-line (line)
  "Take a line from the text database and convert it into a proper csv
format."
  (let* ((line (cl-ppcre:regex-replace-all "," line "."))
         (line (cl-ppcre:regex-replace-all " {1}\\t+" line ",")))
    line))

(defun csvify-file (text-db)
  "Take the text database and convert it into a proper csv
format. Output to a string."
  (with-output-to-string (out-stream)
    (with-open-file (in-stream text-db)
      (loop for line = (read-line in-stream nil)
	 while line do
	   (unless (= (length line) 1)
	     (write-line (csvify-line line) out-stream))))))

;;; At this point we can build a list of entries (lists themselves) by
;;; passing the csv string to cl-csv:read-csv. We just need to define
;;; a few functions to clean everything at the entry level; mapcar
;;; will do the rest.

;; Converting the date
(defun time-string-to-universal-time (string)
  (let* ((time-regex "([0-9]{2})/([0-9]{2})/([0-9]{4}) à ([0-9]{2}):([0-9]{2}):([0-9]{2})")
         (replacement-string "\\6 \\5 \\4 \\1 \\2 \\3")
         (string (ppcre:regex-replace-all time-regex string replacement-string)))
    (apply #'encode-universal-time
           (mapcar #'parse-integer
                   (ppcre:split "\\s+" string)))))

;; Converting the length
(defun length-string-to-seconds (string)
  (let ((lst (reverse (mapcar #'parse-integer
                              (ppcre:all-matches-as-strings "[0-9]+" string))))
        (time-lst '(1 60 3600)))
    (loop for i from 0 below (length lst) sum
	 (* (nth i time-lst) (nth i lst)))))

;; Wrapping up
(defun clean-entry (entry)
  (let ((date (first entry))
        (type (second entry))
        (len (third entry))
        (number (fourth entry)))
    (list :date (time-string-to-universal-time date)
          :type (intern (string-upcase type))
          :length (if (string= type "voix")
                      (length-string-to-seconds len)
                      (parse-integer len :junk-allowed t))
          :number (remove #\Space number))))

;;; Building the clean database

(defun make-db (text-db)
  (mapcar #'clean-entry (cl-csv:read-csv (csvify-file text-db))))

;;; Saving, loading and displaying 

(defun save-db (db filename)
  (with-open-file (out-stream filename
                              :direction :output
                              :if-exists :supersede)
    (with-standard-io-syntax
      (print db out-stream))))

(defun load-db (filename)
  (with-open-file (in-stream filename)
    (with-standard-io-syntax
      (read in-stream))))

(defun dump-db (db)
  (dolist (entry db)
    (format t "~{~a:~10t~a~%~}~&---~%" entry)))

;;; Addings contacts

(defun make-contact (name number)
  (list :name name :number number))

;;; Querying the databases

(defun select (selector-fn db)
  (remove-if-not selector-fn db))

(defun where (field value)
  #'(lambda (entry)
      (equal (getf entry field) value)))

;;; Finding a new number

(defun list-numbers (comm)
  "List all the numbers to which a call or SMS was sent."
  (remove-duplicates
   (mapcar #'(lambda (entry) (getf entry :number)) comm) :test #'string=))

(defun check-numbers (comm contacts)
  "Check if a particular number to which a call or SMS was sent is
still not in the contact database."
  (let ((numbers (list-numbers comm))
        (new-numbers nil))
    (loop for number in numbers do
	 (let ((match (find-if (where :number number) contacts)))
	   (cond ((null match)
		  (progn
		    (format t "NEW:~10t~a~%" number)
		    (push (list :name nil :number number) new-numbers)))
		 ((null (getf match :name))
		  (format t "UNKNOWN:~10t~a~%" number))
		 (t
		  (format t "FOUND:~10t~a~25t=> ~a~%" number (getf match :name))))))
    new-numbers))
