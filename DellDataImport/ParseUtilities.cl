(in-package :COMMON-LISP-USER)


;;;-----------------------------------------------------------------------
;;; 2015-01-07 WSD quick band-aid on using free SBCL vs. ACL 9.0 license, thus needing some added library functions

;;; From cl2ffi package

(defun string-replace (source from to &key all)
  "Returns a new string where substrings of SOURCE that match FROM are replaced with TO.  All arguments should be strings, case-sensitive."
  (let* ((match (string-find source from)))
    (if match
        (let ((pre (subseq source 0 match))
              (post (subseq source (+ match (length from)))))
          (str+ pre to (if all
                           (string-replace post from to :all t)
                           post)))
        source)))
  
(defun string-find (source target 
                    &key all (case-sensitive t)
                         (test (if case-sensitive #'string= #'string-equal)))
  "Returns the index of SOURCE where the substring TARGET first appears.  Arguments should be strings, case-sensitive.  If ALL is true, then a list of all such indices is returned.  This is currently a non-optimized implementation (TODO: replace with Boyer-moore algorithm)."
  (loop
     with target-length = (length target)
     for i upto (- (length source) target-length)
     if (funcall test (subseq source i (+ i target-length))
                      target)
       if all
         collect i
       else
         return i))

(defun str+ (&rest strings)
  "Returns a concatenation of every argument coerced into strings as by princ-to-string."
  (reduce #'(lambda (combined str)
              (concatenate 'string combined (princ-to-string str)))
          strings
          :initial-value ""))

(defun list-to-delimited-string (strings &optional (delim ", "))
  "Returns a string where each element of strings has been coerced into a string by princ-to-string and concatenated with delim between each item."
  (reduce #'str+
	  (cdr (mapcan #'(lambda (str) (list delim str)) 
		       strings))
	  :initial-value ""))



;;;-----------------------------------------------------------------------
;;; 2015-01-07 WSD </band-aid>


(defun delimited-string-to-list (string &optional (delim ", "))
  "Returns a list where each element is a substring of original string as delineated by occurence of the delim between each item."
  (let* ((char-delim? (typep delim 'character))
	 (len (if char-delim? 1 (length delim)))
	(pos 0)
	(lst '())
	(str (copy-tree string)))
    (loop while (setq pos
		      (if char-delim?
			  (position delim str)
			  (search delim str))) do
	 (setq lst (cons (subseq str 0 pos) lst)
	       str (subseq str (+ pos len))
	       ))
    (reverse (cons str lst))))

;;; 12-May-01 WSD added error checking to year
;;; 16-Apr-09 WSD patched special typo that had year at 0201
;;; 16-Oct-17 WSD added check for datestring of "0"
(defun convert-stringIfNeeded (tokenString typeKey)
  (case typeKey
    (:string tokenString)
    (:number (unless (equal tokenString "")
	       (read-from-string tokenString)))
    (:yyyymmdd (unless (or (equal tokenString "")
			   (equal tokenString "0"))
		 ;;(format t "~%~s" tokenString)
		 (encode-universal-time 0 0 0 
					(read-from-string (subseq tokenString 6 8))
					(read-from-string (subseq tokenString 4 6))
					;; some bad data in the data set have "0" in first digit, so accommodating
					(let ((year (read-from-string (subseq tokenString 0 4))))
					  (if (= year 201) 2017
					      (if (< year 1000) (+ 1000 year) 
						  year)
					      )))))))

;;; 12-Jan-11 WSD cycles through *policyParsingControlList*
;;; definitions and parses into slots
;;; 12-May-02 WSD modified to catch error if record is "short" (length < sequence_end)
(defun parse-FixedFieldData (data newObj parsingControlList)
  (loop for (slotname start length typeKey) in parsingControlList
      as seqStart = (- start 1)
      as seqEnd = (+ (- start 1) length)
      ;;as tracing = (format t "~%setting ~s (from ~s to ~s)" slotname start seqEnd)
      as tokenString = (if (>= (length data) seqEnd)
			   (string-trim " " (subseq data seqStart seqEnd))
			 "")
      do
	;;(format t " as ~s..."  tokenString)
	(setf (slot-value newObj slotname)
	  (convert-stringIfNeeded tokenString typeKey))
	;;(format t "just set")
	))

(defun thisYear ()
  (multiple-value-bind (sec min hr day mo thisYear) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hr day mo))
    thisYear))

;;; 12-Jun-29 WSD added
(defun yearLater (time)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time time)
    (- (encode-universal-time sec min hr day mo (+ yr 1)) 1)))


(defun datestring (time)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time time)
    (declare (ignore sec min hr))
    (format nil "~a-~a-~a" yr mo day)))

(defun timestamp (time)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time time)
    (format nil "~2,'0d:~2,'0d:~2,'0d~a on ~a-~a-~a" (if (> (- hr 12) 0) (- hr 12) hr) min sec (if (> (- hr 12) 0) "pm" "am") yr mo day)))


(defmacro monthNum (universalTime)
  `(multiple-value-bind (sec min hr day mo yr) (decode-universal-time ,universalTime)
     (declare (ignore sec min hr day yr))
     mo))

;;; 12-Jun-27 WSD genericized for PolicyAARecords, Agent100Records and SEGCO records
(defun read-DellRecordsIntoObjects (filepath createFn &optional (feedback-increment 50000))
  (with-open-file (data filepath :direction :input)
    (loop with counter = 0
	as buffer = (read-line data nil nil)
	when buffer do
	  (incf counter)
	  (funcall createFn buffer)
	  (if (zerop (mod counter feedback-increment))
	      (princ "."))
	until (or (> counter 999999) (null buffer))
	      finally (return counter))))
