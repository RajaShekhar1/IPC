(in-package :COMMON-LISP-USER)

;;; 16-Mar-17 WSD 
(defclass INSURED ()
  ;; SEGIN records
  ((EX-IN-POLICY
    :accessor EX-IN-POLICY
    :initform nil
    :initarg :EX-PR-POLICY)
   (EX-IN-SSN
    :accessor EX-IN-SSN
    :initform nil
    :initarg :EX-IN-SSN)
   ))



;;; 16-Mar-17 WSD added
(defun create-InsuredObjFromString (data)
  (let ((newObj (make-instance 'INSURED))
	(policyObj nil))
    (parse-FixedFieldData data newObj *policyInsuredParsingControlList*)
    ;;unlike other parsing routines, just directly set the InsuredSSN into the Policy Object
    ;; should rewrite this to not create all these throw-away objects, but easier for now to just reuse the existing parsing routines
    (setf policyObj (gethash (EX-IN-POLICY newObj) *Policies*))
    (when policyObj
      (setf (insuredSSN policyObj) (EX-IN-SSN newObj)))
    ;;and leave the newObj to trash - don't need it anymore
    ))

