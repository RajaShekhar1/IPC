(in-package :COMMON-LISP-USER)

;;; 14-Feb-18 WSD added primarily to extract a map between policies and Employers
(defclass PAYROLL ()
  ;;From SEGPR records
  ((EX-PR-POLICY
    :accessor EX-PR-POLICY
    :initform nil
    :initarg :EX-PR-POLICY)
   (EX-PR-EMPLOYERID			;;matches up to DEM-ID on EMPLOYER record
    :accessor EX-PR-EMPLOYERID
    :initform nil
    :initarg :EX-PR-EMPLOYERID)
   ))

;;; 14-Feb-18 WSD added
(defun create-PayrollObjFromString (data)
  (let ((newObj (make-instance 'PAYROLL))
	(policyObj nil))
    (parse-FixedFieldData data newObj *policyPayrollParsingControlList*)
    ;;unlike other parsing routines, just directly set the Employer ID into the Policy Object
    ;; should rewrite this to not create all these throw-away objects, but easier for now to just reuse the existing parsing routines
    (setf policyObj (gethash (EX-PR-POLICY newObj) *Policies*))
    (when policyObj
      (setf (employerID policyObj) (EX-PR-EMPLOYERID newObj)))
    ;;and leave the newObj to trash - don't need it anymore
    ))