(defclass SUPBENEFIT ()
  ;;From SEGSB records
  ;;Supplemental Benefits define change amounts to premium and/or benefits for the defined start/stop timeframe
  ((EX-SB-POLICY
    :accessor EX-SB-POLICY
    :initform nil
    :initarg :EX-SB-POLICY)
   (EX-SB-TYPE
    :accessor EX-SB-TYPE
    :initform nil
    :initarg :EX-SB-TYPE)
   (EX-SB-PREM
    :accessor EX-SB-PREM
    :initform nil
    :initarg :EX-SB-PREM)
   (EX-SB-BEN
    :accessor EX-SB-BEN
    :initform nil
    :initarg :EX-SB-BEN)
   (EX-SB-BEGIN-DATE
    :accessor EX-SB-BEGIN-DATE
    :initform nil
    :initarg :EX-SB-BEGIN-DATE)
   (EX-SB-END-DATE
    :accessor EX-SB-END-DATE
    :initform nil
    :initarg :EX-SB-END-DATE)
   ))

;;; 12-Jun-28 WSD added
(defun create-SupplementalBenefitObjFromString (data)
  (let ((newObj (make-instance 'SUPBENEFIT)))
    (parse-FixedFieldData data newObj *policySupBenefitParsingControlList*)
    (setf (gethash (EX-SB-POLICY newObj) *SupBenefits*)
      ;;if we already have a list, then add this object to it, otherwise start a new entry
      (let ((existingEntries (gethash (EX-SB-POLICY newObj) *SupBenefits*)))
	(if existingEntries
	    (push newObj existingEntries)
	  (list newObj))))))