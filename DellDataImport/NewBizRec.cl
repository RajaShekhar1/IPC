;;; 13-Nov-14 WSD added to gather policy submission dates
(defclass NEWBIZREC ()
  ;;From SEGNB records
  ;;New Business records record administrative info about policy application receipt, entry and processing 
  ((EX-NB-POLICY
    :accessor EX-NB-POLICY
    :initform nil
    :initarg :EX-NB-POLICY)
   (EX-NB-BRIDGE-DATE
    ;; Date, in YYYYMMDD format, that policy completed all underwriting requirements and bridged from the new bus. application system to the policy master. 
    :accessor EX-NB-BRIDGE-DATE
    :initform nil
    :initarg :EX-NB-BRIDGE-DATE) 
   (EX-NB-RECEIVED-DATE
    ;; Date, in YYYYMMDD format, in which policy was received to enter into New Bus.
    :accessor EX-NB-RECEIVED-DATE
    :initform nil
    :initarg :EX-NB-RECEIVED-DATE)
   ))

;;; 13-Nov-14 WSD added
(defun create-NewBusRecObjFromString (data)
  (let ((newObj (make-instance 'NEWBIZREC)))
    (parse-FixedFieldData data newObj *policyNewBusinessParsingControlList*)
    (setf (gethash (EX-NB-POLICY newObj) *Newbizrecs*) newObj)))








