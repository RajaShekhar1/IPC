;;; 12-Jun-27 WSD added
(defclass SPLIT ()
  ;;From SEGCO records
  ;;Note:  "SPLIT" per se is a bit of a misnomer, as the original Policy record has the first 2 agents, and the SPLIT record contains the 3rd & 4th co-writing agents if needed
  ((EX-CO-POLICY
    :accessor EX-CO-POLICY
    :initform nil
    :initarg :EX-CO-POLICY)
   (EX-CO-SPLIT2
    :accessor EX-CO-SPLIT2
    :initform nil
    :initarg :EX-CO-SPLIT2)
   (EX-CO-WAGT3
    :accessor EX-CO-WAGT3
    :initform nil
    :initarg :EX-CO-WAGT3)
   (EX-CO-SPLIT3
    :accessor EX-CO-SPLIT3
    :initform nil
    :initarg :EX-CO-SPLIT3)
   (EX-CO-WAGT4
    :accessor EX-CO-WAGT4
    :initform nil
    :initarg :EX-CO-WAGT4)
   ))

;;; 12-Jun-27 WSD added
(defun create-SplitObjFromString (data)
  (let ((newObj (make-instance 'SPLIT)))
    (parse-FixedFieldData data newObj *policyCoAgentParsingControlList*)
    (setf (gethash (EX-CO-POLICY newObj) *Splits*) newObj)))