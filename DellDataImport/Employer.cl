;;; 14-Feb-18 WSD added
;;; 14-Feb-21 WSD added agents
;;; 15-Oct-29 WSD added DEM-REGION-ID
(defclass EMPLOYER ()
  ;;from EMPLOYER MASTER DATA MAP
  ;; EMPLOYER.000
  (
   (agents
    ;;list of agentcodes of agents who've written policies for this group
    :accessor agents
    :initform nil
    :initarg :agents)
   ;;
   (DEM-ID
   :accessor DEM-ID
   :initform nil
   :initarg :DEM-ID)
  (DEM-ALTERNATE-REPORTING-ID
   :accessor DEM-ALTERNATE-REPORTING-ID
   :initform nil
   :initarg :DEM-ALTERNATE-REPORTING-ID)
  (DEM-CO-NAME
   :accessor DEM-CO-NAME
   :initform nil
   :initarg :DEM-CO-NAME)
  (DEM-ATTN
   :accessor DEM-ATTN
   :initform nil
   :initarg :DEM-ATTN)
  (DEM-ADDR1
   :accessor DEM-ADDR1
   :initform nil
   :initarg :DEM-ADDR1)
  (DEM-ADDR2
   :accessor DEM-ADDR2
   :initform nil
   :initarg :DEM-ADDR2)
  (DEM-CITY
   :accessor DEM-CITY
   :initform nil
   :initarg :DEM-CITY)
  (DEM-STATE
   :accessor DEM-STATE
   :initform nil
   :initarg :DEM-STATE)
  (DEM-ALT-STATE
   :accessor DEM-ALT-STATE
   :initform nil
   :initarg :DEM-ALT-STATE)
  (DEM-ZIP-CODE
   :accessor DEM-ZIP-CODE
   :initform nil
   :initarg :DEM-ZIP-CODE)
  (DEM-GRP-BEG-YYYY
   :accessor DEM-GRP-BEG-YYYY
   :initform nil
   :initarg :DEM-GRP-BEG-YYYY)
  (DEM-GRP-BEG-MM
   :accessor DEM-GRP-BEG-MM
   :initform nil
   :initarg :DEM-GRP-BEG-MM)
  (DEM-GRP-BEG-DD
   :accessor DEM-GRP-BEG-DD
   :initform nil
   :initarg :DEM-GRP-BEG-DD)
  (DEM-BILL-YYYY
   :accessor DEM-BILL-YYYY
   :initform nil
   :initarg :DEM-BILL-YYYY)
  (DEM-BILL-MM
   :accessor DEM-BILL-MM
   :initform nil
   :initarg :DEM-BILL-MM)
  (DEM-BILL-DD
   :accessor DEM-BILL-DD
   :initform nil
   :initarg :DEM-BILL-DD)
  (DEM-PLN-BEG-YYYY
   :accessor DEM-PLN-BEG-YYYY
   :initform nil
   :initarg :DEM-PLN-BEG-YYYY)
  (DEM-PLN-BEG-MM
   :accessor DEM-PLN-BEG-MM
   :initform nil
   :initarg :DEM-PLN-BEG-MM)
  (DEM-PLN-BEG-DD
   :accessor DEM-PLN-BEG-DD
   :initform nil
   :initarg :DEM-PLN-BEG-DD)
  (DEM-PLAN-TYPE
   :accessor DEM-PLAN-TYPE
   :initform nil
   :initarg :DEM-PLAN-TYPE)
  (DEM-BILL-MODE
   :accessor DEM-BILL-MODE
   :initform nil
   :initarg :DEM-BILL-MODE)
  (DEM-STATUS
   :accessor DEM-STATUS
   :initform nil
   :initarg :DEM-STATUS)
  (DEM-STATUS-YYYY
   :accessor DEM-STATUS-YYYY
   :initform nil
   :initarg :DEM-STATUS-YYYY)
  (DEM-STATUS-MM
   :accessor DEM-STATUS-MM
   :initform nil
   :initarg :DEM-STATUS-MM)
  (DEM-STATUS-DD
   :accessor DEM-STATUS-DD
   :initform nil
   :initarg :DEM-STATUS-DD)
  (DEM-REGION-ID
   :accessor DEM-REGION-ID
   :initform nil
   :initarg :DEM-REGION-ID)
  ))


;;; 14-Feb-18 WSD added
(defun create-EmployerObjFromString (data)
  (let ((newObj (make-instance 'EMPLOYER)))
    (parse-FixedFieldData data newObj *employerMasterParsingControlList*)
    (setf (gethash (DEM-ID newObj) *EmployerGroups*) newObj)))

;;; 14-Feb-18 WSD added
(defun parseEmployerRecords (&optional (dumpfile t))
  (format t "~%[~a]Loading EMPLOYER.000" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s EMPLOYER records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "EMPLOYER.000")
				       #'create-EmployerObjFromString)))

;;; 14-Feb-18 WSD added
(defun getEmployer (employerID)
  (unless (string-equal "" employerID)
    (gethash (string-upcase employerID) *EmployerGroups*)))