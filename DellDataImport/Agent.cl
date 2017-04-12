
;;; 12-May-02 WSD added
;;; 12-Jun-20 WSD added AGD-100-ALTERNATE-ID
;;; 12-Jul-02 WSD added periodPremium
;;; 13-Jul-03 WSD added periodPolicyCountVals and periodPolicyCountValsHier
;;; 14-May-22 WSD added AGD-100-EMAIL-ADDR
(defclass AGENT ()
;;;  slotlist AGD-100-AGENT AGD-100-TYPE AGD-100-EFF-DATE AGD-100-SUBCNT
;;;  AGD-100-SSAN AGD-100-NAME AGD-100-BUS-ADDR-1 AGD-100-BUS-ADDR-2
;;;  AGD-100-BUS-CITY AGD-100-BUS-TERR-STATE AGD-100-BUS-ZIP
;;;  AGD-100-BUS-COUNTRY AGD-100-HOME-ADDR-1 AGD-100-HOME-ADDR-2
;;;  AGD-100-HOME-CITY AGD-100-HOME-TERR-STATE AGD-100-HOME-ZIP
;;;  AGD-100-HOME-COUNTRY AGD-100-BUS-PHONE AGD-100-HOME-PHONE AGD-100-SEX
;;;  AGD-100-STATUS AGD-100-AGENT-DATE-OF-BIRTH AGD-100-DATE-HIRED
;;;  AGD-100-DATE-TERMINATED
  (
    (subagents				;list of subagents
     :accessor subagents
     :initform nil)
    (parent				;single upline in hierarchy
     :accessor parent
     :initform nil)
    (hierarchy				;ordered list of all uplines in hierarchy, lowest to highest
     :accessor hierarchy
     :initform nil)
    (contractCode			;identifier from ACT
     :accessor contractCode
     :initform nil)
    (ACTcompany				;identifier from ACT
     :accessor ACTcompany
     :initform nil)
    (periodPremium			;matrix containing premium amounts from THIS agent for period
     :accessor periodPremium
     :initform nil)
    (periodPremiumSubs			;matrix containing premium amounts from ALL SUBHIERARCHY agent (including this agent) for period
     :accessor periodPremiumSubs
     :initform nil)
    (periodPolicyCountVals		;matrix containing policy counts and values from THIS agent for period
     :accessor periodPolicyCountVals	; - entries are stored as dotten pair of [Count . PremValue] where PremValue is total 
     :initform nil)			;   total premium for all included in count.  Different from periodPremium since can't use splits
    (periodPolicyCountValsHier		;matrix containing policy count data from ALL SUBHIERARCHY agents (including this agent) for period
     :accessor periodPolicyCountValsHier
     :initform nil)
    
    
   ;;imported from DELL record 
   (AGD-100-AGENT
    :accessor AGD-100-AGENT
    :initform nil
    :initarg :AGD-100-AGENT)
   (AGD-100-TYPE
    :accessor AGD-100-TYPE
    :initform nil
    :initarg :AGD-100-TYPE)
   (AGD-100-EFF-DATE
    :accessor AGD-100-EFF-DATE
    :initform nil
    :initarg :AGD-100-EFF-DATE)
   (AGD-100-SUBCNT
    :accessor AGD-100-SUBCNT
    :initform nil
    :initarg :AGD-100-SUBCNT)
   (AGD-100-SSAN
    :accessor AGD-100-SSAN
    :initform nil
    :initarg :AGD-100-SSAN)
   (AGD-100-NAME
    :accessor AGD-100-NAME
    :initform nil
    :initarg :AGD-100-NAME)
   (AGD-100-BUS-ADDR-1
    :accessor AGD-100-BUS-ADDR-1
    :initform nil
    :initarg :AGD-100-BUS-ADDR-1)
   (AGD-100-BUS-ADDR-2
    :accessor AGD-100-BUS-ADDR-2
    :initform nil
    :initarg :AGD-100-BUS-ADDR-2)
   (AGD-100-BUS-CITY
    :accessor AGD-100-BUS-CITY
    :initform nil
    :initarg :AGD-100-BUS-CITY)
   (AGD-100-BUS-TERR-STATE
    :accessor AGD-100-BUS-TERR-STATE
    :initform nil
    :initarg :AGD-100-BUS-TERR-STATE)
   (AGD-100-BUS-ZIP
    :accessor AGD-100-BUS-ZIP
    :initform nil
    :initarg :AGD-100-BUS-ZIP)
   (AGD-100-BUS-COUNTRY
    :accessor AGD-100-BUS-COUNTRY
    :initform nil
    :initarg :AGD-100-BUS-COUNTRY)
   (AGD-100-HOME-ADDR-1
    :accessor AGD-100-HOME-ADDR-1
    :initform nil
    :initarg :AGD-100-HOME-ADDR-1)
   (AGD-100-HOME-ADDR-2
    :accessor AGD-100-HOME-ADDR-2
    :initform nil
    :initarg :AGD-100-HOME-ADDR-2)
   (AGD-100-HOME-CITY
    :accessor AGD-100-HOME-CITY
    :initform nil
    :initarg :AGD-100-HOME-CITY)
   (AGD-100-HOME-TERR-STATE
    :accessor AGD-100-HOME-TERR-STATE
    :initform nil
    :initarg :AGD-100-HOME-TERR-STATE)
   (AGD-100-HOME-ZIP
    :accessor AGD-100-HOME-ZIP
    :initform nil
    :initarg :AGD-100-HOME-ZIP)
   (AGD-100-HOME-COUNTRY
    :accessor AGD-100-HOME-COUNTRY
    :initform nil
    :initarg :AGD-100-HOME-COUNTRY)
   (AGD-100-BUS-PHONE
    :accessor AGD-100-BUS-PHONE
    :initform nil
    :initarg :AGD-100-BUS-PHONE)
   (AGD-100-HOME-PHONE
    :accessor AGD-100-HOME-PHONE
    :initform nil
    :initarg :AGD-100-HOME-PHONE)
   (AGD-100-SEX
    :accessor AGD-100-SEX
    :initform nil
    :initarg :AGD-100-SEX)
   (AGD-100-STATUS
    :accessor AGD-100-STATUS
    :initform nil
    :initarg :AGD-100-STATUS)
   (AGD-100-AGENT-DATE-OF-BIRTH
    :accessor AGD-100-AGENT-DATE-OF-BIRTH
    :initform nil
    :initarg :AGD-100-AGENT-DATE-OF-BIRTH)
   (AGD-100-DATE-HIRED
    :accessor AGD-100-DATE-HIRED
    :initform nil
    :initarg :AGD-100-DATE-HIRED)
   (AGD-100-DATE-TERMINATED
    :accessor AGD-100-DATE-TERMINATED
    :initform nil
    :initarg :AGD-100-DATE-TERMINATED)
   (AGD-100-ALTERNATE-ID
    :accessor AGD-100-ALTERNATE-ID
    :initform nil
    :initarg :AGD-100-ALTERNATE-ID)
   (AGD-100-EMAIL-ADDR
    :accessor AGD-100-EMAIL-ADDR
    :initform nil
    :initarg :AGD-100-EMAIL-ADDR)
   ))


;;; 12-May-02 WSD added
(defun create-AgentObjFromString (data)
  (let ((newObj (make-instance 'AGENT)))
    (parse-FixedFieldData data newObj *agentMaster100ParsingControlList*)
    (setf (gethash (AGD-100-AGENT newObj) *Agents*) newObj)
    ;;also setup alternteID index for secondary lookup
    (setf (AGD-100-ALTERNATE-ID newObj) (string-upcase (AGD-100-ALTERNATE-ID newObj)))
    (setf (gethash (AGD-100-ALTERNATE-ID newObj) *AgentsAlternateIDs*)
      (AGD-100-AGENT newObj))
    ))


;;; 12-Jul-10 WSD added -- hash entries are list of objects
(defun create-Agent200ObjFromString (data)
  (let ((newObj (make-instance 'HIERARCHY)))
    (parse-FixedFieldData data newObj *agentMaster200ParsingControlList*)
    (setf (gethash (AGD-200-AGENT newObj) *Agent200Recs*)
      (let ((existingEntries (gethash (AGD-200-AGENT newObj) *Agent200Recs*)))
	(if existingEntries
	    (push newObj existingEntries)
	  (list newObj))))))


(defun parseAgent100Records (&optional (dumpfile t))
  (format t "~%[~a]Loading AGMST100.000" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s Agent100 records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "AGMST100.000")
				       #'create-AgentObjFromString)))

(defun parseAgent200Records (&optional (dumpfile t))
  (format t "~%[~a]Loading AGMST200.000" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s Agent200 records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "AGMST200.000")
				       #'create-Agent200ObjFromString)))


;;; 12-Jun-20 WSD added - use a secondary Index to lookup agents
;;; 12-Jul-12 WSD if string is blank (equivalent to NIL), so fail
(defun getAgentViaAlternateID (alternateID)
  (unless (string-equal "" alternateID)
    (let ((trueKey (gethash (string-upcase alternateID) *AgentsAlternateIDs*)))
      (if trueKey
	  (gethash trueKey *Agents*)))))

;;; 12-Jun-20 WSD added use of getAgentViaAlternateID to have alternate lookup of agent ID (Dell apparently using old keys in data hierarchy file)
(defun getAgent (keyToken &optional (keyType :agentcode))
  (case keyType
    (:agentcode 
     (or (gethash (string-upcase keyToken) *Agents*)
	 (getAgentViaAlternateID keyToken)))
    ((:name :city :zip :busphone :company)
     (if (eq keyType :zip)
	 (setf keyToken (subseq keyToken 0 5)))		;only use first 5 digits
     (let ((possibles nil))
       (maphash #'(lambda (key agentObj)
		    (declare (ignore key))
		    (if (case keyType
			  ((:name :company) (search keyToken (AGD-100-NAME agentObj) :test #'string-equal))
			  (:city (string-equal keyToken (AGD-100-BUS-CITY agentObj)))
			  (:zip (string-equal keyToken (if (>= (length (AGD-100-BUS-ZIP agentObj)) 5)
							   (subseq (AGD-100-BUS-ZIP agentObj) 0 5))))
			  (:busphone (string-equal keyToken (AGD-100-BUS-PHONE agentObj)))
			  (t nil))
			(push agentObj possibles)))
		*Agents*)
       (nreverse possibles)))))


(defmacro agentHier-firstName (agentTuple)
  `(nth 0 ,agentTuple))
(defmacro agentHier-lastName (agentTuple)
  `(nth 1 ,agentTuple))
(defmacro agentHier-agentCode (agentTuple)
  `(let* ((rawCode (nth 3 ,agentTuple))
	  (rawLength (length rawCode)))
     (if (<= rawLength 5)
	 rawCode
       (subseq rawCode (- rawLength 5)))))
(defmacro agentHier-contractCode (agentTuple)
  `(nth 4 ,agentTuple))
(defmacro agentHier-Company (agentTuple)
  `(nth 7 ,agentTuple))
(defmacro agentHier-referral (agentTuple)
  `(nth 14 ,agentTuple))

(defmacro tagHier-agentCode5 (agentTuple)
  `(let* ((rawCode (string-trim " " (nth 1 ,agentTuple)))
	  (rawLength (length rawCode)))
     (if (<= rawLength 5)
	 rawCode
       (subseq rawCode (- rawLength 5)))))
(defmacro tagHier-agentCode (agentTuple)
  `(string-trim " " (nth 1 ,agentTuple)))

;;; 12-Jul-05 WSD added
(defun establish-agentAliases ()
  (setf *AgentAliases* nil)
  (let ((names (make-hash-table)))
    (macrolet ((strip (str)
		 `(delete #\' (delete #\. (delete #\, (delete #\! (delete #\& (remove #\^ (remove #\space (string-upcase ,str))))))))))
      ;;find all agents with "nearly identical" names
      (map-agents #'(lambda (a) 
		      (let* ((agentNameSymbol (read-from-string (strip (AGD-100-NAME a)))))
			(setf (gethash agentNameSymbol names) 
			  (if (gethash agentNameSymbol names)
			      (push (AGD-100-AGENT a) (gethash agentNameSymbol names))
			    (list (AGD-100-AGENT a))))))))
    ;;now for all those agents with multiple entries on same name, consider any with same parent as alias entries
    (maphash #'(lambda (k agentCodes)
		 (declare (ignore k))
		 ;; have to make multiple passes, as multiple sets of aliases could appear with diff parents
		 (loop while (> (length agentCodes) 1) do
		      (loop with firstAgentCode = (car agentCodes) 
			 with firstParent = (parent (getAgent firstAgentCode))
			 for agentCode in (cdr agentCodes)
			 when (eq (parent (getagent agentCode))
				  firstParent)
			 collect agentCode into aliasCodes
			 finally
			   (progn (if aliasCodes
				      (push (push firstAgentCode aliasCodes)
					    *AgentAliases*))
				  (setf agentCodes
					;;remove the car and any aliases thereof for the next pass
					(delete-if
					 #'(lambda (code)
					     (find code aliasCodes :test #'string-equal))
					 (cdr agentCodes)))))))		   
	     names)))

;;; 14-Mar-04 WSD temporarily needed until Dell completes digesting recent sweeping edits to agent hierarchy
(defun setup-NewRMDAgentObjects ()
    (loop for (code . name) in '(("26Z1A" . "Home Office") ("26Z4A" . "SouthCentral Region")
				 ("26Z5A" . "Western Region") ("26Z6A" . "GreatLakes Region"))
	as newObj = (make-instance 'AGENT) do
	  (setf (AGD-100-AGENT newObj) code
		(AGD-100-NAME newObj) name)
	  (setf (gethash code *Agents*) newObj)))

;;; 12-May-15 WSD added
(defun dump-orphanedAgents (&optional (dumpPath (join outputDocumentPath "agent-orphans.csv")))
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format dumpfile "agentcode,contractcode,name,company,city,state")
    (loop for agent in (orphaned-agentsafteryear 2012)
	do
	  (format dumpfile "~%~a,~a,~s,~s,~s,~a"
		  (AGD-100-AGENT agent)
		  (or (contractCode agent) "")
		  (agent-nameLastFirst agent)
		  (or (ACTcompany agent) "")
		  (or (AGD-100-BUS-CITY agent) "")
		  (or (AGD-100-BUS-TERR-STATE agent) "")))))


(defun print-listOfAgents (agentList &key (stream t) (name-p nil))
  (format stream "(~a~a"
	  (if agentList (AGD-100-AGENT (car agentList)) "")
	  (if name-p (concatenate 'string "[" (AGD-100-NAME (car agentList)) "]") ""))
  (dolist (a (cdr agentList))
    (format stream ", ~a~a"
	    (AGD-100-AGENT a)
	    (if name-p
		(concatenate 'string "[" (AGD-100-NAME a) "]")
	      "")))
  (format stream ")"))

;;; 12-May-11 WSD added for pretty display of agent name
(defmethod agent-nameLastFirst ((agent AGENT))
  (let* ((name (string-capitalize (AGD-100-NAME agent))) ;no affect on original slotvalue
	 (namelen (length name))
	 (bang-pos (position #\! name)))
    (if bang-pos
	(if (= bang-pos (- namelen 1))
	    (subseq name 0 (- namelen 1)) ;company name
	  (nsubstitute #\, #\! name))	;individual name
      name)))

(defmethod agent-nameLastFirst ((agentCode STRING))
  (let ((agent (getAgent agentCode)))
    (if agent
	(agent-nameLastFirst agent)
      (format nil "-no agent ~a-" agentCode))))


;;; 12-May-14 WSD added for pretty-printing to include company name if desired
;;; 12-Jun-25 WSD changed to not use the ACTcompany until I determine to pull that data in
(defun agent-nameAndCompany (agent)
  (agent-nameLastFirst agent))

;;; 12-May-16 WSD added
(defun agent-nameCompanyCodeContract (agent)
  (concatenate 'string
    (agent-nameAndCompany agent) " ["
    (AGD-100-AGENT agent) ", "
    (contractCode agent) "]"))

;;; 12-May-11 WSD print the entire hierarchy outline below IMA
(defun print-agentHierarchy (root level max-level &optional (imaHierFile t))
  (format imaHierFile "~%~a~a ~a [~a"
	  (make-string (* 3 level) :initial-element #\space)
	  (make-string level :initial-element (if (get-policies :agentCode (AGD-100-AGENT root))
						  #\+
						#\-))
	  (agent-nameAndCompany root)
	  (AGD-100-AGENT root))
  (if (contractCode root)
      (format imaHierFile ", ~a]" (contractCode root))
    (format imaHierFile "]"))

  (when (or (null max-level)
	    (< level max-level))
    (loop for subagent in (sort (copy-list (subagents root)) #'string-lessp :key #'AGD-100-NAME)
	do (print-agentHierarchy subagent (+ 1 level) max-level imaHierFile))))

;;; 12-May-11 WSD added
(defun ima (&optional (max-level 10))
  (with-open-file (imaHierFile
		   (join outputDocumentPath "IMA-hierarchy.txt")
		   :direction :output :if-exists :supersede :if-does-not-exist :create)

    (format imaHierFile "Insurance Marketing Alliancem LLC")
    (loop for agent in (sort (copy-list (subagents (getAgent "CCO26")))
			     #'(lambda (a b)
				 ;;special case for RMDs to come last
				 (cond
				  ;;A is an RMD, should be after B unless B is too
				  ((member (AGD-100-AGENT a) '("O26Z1" "O26Z4" "O26Z5" "O26Z6" "A26Z4") :test #'string-equal)
				   (if (member (AGD-100-AGENT b) '("O26Z1" "O26Z4" "O26Z5" "O26Z6" "A26Z4")  :test #'string-equal)
				       ;;if both, then sort by ending digits on agentcode
				       (string-lessp (AGD-100-AGENT a) (AGD-100-AGENT b) :start1 1 :start2 1)
				     nil))
				  ;;A is not an RMD, but B is, then B always "greater"
				  ((member (AGD-100-AGENT b) '("O26Z1" "O26Z4" "O26Z5" "O26Z6" "A26Z4")  :test #'string-equal)
				   t)
				  ;;otherwise, normal alphabetical name
				  (t (string-lessp (AGD-100-NAME a) (AGD-100-NAME b))))))
	do
	  (print-agentHierarchy agent 1 max-level imaHierFile))))


(defmethod show-upline ((agent t))
  "No agent found")

(defmethod show-upline ((agent STRING))
  (show-upline (getagent agent)))

(defmethod show-upline ((agent AGENT))
  (format t "~a" (agent-nameCompanyCodeContract agent))
  (when (parent agent)
    (format t " --> ")
    (show-upline (parent agent))))


(defmethod show-upline ((agentHier HIERARCHY))
  (let ((agent (getAgent (AGD-200-AGENT agentHier)))
	parentCode)
    (format t "~%~a [~a]" (AGD-100-NAME agent) (AGD-200-AGENT agentHier))
    (dotimes (i (AGD-200-HIER-COUNT agentHier))
      (setf parentCode (case i
			 (0 (AGD-200-HT-AGENT-1 agentHier))
			 (1 (AGD-200-HT-AGENT-2 agentHier))
			 (2 (AGD-200-HT-AGENT-3 agentHier))
			 (3 (AGD-200-HT-AGENT-4 agentHier))
			 (4 (AGD-200-HT-AGENT-5 agentHier))
			 (5 (AGD-200-HT-AGENT-6 agentHier))
			 (6 (AGD-200-HT-AGENT-7 agentHier))))
      (format t "  >>> ~a [~a]" (AGD-100-NAME (getAgent parentCode)) parentCode))))

(defmacro su (agent)
  `(show-upline ,agent))

(defmethod get-aliases ((agent agent))
  (get-aliases (AGD-100-AGENT agent)))

(defmethod get-aliases ((agentCode string))
  (find agentCode *AgentAliases* :test #'(lambda (x y) (find x y :test #'string-equal))))

(defmethod agentPremium ((agent t) (policy STRING) &key &allow-other-keys)
(agentPremium agent (gethash policy *policies*)))

(defmethod agentPremium ((agent AGENT) (policy POLICY) &key &allow-other-keys)
(agentPremium (AGD-100-AGENT agent) policy))

;;; 12-Jun-29 WSD
;;; 15-Aug-10 WSD added include-splits?
(defmethod agentPremium ((agentcode STRING) (policy POLICY) &key (include-splits? t) &allow-other-keys)
(let ((prem (annualPremium policy))
(agentSplit (if include-splits?
        (cdr (find agentcode (splits policy) :key #'car :test #'string-equal))
        (if (string-equal agentcode (EX-WRITING-AGENT1 policy))
            1.0
            0)
        )))
  (if agentSplit (* agentSplit prem) 0)))


;;; 15-Aug-10 WSD added include-splits?
(defun agentPremiumForYear (agentcode year &key (include-splits? t))
(reduce #'+
  (mapcar #'(lambda (p)
          (agentPremium agentcode p :include-splits? include-splits?))
      (get-policies :startYr year :endYr year :agentcode agentcode))
  :initial-value 0))

;;; 12-Jul-02 WSD added - maybe superfluous
(defun reset-agentPeriodPremiums ()
(map-agents #'(lambda (a) (setf (periodPremium a) nil
              (periodPremiumSubs a) nil))))

;;; 15-Mar-10 WSD added only-paid-policies?
;;; 15-Aug-10 WSD added include-splits? to possibly credit 100% of premium to writing agent
(defun compute-individualAgentPremiumsForPeriod (periodTypeKey startYear endYear
						 &key (only-paid-policies? nil) (include-splits? t))
;;map through all Policies within startYear to endYear, and attribute split policy premium to agents' periodPremium
;;Agents' periodPremium will be a tuple of values according to the following pattern:
;; :annual   - array of single amounts for each year in range
;; :quarter  - same as :annual, but array dimensions are #years by 4
;; :month    - same as :quarter, but array dimensions are #years x 12
(reset-agentPeriodPremiums)
(map-policies
#'(lambda (policy)
   (multiple-value-bind (sec min hr day mo yr) (decode-universal-time (EX-ISSUE-DATE policy))
 (declare (ignore sec min hr day))
 (loop with totalPremium = (annualPremium policy)
     with yearOffset = (- yr startYear)
      ;; either credit according to split agents, or else count 100% as with Writing Agent
     for (agentCode . split) in (if include-splits?
                    (splits policy)
                    (list (cons (EX-WRITING-AGENT1 policy) 1.0)))
     as agent = (getAgent agentCode)
     as agentPremium = (* split totalPremium)
     unless (null agent)
     do
       ;;if first encounter with agent this go 'round, then setup the periodPremium slot
       (unless (periodPremium agent)
     (setf (periodPremium agent) (setup-zeroPremiumArray periodTypeKey startYear endYear)))
       ;;now store values accordingly
       (case periodTypeKey
     (:annual (incf (aref (periodPremium agent) yearOffset) agentPremium))
     (:quarter (incf (aref (periodPremium agent) yearOffset (floor (- mo 1) 3)) agentPremium))
     (:month (incf (aref (periodPremium agent) yearOffset (- mo 1)) agentPremium))
     ))))
:startYr startYear :endYr endYear
:notstatuslist '(:declined)
:only-paid-policies? only-paid-policies?
))

;;; 13-Jul-03 WSD similar to compute-individualAgentPremiumsForPeriod, but for agents' policy count & value data
;;; 15-Mar-10 WSD added only-paid-policies?
(defun compute-individualAgentPolicyCountValsForPeriod (startYear endYear &optional (startMonth 1) (endMonth 12) (only-paid-policies? nil))
  ;;map through all Policies within startMonth/startYear to endMonth/endYear, and tally policy totals for the agent in "agent1"
  ;;according to planCode, include summing total premium value for that counted policy
  (reset-agentPeriodPolicyCountVals)
  (setup-planCodeIndexForPolicyCounting startYear endYear)
  (map-policies
   #'(lambda (policy)
       (let* ((agentCode (EX-WRITING-AGENT1 policy))
	      (agent (getAgent agentCode))
	      (planCodeIndex (cdr (assoc (EX-PLAN-AND-OPT policy) *PlanCodeIndex* :test #'string-equal))))
	 (unless (or (null agent)
		     (member agentCode *agentCodesExcludedFromCalculations* :test #'string-equal))
	   ;;if first encounter with agent this go 'round, then setup the periodPremium slot
	   (if (null (periodPolicyCountVals agent))
	     (setf (periodPolicyCountVals agent) (setup-zeroPolicyCountArray)))
	   ;;now store values accordingly
	   (incf (car (aref (periodPolicyCountVals agent) planCodeIndex)) 1)
	   (incf (cdr (aref (periodPolicyCountVals agent) planCodeIndex)) (annualPremium policy)))))
   :startYr startYear :endYr endYear :startMo startMonth :endMo endMonth
   :notstatuslist '(:declined)
   :only-paid-policies? only-paid-policies?
   ))



(defun map-agents (mapFn &key name company state)
  "map a given function across all agents depending on any constraints.  NAME and COMPANY are 'contains' matches, and STATE is 'equals'"
  (flet ((include-agent-p (agentObj)
	   (let ((agent-name (AGD-100-NAME agentObj))
		 (agent-company (ACTcompany agentObj))
		 (agent-state (AGD-100-BUS-TERR-STATE agentObj)))
	     (and (or (not name) (SEARCH name agent-name :TEST #'CHAR-EQUAL))
		  ;;company might be just in the name, for DELL data, or else imported from ACT
		  (or (not company) (and agent-company (or (SEARCH company agent-name :TEST #'CHAR-EQUAL)
							   (SEARCH company agent-company :TEST #'CHAR-EQUAL))))
		  (or (not state) (and agent-state (string-equal state agent-state)))))))
    (maphash #'(lambda (key agent)
		 (declare (ignore key))
		 (when (include-agent-p agent)
		   (funcall mapFn agent)))
	     *Agents*)))

(defun get-agents (&key name company state)
  (let ((collected-agents nil))
    (map-agents #'(lambda (agent)
		    (push agent collected-agents))
		:name name
		:company company
		:state state)
    (sort collected-agents #'string-lessp :key #'AGD-100-NAME)))

;;; 12-May-15 WSD added
(defun is-subagent-of? (lowerAgent targetAncestorAgent)
    ;;if no parent, then we're at terminal of upline; otherwise recurse until find a match
    (if (parent lowerAgent)
	(or (eq (parent lowerAgent) targetAncestorAgent)
	    (is-subagent-of? (parent lowerAgent) targetAncestorAgent))))

;;; 15-Apr-14 WSD added as utility for output file
;;; 15-May-12 WSD premiums still in an array, needed to look at first (assume annual)
;;; 16-May-09 WSD added (not (numberp ...   was getting a ZERO value as an agent premium hash value (I think this is when we have a non-producing agent in the mix?)
(defun topMostAgentAmongAgentList (agentList &optional agentPremiumHash)
  (car (sort agentList #'(lambda (a1 a2)
			   (or (is-subagent-of? a2 a1)
			       (and agentPremiumHash
				    (not (numberp (gethash a2 agentPremiumHash))) ;these should be array's, not raw numbers or zeros
				    (not (numberp (gethash a1 agentPremiumHash)))
				    (> (aref (gethash a2 agentPremiumHash) 0)
				       (aref (gethash a1 agentPremiumHash) 0)))))
	     :key #'getagent)))

#|
  (if (> 1 (length agentList))
      (loop for remaining on agentList
	 as agent = (car remaining)
	 unless (find (getagent agent) (cdr remaining)
		      :test #'is-subagent-of? :key #'getagent)
	 return agent)
      (car agentList)))
|#

;;; 15-Apr-14 WSD added as utility for output file
(defun topMostAgentBelowRegion (agent)
  (if (or (null agent)
	  (find agent (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*))
	  (find (parent agent) (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*)))
      agent
      (topMostAgentBelowRegion (parent agent))))


;;; 12-May-15 WSD added
;;; 13-Jul-12 WSD updated to find all agents even among splits and not merely single-writing agent
;;; 14-Mar-03 WSD changed imaRec to homeOfficeRec
(defun orphaned-agentsAfterYear (startYear)
  (let ((homeOfficeRec (getAgent "26Z1A"))
	(collected-agents nil))
    (map-policies #'(lambda (policy)
		      (dolist (agentCode.Split (splits policy))
			(let ((polAgent (gethash (car agentCode.Split) *Agents*))) ;some policies have no or "" agentcode
			  (when polAgent
			    (unless (is-subagent-of? polAgent homeOfficeRec)
			      (push polAgent collected-agents))))))
		  :startYr startYear)
    (sort (delete-duplicates collected-agents) #'string-lessp :key #'AGD-100-NAME)))


(defun la (&key name company state)
   "diagnostic to '(L)ist (A)gents' fitting given criteria"
  (let ((agentlist (get-agents :name name :company company :state state)))
    (if (or (< (length agentlist) 20)
	    (y-or-n-p "Found ~s agents.  Display them?" (length agentlist)))
	(dolist (a agentlist)
	  (format t "~%~a ~a /~a/  ~a, ~a"
		  (AGD-100-AGENT a)
		  (agent-nameAndCompany a)
		  (if (contractCode a) (contractCode a) "*")
		  (AGD-100-BUS-CITY a)
		  (AGD-100-BUS-TERR-STATE a)))
      )))

;;; 13-Feb-15 WSD changed "Judy" to "WEST" and "IMA" to "Other"
;;; 13-Jul-02 WSD changed "John" to "Atlantic" and "Other" to "H.O.", also remove "WEST" from choices to default to H.O.
;;; 14-Mar-04 WSD changed to new revion names
(defun RMD-shortname (agent)
  ;;assumes prep-mainAgentObjects has been called
  (cond
   ((is-subagent-of? agent *GLakesRegion*) "GreatLakes")
   ((is-subagent-of? agent *WestRegion*) "Western")
   ((is-subagent-of? agent *CentralRegion*) "SouthCentral")
   ((is-subagent-of? agent *HomeOffice*) "HomeOffice")
   (t " - ")))