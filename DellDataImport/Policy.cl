;;; 12-Jun-27 WSD added splits
;;; 13-Nov-14 WSD added bridge-date and received-date
;;; 14-Feb-18 WSD added employerID
;;; 16-Aug-04 WSD added riders
(defclass POLICY ()
  (
   (splits
    ;;splits is a list of conses of the form (Agent-ID . SplitPercentage)
    :accessor splits
    :initform nil
    :initarg :splits)
   (bridge-date
    ;;from NB record
    :accessor bridge-date
    :initform nil
    :initarg :bridge-date)
   (received-date
    ;;from NB record
    :accessor received-date
    :initform nil
    :initarg :received-date)
   (employerID
    ;;from PR record
    :accessor employerID
    :initform nil
    :initarg :employerID)
   (insuredSSN
    ;;from IN record
    :accessor insuredSSN
    :initform nil
    :initarg :insuredSSN)
   (riderPolicies
    ;;list of policies; computed after parsing all policies
    :accessor riderPolicies
    :initform '()
    :initarg :riderPolicies)
   (isRider?
    :accessor isRider?
    :initform nil ;;"no" by default
    :initarg :isRider?)
   
   
   (EX-POLICY 
    :accessor EX-POLICY 
    :initform nil
    :initarg :EX-POLICY)
   (EX-SEG-ID  
    :accessor EX-SEG-ID
    :initform nil
    :initarg :EX-SEG-ID)
   (EX-INS-NAME 
    :accessor EX-INS-NAME
    :initform nil
    :initarg :EX-INS-NAME)
   (EX-INS-SEX 
    :accessor EX-INS-SEX
    :initform nil
    :initarg :EX-INS-SEX)
   (EX-INS-DOB
    :accessor EX-INS-DOB
    :initform nil
    :initarg :EX-INS-DOB)
   (EX-PAYER-NAME
    :accessor EX-PAYER-NAME
    :initform nil
    :initarg :EX-PAYER-NAME)
   (EX-PAYER-ADDR1
    :accessor EX-PAYER-ADDR1
    :initform nil
    :initarg :EX-PAYER-ADDR1)
   (EX-PAYER-ADDR2
    :accessor EX-PAYER-ADDR2
    :initform nil
    :initarg :EX-PAYER-ADDR2)
   (EX-PAYER-CITY
    :accessor EX-PAYER-CITY
    :initform nil
    :initarg :EX-PAYER-CITY)
   (EX-PAYER-STATE
    :accessor EX-PAYER-STATE
    :initform nil
    :initarg :EX-PAYER-STATE)
   (EX-PAYER-ZIP
    :accessor EX-PAYER-ZIP
    :initform nil
    :initarg :EX-PAYER-ZIP)
   (EX-DUE-DATE
    :accessor EX-DUE-DATE
    :initform nil
    :initarg :EX-DUE-DATE)
   (EX-PAYCODE
    :accessor EX-PAYCODE
    :initform nil
    :initarg :nil)
   (EX-MODE
    :accessor EX-MODE
    :initform nil
    :initarg :EX-MODE)
   (EX-BILL-KEYPOL
    :accessor EX-BILL-KEYPOL
    :initform nil
    :initarg :EX-BILL-KEYPOL)
   (EX-SUSPENSE-AMT
    :accessor EX-SUSPENSE-AMT
    :initform nil
    :initarg :EX-SUSPENSE-AMT)
   (EX-LAST-BILLDATE
    :accessor EX-LAST-BILLDATE
    :initform nil
    :initarg :EX-LAST-BILLDATE)
   (EX-PLAN-AND-OPT
    :accessor EX-PLAN-AND-OPT
    :initform nil
    :initarg :EX-PLAN-AND-OPT)
   (EX-ISSAGE
    :accessor EX-ISSAGE
    :initform nil
    :initarg :EX-ISSAGE)
   (EX-ISSUE-DATE
    :accessor EX-ISSUE-DATE
    :initform nil
    :initarg :EX-ISSUE-DATE)
   (EX-STATUS
    ;;One digit code for status of policy. Valid values are:
    ;;"" or nil - active
    ;;"B" - administrative rewrite
    ;;"C" - cancelled
    ;;"D" - deceased
    ;;"E" - expired
    ;;"L" - lapsed
    ;;"M" - matured
    ;;"P" - paidup
    ;;"R" - reinstated
    ;;"S" - surrendered
    ;;"X" - extended term
    ;;"Y" - undelivered
    ;;"Z" - declined
    :accessor EX-STATUS
    :initform nil
    :initarg :EX-STATUS)
   (EX-STATUS-DATE
    :accessor EX-STATUS-DATE
    :initform nil
    :initarg :EX-STATUS-DATE)
   (EX-LIFE-ANNUAL-PREM
    :accessor EX-LIFE-ANNUAL-PREM
    :initform nil
    :initarg :EX-LIFE-ANNUAL-PREM)
   (EX-FACE-AMOUNT
    :accessor EX-FACE-AMOUNT
    :initform nil
    :initarg :EX-FACE-AMOUNT)
   (EX-BASIC-PREM
    :accessor EX-BASIC-PREM
    :initform nil
    :initarg :EX-BASIC-PREM)
   (EX-WRITING-AGENT1
    :accessor EX-WRITING-AGENT1
    :initform nil
    :initarg :EX-WRITING-AGENT1)
   (EX-SBCNT1
    :accessor EX-SBCNT1
    :initform nil
    :initarg :EX-SBCNT1)
   (EX-SPLIT-PCT1
    :accessor EX-SPLIT-PCT1
    :initform nil
    :initarg :EX-SPLIT-PCT1)
   (EX-WRITING-AGENT2
    :accessor EX-WRITING-AGENT2
    :initform nil
    :initarg :EX-WRITING-AGENT2)
   (EX-SBCNT2
    :accessor EX-SBCNT2
    :initform nil
    :initarg :EX-SBCNT2)
   ))

(defun create-PolicyObjFromString (data)
  (let ((newObj (make-instance 'POLICY)))
    (parse-FixedFieldData data newObj *policyParsingControlList*)
    ;; *****HACKING TO ELIMINATE RIDERS
    ;; ********************************
    ;;(unless (find (aref (EX-POLICY newObj) (- (length (EX-POLICY newObj)) 1)) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (setf (gethash (EX-POLICY newObj) *Policies*) newObj)
    ;;)
    ))

(defun parsePolicyAARecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGAA.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s PolicyAA records"
    (read-DellRecordsIntoObjects (join inputDocumentPath "SEGAA.TXT")
				       #'create-PolicyObjFromString)))

(defun parsePolicyCORecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGCO.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s SEG-CO records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "SEGCO.TXT")
				       #'create-SplitObjFromString)))

;;; 13-Nov-14 WSD added
(defun parsePolicyNBRecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGNB.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s SEG-NB records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "SEGNB.TXT")
				       #'create-NewBusRecObjFromString)))

(defun parsePolicySBRecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGSB.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s SEG-SB records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "SEGSB.TXT")
				       #'create-SupplementalBenefitObjFromString)))

;;; 14-Feb-18 WSD added
(defun parsePolicyPRRecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGPR.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s SEG-PR records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "SEGPR.TXT")
				       #'create-PayrollObjFromString)))

;;; 16-Mar-17 WSD added
(defun parsePolicyINRecords (&optional (dumpfile t))
  (format t "~%[~a]Loading SEGIN.TXT" (timestamp (get-universal-time)))
  (format dumpfile "~%counter=~s SEG-IN records"
	  (read-DellRecordsIntoObjects (join inputDocumentPath "SEGIN.TXT")
				       #'create-InsuredObjFromString)))



;;; 12-Jun-28 WSD added
;;; 15-Sep-07 WSD replaced error full-stop with warning when blank/error in split pct found - just assign 100% to Agent1
(defun compute-policySplits ()

  (macrolet ((rounding (n) `(/ (round (* ,n 10000)) 10000)))
    (map-policies #'(lambda (policy)
    (let ((splitRecord (gethash (EX-POLICY policy) *splits*)))
	            

		      ;(format t "processing policy ~s" (EX-POLICY policy))
    (cond
		       ;;error claus(es) first - should not have a blank split-pct1 AND other splits
      ( (and (not (EX-SPLIT-PCT1 policy)) splitRecord)
			  (format t "~2%***ERROR:  <<policy ~s has no split-percentage for Agent1 but does have a matching split record.>>~%" (EX-POLICY policy))
			   ;;when this error occurs, just bail and assign agent1
			  (setf (splits policy) 
			   (list (cons (EX-WRITING-AGENT1 policy) 1.0))
        )
      )
		        ;;main clauses to assign split percentages, even if 100% (1.0) to Agent1
		  ( (not (EX-SPLIT-PCT1 policy))
			  (setf (splits policy) (list (cons (EX-WRITING-AGENT1 policy) 1.0)))
      )
		  


      ( (not splitRecord)
        (handler-bind
          (
            (simple-type-error
             (lambda (c)
                (format t "~%processing policy ~s ~%split record: ~s ~%branch 1 ~%failed because of: ~s" (EX-POLICY policy) splitRecord c)
             )
            ) 
          )

          (setf (splits policy) (list (cons (EX-WRITING-AGENT1 policy) (EX-SPLIT-PCT1 policy))  
					  (cons (EX-WRITING-AGENT2 policy) (rounding (- 1.0 (EX-SPLIT-PCT1 policy))))))
        )
      )


      ( (not (EX-CO-SPLIT3 splitRecord))
        (handler-bind   
          (
            (simple-type-error
              (lambda (c)
                (format t "~%processing policy ~s ~%split record: ~s ~%branch 2 ~%failed because of: ~s" (EX-POLICY policy) splitRecord c)
                (format t "~%EX-WRITING-AGENT1:~s" (EX-WRITING-AGENT1 policy) )
                (format t "~%EX-SPLIT-PCT1:~s" (EX-SPLIT-PCT1 policy) )
                (format t "~%EX-WRITING-AGENT2:~s" (EX-WRITING-AGENT2 policy) )
                (format t "~%EX-CO-SPLIT2:~s" (EX-CO-SPLIT2 splitRecord) )
                (format t "~%EX-CO-WAGT3:~s" (EX-CO-WAGT3 splitRecord))
              )
            ) 
          )
		      (setf (splits policy) (list (cons (EX-WRITING-AGENT1 policy) (EX-SPLIT-PCT1 policy))
					    (cons (EX-WRITING-AGENT2 policy) (EX-CO-SPLIT2 splitRecord))
					    (cons (EX-CO-WAGT3 splitRecord) (rounding (- 1.0 
											 (EX-SPLIT-PCT1 policy)
											 (EX-CO-SPLIT2 splitRecord)))))
          )
        )
      )     
      ( t 
        (handler-bind   
          (
            (simple-type-error
              (lambda (c)
                (format t "~%processing policy ~s ~%split record: ~s ~%branch 3 ~%failed because of: ~s" (EX-POLICY policy) splitRecord c)
              )
            ) 
          )
          (setf (splits policy) (list (cons (EX-WRITING-AGENT1 policy) (EX-SPLIT-PCT1 policy))
						    (cons (EX-WRITING-AGENT2 policy) (EX-CO-SPLIT2 splitRecord))
						    (cons (EX-CO-WAGT3 splitRecord) (EX-CO-SPLIT3 splitRecord))
						    (cons (EX-CO-WAGT4 splitRecord) (rounding (- 1.0 
												 (EX-SPLIT-PCT1 policy)
												 (EX-CO-SPLIT2 splitRecord)
												 (EX-CO-SPLIT3 splitRecord)))))
          )
        )

      )
    )
))))
)

;;; 13-Nov-14 WSD added
(defun setup-newBizDatesInPolicies ()
  (maphash #'(lambda (key NewBizObj)
	       (let ((pol (gethash key *Policies*)))
		 (when pol
		   (setf (bridge-date pol) (EX-NB-BRIDGE-DATE NewBizObj)
			 (received-date pol)(EX-NB-RECEIVED-DATE NewBizObj)))))
	   *Newbizrecs*))


;;; 16-Aug-04 WSD added to handle/convert riders for BGL & VGL
(defun setup-riderPolicies ()
  (map-policies #'(lambda (pol)
		    (let ((polNum (EX-POLICY pol)))
		      (when (find (aref polNum (- (length polNum) 1))
				"ABCDEFGHIJKLMNOPQRSTUVWXYZ")
		      (let* ((basePolNum (subseq polNum 0 (- (length polNum) 1)))
			     (basePol (gethash basePolNum *Policies*)))
			(if basePol
			    (progn
			      (setf (isRider? pol) t)
			      (push pol (riderPolicies basePol)))
			    ;;else
			    (format t "~%>> Base Policy not found for rider ~a<<<" (EX-POLICY pol)))))))
		:include-riders? t
		))

;;; 16-Aug-12 WSD created - find base policy of a rider policy
(defmethod base-policy ((rider POLICY))
  (when (isRider? rider)
    (let* ((polNum (EX-POLICY rider))
	   (basePolNum (subseq polNum 0 (- (length polNum) 1))))
      (gethash basePolNum *Policies*))))

(defmethod annualPremium ((policy t))
  (format t "~%<<annualPremium error:  Can't find policy for ~s" policy)
  0)

(defmethod annualPremium ((policyID string))
  (annualPremium (gethash policyID *policies*)))

;;; 16-Aug-04 WSD added riders into premium sum
(defmethod annualPremium ((policy POLICY))
  ;;premium is the basic premium (for the modal period), plus any fee adjustment or rate adjustment, all multiplied by mode #
  ;;Note:  technically, these fee or rate adjustments are only valid within a begin/end date window, but we're just going to
  ;;       merely take the first one we find whose valid window overlaps the first year of Policy Issue
  ;;       Rates are marked by type 008, and Fees by type 009
  (let* ((policyIssueDate (EX-ISSUE-DATE policy))
	 (firstYear (yearLater policyIssueDate))
	 (supBenefitsToPolicy (gethash (EX-POLICY policy) *SupBenefits*))
	 (SB_008-supBenefit (find-if #'(lambda (sup)
					 (and (string-equal "+008" (EX-SB-TYPE sup))
					      (dateWindows2OverlapsWindow1? policyIssueDate firstYear
									    (EX-SB-BEGIN-DATE sup) (EX-SB-END-DATE sup))))
				     supBenefitsToPolicy))
	 (SB_008-rateIncrease (if SB_008-supBenefit
				  (EX-SB-PREM SB_008-supBenefit)
				0))
	 (SB_009-supBenefit (find-if #'(lambda (sup)
					 (and (string-equal "+009" (EX-SB-TYPE sup))
					      (dateWindows2OverlapsWindow1? policyIssueDate firstYear
									    (EX-SB-BEGIN-DATE sup) (EX-SB-END-DATE sup))))
				     supBenefitsToPolicy))
	 (SB_009-fees (if SB_009-supBenefit
			  (EX-SB-PREM SB_009-supBenefit)
			0)))
    (+ (* (read-from-string (EX-MODE policy)) (+ (EX-BASIC-PREM policy) SB_008-rateIncrease SB_009-fees))
       (if (riderPolicies policy)
	   (loop for rider in (riderPolicies policy)
		summing (annualPremium rider))
	   ;;else nothing to add
	   0))
    ))

(defun setup-zeroPremiumArray (periodTypeKey startYear endYear)
  (let ((numYears (+ (- endYear startYear) 1)))
    (make-array (case periodTypeKey
		  (:annual (list numYears))
		  (:quarter (list numYears 4))
		  (:month (list numYears 12)))
		:initial-element 0)))

;;; 13-Jul-03 WSD added
(defun reset-agentPeriodPolicyCountVals ()
  (map-agents #'(lambda (a) (setf (periodPolicyCountVals a) nil
				  (periodPolicyCountValsHier a) nil))))

;;; 13-Jul-03 WSD added
(defun setup-planCodeIndexForPolicyCounting (startYear endYear)
  ;; find all the unique plancodes, sort and index them for use as indices later
  (let ((planCodesForPeriod nil))
    (map-policies
     #'(lambda (policyObj)
	 (pushnew (string-upcase (EX-PLAN-AND-OPT policyObj)) planCodesForPeriod
		  :test #'string-equal))
     :STARTYR startYear :ENDYR endYear :NOTSTATUSLIST '(:declined))

    (setf *PlanCodeIndex*
      (let ((i -1))
	(map 'list #'(lambda (z) (cons z (incf i))) (sort planCodesForPeriod #'string-lessp))))
    (setf *QtyPlanCodesForPeriod* (length *PlanCodeIndex*))
    ))

(defun setup-zeroPolicyCountArray ()
  (let ((blank-array (make-array *QtyPlanCodesForPeriod* :element-type 'list :initial-element nil)))
    ;;using :initial-element to make-array wasn't giving unique conses, doing separately
    (dotimes (i *QtyPlanCodesForPeriod*)
      (setf (aref blank-array i) (cons 0 0)))
    (values blank-array)
    ))

;;; 12-Jul-03 WSD agent must be object
(defun compute-totalHierPremium (agent periodTypeKey startYear endYear &key (excludeAgents *agentCodesExcludedFromCalculations*))
  ;;if that slot already has value, then no need to recompute (might have been computed from same query on ancestor), just return for parent call
  (if (periodPremiumSubs agent)
      (periodPremiumSubs agent)
      ;;otherwise, traverse and cache before passing answer upward
    (if (member (AGD-100-AGENT agent) excludeAgents :test #'string-equal)
	;;if excluded, just set as zero
	(setf (periodPremiumSubs agent)
	  (setup-zeroPremiumArray periodTypeKey startYear endYear))
      ;;else traverse
      (flet ((sumArray2IntoArray1 (a1 a2)
	       (dotimes (year (+ (- endYear startYear) 1))
		 (case periodTypeKey
		   (:annual (incf (aref a1 year) (aref a2 year)))
		   ((:quarter :month)
		    (dotimes (qtrOrMonth (if (eq periodTypeKey :quarter) 4 12))
		      (incf (aref a1 year qtrOrMonth) (aref a2 year qtrOrMonth))))))))

	(let* ((collectionArray (setup-zeroPremiumArray periodTypeKey startYear endYear)))
	  (loop for sub in (subagents agent)
	      as subCollection = (compute-totalHierPremium sub periodTypeKey startYear endYear)
	      do
		(sumArray2IntoArray1 collectionArray subCollection))
	  ;;include this agent's direct production, if any
	  (if (periodPremium agent)
	      (sumArray2IntoArray1 collectionArray (periodPremium agent)))
	  ;;now cache in this agent's slot and thus return the value
	  (setf (periodPremiumSubs agent) collectionArray))))))

;;; 13-Jul-03 WSD similar to compute-totalHierPremium, but simpler roll-up of Policy count data
(defun compute-totalHierPolicyCountVals (agent startYear endYear &key (excludeAgents *agentCodesExcludedFromCalculations*))
  ;;if that slot already has value, then no need to recompute (might have been computed from same query on ancestor), just return for parent call
  (if (periodPolicyCountValsHier agent)
      (periodPolicyCountValsHier agent)
      ;;otherwise, traverse and cache before passing answer upward
    (if (member (AGD-100-AGENT agent) excludeAgents :test #'string-equal)
	;;if excluded, just set as zero
	(setf (periodPolicyCountValsHier agent)
	  (setup-zeroPolicyCountArray))
      ;;else traverse
      (flet ((sumPolicyCntArray2IntoArray1 (a1 a2)
	       ;;these are arrays of dotted pairs
	       (dotimes (i *QtyPlanCodesForPeriod*)
		 (incf (car (aref a1 i)) (car (aref a2 i)))
		 (incf (cdr (aref a1 i)) (cdr (aref a2 i))))))

	(let* ((collectionArray (setup-zeroPolicyCountArray)))
	  (loop for sub in (subagents agent)
	      as subCollection = (compute-totalHierPolicyCountVals sub startYear endYear)
	      do
		(sumPolicyCntArray2IntoArray1 collectionArray subCollection))
	  ;;include this agent's direct production, if any
	  (if (periodPolicyCountVals agent)
	      (sumPolicyCntArray2IntoArray1 collectionArray (periodPolicyCountVals agent)))
	  ;;now cache in this agent's slot and thus return the value
	  (setf (periodPolicyCountValsHier agent) collectionArray))))))


;;; 12-Jul-03 WSD added
(defun compute-allPremiumHierarchies (periodTypeKey startYear endYear)
  (map-agents #'(lambda (a) (compute-totalHierPremium a periodTypeKey startYear endYear))))


;;; 12-Jul-05 WSD added
;;; 15-Aug-10 WSD added include-splits? to possibly count all premium to writing agent
(defun compute-premiumsForPeriod (periodTypeKey startYear endYear
				  &key (only-paid-policies? nil) (include-splits? t))
  (compute-individualAgentPremiumsForPeriod periodTypeKey startYear endYear
					    :only-paid-policies? only-paid-policies?
					    :include-splits? include-splits?)
  (compute-allPremiumHierarchies periodTypeKey startYear endYear)
  )

;;; 13-Jul-05 WSD added (yes, ironically, one year to the day as the premium version)
(defun compute-policyCountsForPeriod (startYear endYear &optional (startMonth 1) (endMonth 12) (only-paid-policies? nil))
  (compute-individualAgentPolicyCountValsForPeriod startYear endYear startMonth endMonth only-paid-policies?)
  (map-agents #'(lambda (a) (compute-totalHierPolicyCountVals a startYear endYear)))
  )

;;; 12-Jul-06 WSD added
(defun total-periodPremiumSubsInclAliases (agentObj periodKey)
  (let* ((arrayDims (array-dimensions (periodPremiumSubs agentObj)))
	 (numPeriods (case periodKey
		       (:annual (first arrayDims))
		       (t (second arrayDims))))
	 (aliases (or (get-aliases agentObj)
		      (list (AGD-100-AGENT agentObj))))) ;wasteful since most are not aliases, but oh well
    (loop for aCode in aliases
	    as agent = (getAgent aCode)
	summing
	  (loop for i from 0 to (- numPeriods 1)
	      summing
		(case periodKey
		  (:annual (aref (periodPremiumSubs agent) i))
		  (t (aref (periodPremiumSubs agent) 0 i)))) ;assuming a single year
	       )))

;;; 16-Aug-19 WSD
(defun total-periodPremiumDirect (agentObj periodKey)
  (let* ((directPremiums (periodPremium agentObj))
	 (arrayDims (if directPremiums
			(array-dimensions (periodPremium agentObj))))
	 (numPeriods (if directPremiums
			 (case periodKey
			   (:annual (first arrayDims))
			   (t (second arrayDims))))))
    (if directPremiums (loop for i from 0 to (- numPeriods 1)
			  summing
			    (case periodKey
			      (:annual (aref (periodPremium agentObj) i))
			      (t (aref (periodPremium agentObj) 0 i))))
	0)))

;;; 16-Aug-19 WSD added
(defun total-periodPremiumDirectInclAliases (agentObj periodKey)
    (+ (total-periodPremiumDirect agentObj periodKey)
       (loop for aCode in (get-aliases agentObj)
	    as agent = (getAgent aCode)
	summing
	  (total-periodPremiumDirect agent periodKey))))


;;; 13-Jul-17 WSD added
;;; 15-Aug-10 WSD don't include splits when counting last date of policy (i.e., only direct writing counts)
(defun date-ofLatestPolicy (agent)
  (let ((agent-policies (get-policies :agentCode (AGD-100-AGENT agent) :includeSplits? nil)))
    (if agent-policies
	(apply #'max (mapcar #'EX-ISSUE-DATE agent-policies)))))

;;; 12-May-15 WSD diagnostic
;;; 13-Mar-11 WSD added includeSplits?
(defun lp (agentcode  &key year planCodes (display-p nil) (summary-p t) (suppressZeroPolicies nil) (includeSplits? t) (by-bridge-date? nil))
  "diagnostic to '(L)ist (P)olicies' of an agent given the code"
  (setf agentcode (string-upcase agentcode))
  (let ((agent-policies (get-policies :agentCode agentcode :startYr year :endYr year
				      :planCodes planCodes :includeSplits? includeSplits?))
	(agent (gethash agentcode *Agents*)))
    (unless (and suppressZeroPolicies
		 (not agent-policies))
      (format t "~%~s policies (from ~a to ~a) for ~a / ~a [~a,~a] / contract: ~a"
	      (length agent-policies)
	      (if agent-policies
		  (datestring (apply #'min (mapcar #'EX-ISSUE-DATE agent-policies)))
		"none")
	      (if agent-policies
		  (datestring (apply #'max (mapcar #'EX-ISSUE-DATE agent-policies)))
		"none")
	      agentcode
	      (agent-nameAndCompany agent)
	      (AGD-100-BUS-CITY agent)
	      (AGD-100-BUS-TERR-STATE agent)
	      (or (contractCode agent) "-unknown to IMA-")
	      ))

    (when (and agent-policies summary-p) ;display summary of policy types
      ;;use a hash-table of cons pairs of (count.LAP)
      (let ((planTypeCounts (make-hash-table :test #'equal)))
	(dolist (p agent-policies)
	  (if (gethash (EX-PLAN-AND-OPT p) planTypeCounts)
	      (progn
		(incf (car (gethash (EX-PLAN-AND-OPT p) planTypeCounts)))
		(incf (cdr (gethash (EX-PLAN-AND-OPT p) planTypeCounts)) (agentPremium agentcode p)))
	    (setf (gethash (EX-PLAN-AND-OPT p) planTypeCounts) (cons 1 (agentPremium agentcode p)))))
	(maphash #'(lambda (planName count.LAP)
		     (format t "~%   ~a: ~a ($~:D)" planName (car count.LAP) (round (cdr count.LAP))))
		 planTypeCounts)))

    ;; display first 50
    (when display-p
      (loop with count = 0
	for p in agent-policies
	while (<= (incf count) 50) do
	  (format t "~%~a ~a"
		  (datestring (if by-bridge-date?
				  (bridge-date p)
				  (EX-ISSUE-DATE p)))
		  (EX-POLICY p))))))
