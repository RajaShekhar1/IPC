;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: USER -*-
;;;
;;; ===========================================================
;;; Filename: parseSalesData.cl
;;;
;;; Copyright 2012 Thumbprint Software, LLC
;;; Copyright 2014-17 5Star Life Insurance Company
;;; All Rights Reserved.
;;; ===========================================================

(in-package :COMMON-LISP-USER)

(defun join (str1 str2) (concatenate 'string str1 str2)) 

(defvar documentPath "/home/swankdave/Projects/fivestar/DellDataImport/")
(defvar inputDocumentPath (join documentPath "input/"))
(defvar outputDocumentPath (join documentPath "output/"))



(LOAD (join documentPath "ParseUtilities.cl"))
(LOAD (join documentPath "Employer.cl"))
(LOAD (join documentPath "Policy.cl"))
(LOAD (join documentPath "Payroll.cl"))
(LOAD (join documentPath "Insured.cl"))
(LOAD (join documentPath "Payroll.cl"))
(LOAD (join documentPath "NewBizRec.cl"))
(LOAD (join documentPath "Hierachy.cl"))
(LOAD (join documentPath "Agent.cl"))
(LOAD (join documentPath "SupBenifit.cl"))
(LOAD (join documentPath "Split.cl"))
(LOAD (join documentPath "Fields.cl"))

;;; 12-May-02 WSD just a util routine to generate the slot definitions to use in classes below (typically only using once to define them)
(defun genSlotListAdHoc (slotLabelList)
  (let ((slotDeflist '()))
    (loop for slotSymbol in slotLabelList
	as slotLabel = (string-upcase (princ slotSymbol))
	do
	  (push (format nil "(~a~% :accessor ~a~% :initform nil~% :initarg :~a)~% "
			slotLabel slotLabel slotLabel)
		slotDeflist))
    (reduce #'(lambda (x y) (concatenate 'string x y)) (nreverse slotDeflist))))

;;; 12-Jun-25 WSD added shortcuts for a few common slots
(defmethod aname ((agentcode string))
  (let ((agent (getAgent agentcode)))
    (if agent 
	(AGD-100-NAME agent)
      (format nil "agent ~a not found" agentcode))))

(defmethod aname ((agent AGENT))
  (AGD-100-NAME agent))

;;; 14-Feb-20 WSD added
(defmethod situsState ((employer EMPLOYER))
  (if (or (not (DEM-ALT-STATE employer)) (string-equal "" (DEM-ALT-STATE employer)))
      (DEM-STATE employer)
    (DEM-ALT-STATE employer)))






;;;-----------------------------------------------------------------------
;;; Lookup, retrieval
;;;

(defmacro tagHier-contractCode (agentTuple)
  `(string-trim " " (nth 10 ,agentTuple)))
;;; 12-Jun-21 WSD added to distinguish from full AgentCode text

;;;-----------------------------------------------------------------------
;;; Parsing, Importing
;;;


;;; 12-May-10 WSD added
;;; 12-Jul-05 WSD added hier 26BJD --> 26AKL
;;; 13-Jul-02 WSD changed label of John Bascom to Atlantic Benefits
;;; 13-Jul-02 WSD added new exceptions to hiearchy
;;; 13-Jul-12 WSD further exceptiosn to hierarchy to clean-up inclusion of Atlantic Benefits; update orphans
;;; 13-Jul-12 WSD alter the agent name of each re-linked sub with a **
;;; 13-Aug-12 WSD final step:  connect all 'orphans' to home office
;;; 13-Sep-11 WSD changed updated per directions form George Conmy
;;; 13-Oct-03 WSD fixed typos
;;; 14-Feb-04 WSD updated hierarchy exceptions for John Bascom
;;; 14-Mar-03 WSD initially tweak agent codes but leave structure in place (all RMDs  point to home office)
;;; 14-Mar-31 WSD added slew of temporary exceptions while Home Office makes agent hierarchy edits
(defun setup-exceptionsToParsing ()
  (flet ((link (sub super &optional (is-orphan? nil))
		(let ((oldParent (parent sub))
		      (prefix (if is-orphan? "**" "^^")))
		  (unless (eq oldParent super) ;ignore if already linked
		    (if oldParent
			(setf (subagents oldParent)
			      (remove sub (subagents oldParent)))) 
		    (setf  (parent sub) super)
		    (pushnew sub (subagents super))
		    (setf (AGD-100-NAME sub)
			  (format nil "~a~a" prefix (AGD-100-NAME sub))))))
	  (linkNoTrace (sub super)
		       (let ((oldParent (parent sub)))
			 (unless (eq oldParent super) ;ignore if already linked
			   (if oldParent
			       (setf (subagents oldParent)
				     (remove sub (subagents oldParent)))) 
			   (setf  (parent sub) super)
			   (pushnew sub (subagents super)))))
	  
	  (promoteSubs (inertAgent)
		       ;;reroute all of inertAgent's subs to inertAgent's parent
		       (let ((parent (parent inertAgent)))
			 (dolist (sub (copy-list (subagents inertAgent)))
			   (setf  (parent sub) parent)
			   (pushnew sub (subagents parent)))
			 (setf (subagents inertAgent) nil))
		       ))
      
      
    (let ((homeOfficeRec (getAgent "26Z1A"))	;changed all imaRec to homeOfficeRec (Home Office Rec) - was CCO26
	  (oldRec (getAgent "O26Z1"))
	  (oldRecZ4 (getAgent "A26Z4"))
	  (marcRec (getAgent "O26Z4"))
	  (judyRec (getAgent "O26Z5"))
	  (johnRec (getAgent "O26Z6"))
	  ;;(atlanticRec (getAgent "26BMW"))
	  
	  (southCentRec (getAgent "26Z4A")) ;South Central = Hardage
	  (westernRec (getAgent "26Z5A")) ;Western = Sloan
	  (gLakesRec (getAgent "26Z6A")) ;Great Lakes = Buchholz
	  
	  )
      ;;set RMDs
      (setf (AGD-100-NAME oldRec) "(legacy) former IMA RMD(Z1)"
	    (AGD-100-NAME oldRecZ4) "(legacy) Marc Hardage advance"
	    (AGD-100-NAME marcRec) "(legacy) Marc Hardage"
	    (AGD-100-NAME johnRec) "(legacy) Southeast RMD"
	    (AGD-100-NAME judyRec) "(legacy) Western RMD")   ;was Judy Heiserman
      
      (link southCentRec homeOfficeRec)
      (link westernRec homeOfficeRec)
      (link gLakesRec homeOfficeRec)
      (link (getAgent "O26ZA")	(getAgent "CO26Z"))

      ;;2014-07-07 hack to get Barnes-Osburn to combine properly with other "force linked" 26AFF
      (setf (AGD-100-NAME (getAgent "26AHV"))
		 (format nil "~a~a" "^^" (AGD-100-NAME (getAgent "26AHV"))))      
      
      (link (getAgent "O26Y6")	(getAgent "26Z1A"))
      (link (getAgent "26AEU")	(getAgent "26Z1A"))
      
      ;;2015-11-04 Whitten agents out of place
      (link (getAgent "26BXZ")	(getAgent "26BNY"))
      (link (getAgent "26BRB")	(getAgent "26BNY"))

      ;;2015-12-15 Ronnie Hohenberger to be reported under AOBG, not separately
      (link (getAgent "26CEC")	(getAgent "26CDH"))

      ;;2016-06-14 Starnes Osburn, etc. under "Barnes Osburn
      (link (getAgent "26CCU")	(getAgent "26AHV"))
      (link (getAgent "26ATW")	(getAgent "26AHV"))
      (link (getAgent "26BNP")	(getAgent "26AHV"))
      
            
      ;;finally, find any remaining 'orphans' and connect under HOME OFFICE homeOfficeRec
      (map-agents 
       #'(lambda (agent)
	   (unless (or (parent agent)
		       (eq agent homeOfficeRec))
	     (link agent homeOfficeRec))))

      ;; 16-Feb-19 rewire ALL RMDs agents to be under Home Office for current reporting purposes
      (loop for RMD in (list southCentRec westernRec gLakesRec) do
	   (loop for sub in (subagents RMD) do
		(linkNoTrace sub homeOfficeRec)))


      ;; 16-Jun-14 rewire select agents to 'disenfranchise' inert-top-level agents so their downlines report a level higher
      (promoteSubs (getAgent "CCO26"))  ;;Chrisman
      (promoteSubs (getAgent "26BNR"))  ;;Bascom
      (promoteSubs (getAgent "O26Z4"))  ;;Legacy Hardage
      
      
      )))







;;; 12-May-10 WSD was trying to find referral via straight code, then contact name, then company name; but for now 
;;;               I'm preprocessing the input data to have the hierarchy as-is, so changed to a simple lookup
(defun find-uniqueParent (hierTuple) 
  (let ((referralToken (agentHier-referral hierTuple)))
    (getAgent (subseq referralToken (- (length referralToken) 5)))))

(defun dateWindows2OverlapsWindow1? (start1 end1 start2 end2)
  (or (and (<= start2 start1)		;start1 inside start2--end2
	   (>= end2 start1))
      (and (<= start2 end1)		;end1 inside start2--end2
	   (>= end2 end1))
      (and (<= start2 start1)		;Window2 envelops Window1
	   (>= end2 end1))))


;;; 12-May-15 WSD added as generic workhorse on Policies
;;; 12-Jun-29 WSD added status qualifiers
;;; 13-Mar-11 WSD added includeSplits to find all policies that mention agentCode or one of agentCodes if those are provided
;;; 13-Nov-14 WSD added option for by-recv-date?
;;; 13-Dec-27 WSD added logic to exclude policies with null 'receive-date'
;;; 15-Mar-10 WSD added only-paid-policies?
;;; 15-Jun-12 WSD added in-force-year, in-force-month and associated filter logic
;;; 15-Jun-12 WSD added planCode-exact-matches?
;;;  16-Aug-10 WSD added include-riders?
(defun map-policies (mapFn &key startYr endYr startMo endMo agentCode agentCodes includeSplits 
			     planCodes (planCode-exact-matches? nil)
			     insured employerID status notstatuslist
			     in-force-year in-force-month
			     (by-recv-date? nil) (by-bridge-date? nil) (only-paid-policies? nil)
			     (include-riders? nil))
  ;;map a given function across all policies depending on any constraints
  (flet ((include-policy-p (policyObj)
	   (let* ((one-second-prior-to-start (if startYr 
						 (- (if startMo
						     (encode-universal-time 0 0 0 1 startMo startYr)
						     (encode-universal-time 0 0 0 1 1 startYr)) 
						    1)))
		  (one-second-after-finish (if endYr 
					       (if endMo
						   (encode-universal-time 0 0 0 1 
									  (if (= 12 endMo) 1 (+ endMo 1))
									  (if (= 12 endMo) (+ endYr 1) endYr))
						 (encode-universal-time 0 0 0 1 (or startMo 1) (+ endYr 1)))))
		  (last-second-of-inForce (if in-force-year
					      ;;want tail-end of InForce year or, if month, just prior to next month
					      (- (if in-force-month
						     (encode-universal-time 0 0 0 1 
									    (if (= 12 in-force-month)
										1
										(+ 1 in-force-month))
									    
									    (if (= 12 in-force-month)
										(+ 1 in-force-year)
										in-force-year))
						     (encode-universal-time 0 0 0 1 1 (+ 1 in-force-year)))
						 1)))
		  (policy-agent1 (EX-WRITING-AGENT1 policyObj))
		  (policy-date (if by-recv-date?
				   (received-date policyObj)
				   (if by-bridge-date?
				       (bridge-date policyObj)
				       ;;default is retrieve by Issue Date
				       (EX-ISSUE-DATE policyObj))))
		  (planCode (string-upcase (EX-PLAN-AND-OPT policyObj)))
		  (planCodeFragments (mapcar #'string-upcase planCodes))
		  (policyStatusCode (EX-STATUS policyObj))
		  (policyStatus (cond
				  ((string-equal "Z" policyStatusCode) :declined)
				  ((string-equal "C" policyStatusCode) :cancelled)
				  ((string-equal "L" policyStatusCode) :lapsed)
				  ((string-equal "D" policyStatusCode) :deceased)
				  ((string-equal "E" policyStatusCode) :expired)
				  ((string-equal "M" policyStatusCode) :matured)
				  ((or (null policyStatusCode)
				       (string-equal "" policyStatusCode)) :active)
				  (t :other)))
		  )
	     (and (or (not startYr) (and policy-date (> policy-date one-second-prior-to-start))) ;null date skips policy
		  (or (not endYr) (and policy-date (< policy-date one-second-after-finish)))
		  (or (not only-paid-policies?) (and policy-date (< policy-date (EX-DUE-DATE policyObj))))
		  (or (not in-force-year)
		      (and (<= policy-date last-second-of-inForce)
			   (not (eq policyStatus :declined))
			   (or (eq policyStatus :active) ;;either still active or changed after inForce date
			       (> (EX-STATUS-DATE policyObj) last-second-of-inForce))))

		  (or (not agentCode) 
		      (if includeSplits
			  (find agentCode (splits policyObj) :key #'car :test #'string-equal) 
			(string-equal agentCode policy-agent1)))
		  (or (not agentCodes)
		      (if includeSplits
			  (let ((foundOne? nil))
			    (dolist (aCode agentCodes)
			      (setf foundOne? (or foundOne?
						  (find aCode (splits policyObj) :key #'car :test #'string-equal))))
			    (values foundOne?))
			(member policy-agent1 agentCodes :test #'string-equal)))
		  (or (not planCodes)
		      ;;match if any of the strings in planCodes are substrings of the actual planCode
		      ;;unless exact match required
		      (find-if #'(lambda (planCodeFragment)
				   (if planCode-exact-matches? 
				       (string-equal planCodeFragment planCode)
				       (search planCodeFragment planCode :test #'string-equal))) 
			       planCodeFragments))
		  (or (not insured)
		      (SEARCH insured (EX-INS-NAME policyObj) :TEST #'CHAR-EQUAL)
		      )
		  (or (not employerID)
		      (equal employerID (employerID policyObj))
		      )
		  (or (not status)
		      ;;ONLY policies of this status
		      (eq status policyStatus))
		  (or (not notstatuslist)
		      
;;;		      (and (format t "~%..[~a] compare ~s to ~s  --> ~s" (EX-POLICY policyObj)
;;;				   policyStatus notstatuslist
;;;				   (not (member policyStatus notstatuslist)))
;;;			   nil)
		      
		      ;;as long as NOT a member of the no-no list status types
		      (not (member policyStatus notstatuslist)))

		  (or include-riders? (not (isRider? policyObj))
		      ;;if filtering out riders, consider whether to include riders who's base policy is outside this range
		      (let ((basePol (base-policy policyObj)))
			(or (null basePol)
			    (null (EX-ISSUE-DATE basePol))
			    ;;OK, I have a base pol, so see if it is outside a date range, if given
			    (not (and startYr (> (EX-ISSUE-DATE basePol) one-second-prior-to-start)))
			    (not (and endYr (< (EX-ISSUE-DATE basePol) one-second-after-finish))))))
		  ))))
    (maphash #'(lambda (key policy)
		 (declare (ignore key))
		 (when (include-policy-p policy)
		   (funcall mapFn policy)))
	     *Policies*)))


;;; 12-May-15 WSD generic policy retrieval
;;; 12-Jun-29 WSD added status and excluding "declined" polices by default
;;; 13-Mar-11 WSD added includeSplits?
(defun get-policies (&key startYr endYr startMo endMo agentCode agentCodes includeSplits?
			  planCodes insured status (notstatuslist '(:declined)))
  (let ((collected-policies nil))
    (map-policies #'(lambda (policy)
		      (push policy collected-policies))
		  :startYr startYr
		  :startMo startMo
		  :endYr endYr
		  :endMo endMo
		  :agentCode agentCode
		  :agentCodes agentCodes
		  :includeSplits includeSplits?
		  :planCodes planCodes
		  :insured insured
		  :status status		  
		  :notstatuslist notstatuslist
		  )
    (sort collected-policies #'< :key #'EX-ISSUE-DATE)))





(defun check-hierMisMatch ()
  (map-agents #'(lambda (agent)
		  (loop for ancestors on (hierarchy agent)
		      when (and (cdr ancestors)
				(not (equal (cdr ancestors) 
					    (hierarchy (car ancestors))))) 
		      do (format t "~%~a[~a] has hier " (AGD-100-NAME agent) (AGD-100-AGENT agent))
			 (print-listOfAgents (hierarchy agent) :name-p t)
			 (format t "; but ~a" (AGD-100-NAME (car ancestors)))
			 (if (hierarchy (car ancestors))
			     (progn
			       (format t "s hier is ~a" (AGD-100-NAME (car ancestors)))
			       (print-listOfAgents (hierarchy (car ancestors)) :name-p t))
			   (format t "has no hierarchy."))))
	      ))
	
;;; 12-Jun-23 WSD changed to (parent agent) from looking at (car (hierarchy agent))
(defun show-hier (agentcode)
  (let ((agent (getAgent agentcode)))
    (format t "~%~a[~a]" (AGD-100-NAME agent) (AGD-100-AGENT agent))
    (loop as nextUp = (parent agent)
	until (null nextUp)
	do 
	  (format t " -> ~a[~a]"  (AGD-100-NAME nextUp) (AGD-100-AGENT nextUp))
	  (setf agent nextUp))
    
    ))


;;; 12-Jun-19 WSD added
;;; 12-Jun-23 WSD changed to (parent agent) from looking at (car (hierarchy agent))
;;; 14-Mar-03 WSD changed imaRec to homeOfficeRec
(defun orphaned-hier? (agentcode &key year planCodes)
  (let* ((agent (getAgent agentcode))
	 (homeOfficeRec (getAgent "26Z1A"))
	 (IMAfoundp (eq agent homeOfficeRec)))
    (loop as nextUp = (parent agent)
	until (or IMAfoundp (null nextUp))
	do (setf IMAfoundp (equal nextUp homeOfficeRec)) 
	   (setf agent nextUp))
    (unless IMAfoundp
      (lp agentcode :year year :planCodes planCodes :suppressZeroPolicies t))))



;;; 12-Jun-21 WSD added
;;; 14-Mar-03 WSD changed imaRec to homeOfficeRec
(defun dump-producingOrphanedHier (&optional (dumpPath (join outputDocumentPath "nonIMA-hierarchy.csv")))
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format dumpfile "agent number,contract,alt ID,hire date,name (city st),history")
    (format dumpfile "~%,policy #,plan,issue date~%")
    (let ((homeOfficeRec (getAgent "26Z1A")))	
      (map-agents #'(lambda (agent)
		      (unless (or (is-subagent-of? agent homeOfficeRec) (eq agent homeOfficeRec))
			(let* ((recentPolicies (reverse (get-policies :agentCode (AGD-100-AGENT agent))))
			       (policyCount (length recentPolicies)))
			  (unless (zerop policyCount)
			    (format dumpfile "~%~a,~a,~a,~a,~s (~a ~a),~s policies from ~a to ~a"
				    (AGD-100-AGENT agent)
				    (or (contractCode agent) "")
				    (AGD-100-ALTERNATE-ID agent)
				    (datestring (AGD-100-DATE-HIRED agent))
				    (agent-nameLastFirst agent)
				    (or (AGD-100-BUS-CITY agent) "")
				    (or (AGD-100-BUS-TERR-STATE agent) "")
				    policyCount
				    (datestring (apply #'min (mapcar #'EX-ISSUE-DATE recentPolicies)))
				    (datestring (apply #'max (mapcar #'EX-ISSUE-DATE recentPolicies)))
				    )
			    (dolist (p (subseq recentPolicies 0 (min 10 policyCount)))
			      (format dumpfile "~%,~a,~a,~a"
				      (EX-POLICY p)
				      (EX-PLAN-AND-OPT p)
				      (datestring (EX-ISSUE-DATE p))))
			    ))))))))



;;;-----------------------------------------------------------------------
;;; Dump Output to CSV files for direct read into reporting Spreadsheet
;;;

;;; 14-Mar-03 WSD now using HomeOffice, but *IMA* is prevalent throughout code, so keeping the variable name, for now, changing to new agent code for H.O.
;;; 14-Mar-03 WSD also changing to use new other Region labels
;;; 14-Mar-04 WSD replaced older RMDs of *Atlantic*, *Judy*, *Marc1*, etc. with new region names
;;; 14-Sep-12 WSD globally replaced *IMA* with *HomeOffice*
(defun prep-mainAgentObjects ()
  (setf *CentralRegion* (getAgent "26Z4A")
	*WestRegion* (getAgent "26Z5A")
	*GLakesRegion* (getAgent "26Z6A")
	*HomeOffice* (getAgent "26Z1A")))
  

(defun quickRMDdump (&optional (dumpPath (join outputDocumentPath "quick2012.csv")))
  (prep-mainAgentObjects)
  (compute-premiumsForPeriod :month 2012 2012)
  
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format dumpfile "Name, Agent, Contract Code,")
    
    (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*)
	do 
	  (format dumpfile "~%,~a,~a,~a" (AGD-100-NAME rmd) (AGD-100-AGENT rmd) (or (contractCode rmd) ""))
	  (dotimes (i 12)
	    (format dumpfile ",~D"  (round (aref (periodPremiumSubs rmd) 0 i)))
	    ))))


;;; 12-Jul-05 WSD added
;;; 13-Jul-02 WSD change label "IMA Other" to "Home Office" and eliminated *Judy* as a separate RMD
;;; 14-Mar-04 WSD changed region variables
;;; 15-Aug-10 WSD excluding splits and crediting 100% to writing agent
(defun dump-YoYAnnualSummary (startYear endYear &optional (dumpPath (join outputDocumentPath "annualSummary.csv")))
  (prep-mainAgentObjects)
  (compute-premiumsForPeriod :annual  startYear endYear :include-splits? nil)
  (let ((numYears (+ (- endYear startYear) 1)))
    (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
      ;;header
      (format dumpfile "Name,code")
      (dotimes (yrOffset numYears)
	(format dumpfile ",~s" (+ startYear yrOffset)))
    
      (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion*)
	  as rmdCode = (AGD-100-AGENT rmd)
	  as aliases = (get-aliases rmdCode)
	  do 
	    (format dumpfile "~%~a,~a" (AGD-100-NAME rmd) rmdCode)
	    (dotimes (i numYears) 
	      (format dumpfile ",~D"  
		      (round 
		       (if aliases
			  ;;sum across aliases
			  (loop for agentCode in aliases
			      as thisAgent = (getAgent agentCode)
			      summing (aref (periodPremiumSubs thisAgent) i))
			;;else, just this RMD
			(aref (periodPremiumSubs rmd) i))))))

      ;;finally, show everything else in one figure of IMA - RMD's
      (format dumpfile "~%Home Office,N/A")
      (dotimes (i numYears) 
	(format dumpfile ",~D"  
		(round (- (aref (periodPremiumSubs *HomeOffice*) i)
			  (aref (periodPremiumSubs *CentralRegion*) i)
			  (aref (periodPremiumSubs *WestRegion*) i)
			  (aref (periodPremiumSubs *GLakesRegion*) i)
			  ))))
      (format dumpfile "~%,Total")
      (dotimes (i numYears) 
	(format dumpfile ",~D"  
		(round (aref (periodPremiumSubs *HomeOffice*) i))))
      )))


;;; 13-Jan-17 WSD added dumpYr since was computing current (new) year when we really wanted prior year, caused blow up
;;; 13-Jul-02 WSD change label "IMA Other" to "Home Office", and removed *Judy* as a separate RMD
;;; 15-Aug-10 WSD not using splits - credit 100% to writing agents in compute-premiumsForPeriod
(defun dump-monthsYTDSummary (&optional (dumpPath (join outputDocumentPath "annualSummary.csv")) dumpYr
				(only-paid-policies? nil))
  (prep-mainAgentObjects)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hr day mo))
    (if dumpYr (setf Yr dumpYr))	;user dumpYr if we have it
    (compute-premiumsForPeriod :month  yr yr :only-paid-policies? only-paid-policies? :include-splits? nil))
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :append :if-does-not-exist :create)
    (dotimes (i 3) (format dumpfile "~%"))
    ;;header
    (format dumpfile "Name,code")
    (dolist (mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
      (format dumpfile ",~a" mo))
    
    (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion*)
	as rmdCode = (AGD-100-AGENT rmd)
	as aliases = (get-aliases rmdCode)
	do 
	  (format dumpfile "~%~a,~a" (AGD-100-NAME rmd) rmdCode)
	  (dotimes (i 12) 
	    (format dumpfile ",~D"  
		    (round 
		     (if aliases
			 ;;sum across aliases
			 (loop for agentCode in aliases
			     as thisAgent = (getAgent agentCode)
			     summing (aref (periodPremiumSubs thisAgent) 0 i))
		       ;;else, just this RMD
		       (aref (periodPremiumSubs rmd) 0 i))))))

    ;;finally, show everything else in one figure of IMA - RMD's
    (format dumpfile "~%Home Office, N/A")
    (dotimes (i 12) 
      (format dumpfile ",~D"  
	      (round (- (aref (periodPremiumSubs *HomeOffice*) 0 i)
			(aref (periodPremiumSubs *CentralRegion*) 0 i)
			(aref (periodPremiumSubs *WestRegion*) 0 i)
			(aref (periodPremiumSubs *GLakesRegion*) 0 i)
			))))
    (format dumpfile "~%,Total")
    (dotimes (i 12) 
      (format dumpfile ",~D"  
	      (round (aref (periodPremiumSubs *HomeOffice*) 0 i))))
    ))





;;; 12-Jul-06 WSD pretty inefficient in dealing with Aliases
;;; 13-Feb-15 WSD checked for NIL on (periodPremium *HomeOffice*) since in early 2013 this was the case
;;; 13-Jul-02 WSD change label "IMA Other" to "Home Office", and removed *Judy* as a separate RMD
;;; 14-Mar-31 WSD printing "home office" label is no longer a special case - print just like other RMDs
(defun dump-monthlySubHierarchy (agent level dumpfile)
  (unless (zerop (total-periodPremiumSubsInclAliases agent :month))
    (let* ((IMA-special-case? (eq agent *HomeOffice*))
	   (agentCode (AGD-100-AGENT agent))
	   (aliases (or (get-aliases agentCode)
			(list agentCode)))
	   (aggregateCode "")
	   (aggregateContract "")
	   (thisPremiumOnly (make-array 12 :initial-element 0))
	   (subTotal (make-array 12 :initial-element 0)))
    
      ;;setup totals & display labels
      (unless IMA-special-case?
	(loop for aCode in aliases
	    for iteration from 1 to 1000 ;just need a counter for first time
	    as aliasAgent = (getAgent aCode)
	    as directPeriodPremium = (periodPremium aliasAgent)
	    do
	      (setf aggregateCode (concatenate 'string aggregateCode 
					       (if (> iteration 1) "/" "")
					       aCode))
	      (setf aggregateContract (concatenate 'string aggregateContract 
						   (if (> iteration 1) "/" "")
						   (contractCode aliasAgent)))
	      (dotimes (i 12)
		(if directPeriodPremium
		    (incf (aref thisPremiumOnly i) (aref directPeriodPremium 0 i)))
		(incf (aref subTotal i) (aref (periodPremiumSubs aliasAgent) 0 i)))))
    
      ;;dump this agent's line
      (format dumpfile "~%")
      (dotimes (i level) (princ #\TAB dumpfile))
      ;; (if IMA-special-case?
      ;; 	  (format dumpfile "Home Office")
      ;; 	(format dumpfile "~a [~a : ~a]" 
      ;; 	      	(agent-nameLastFirst agent)
      ;; 		aggregateCode 
      ;; 		aggregateContract))
      (format dumpfile "~a [~a : ~a]" 
	      	(agent-nameLastFirst agent)
		aggregateCode 
		aggregateContract)
      (dotimes (i (- 6 level)) (princ #\TAB dumpfile))
      (format dumpfile "~a~D"		;totals column
	      #\TAB
	      (round
	       (if IMA-special-case?
		   (- (loop for i from 0 to 11
			  summing (aref (periodPremiumSubs *HomeOffice*) 0 i)) 
		      (total-periodPremiumSubsInclAliases *CentralRegion* :month)
		      (total-periodPremiumSubsInclAliases *WestRegion* :month)
		      (total-periodPremiumSubsInclAliases *GLakesRegion* :month)
		      )
		 (total-periodPremiumSubsInclAliases agent :month))))
      (dotimes (i 12) 
	(format dumpfile "~a~D~a~D"  
		#\TAB (round 
		       (if IMA-special-case?
			   (if (periodPremium *HomeOffice*)
			       (aref (periodPremium *HomeOffice*) 0 i)
			     0) 
			 (aref thisPremiumOnly i)))
		#\TAB (round 
		       (if IMA-special-case?
			   (- (aref (periodPremiumSubs *HomeOffice*) 0 i)
			      (aref (periodPremiumSubs *CentralRegion*) 0 i)
			      (aref (periodPremiumSubs *WestRegion*) 0 i)
			      (aref (periodPremiumSubs *GLakesRegion*) 0 i)
			      ) 
			 (aref subTotal i)))))
      
      ;;dump all subs in decending order of their total hier premium
      ;; collect all subs, remove any "aliases", and sort
      (let ((allSubs (if IMA-special-case?
			 (delete *CentralRegion*
				 (delete *WestRegion*
					 (remove *GLakesRegion* (subagents *HomeOffice*)))) 
		       (if (= 1 (length aliases))
			 (subagents agent)
		       (loop for alias in aliases
			   appending (subagents (getAgent alias)))))))
      
	(when allSubs
	  ;;this is tricky to eliminate any "aliases" among the subs
	  (loop with index = 0 and indexedSub
	      until (>= index (length allSubs)) do
		(setf indexedSub (nth index allSubs))
		;;(format t "~%index=~s (out of ~s allSubs)" index (length allSubs))
		(dolist (subAlias (remove (AGD-100-AGENT indexedSub)
					  (get-aliases indexedSub)
					  :test #'string-equal))
		  (setf allSubs
		    (remove subAlias allSubs
			    :test #'string-equal
			    :key #'AGD-100-AGENT
			    :start (+ index 1))))
		(incf index))
      
	  ;;now, just recurse on the subs
	  (dolist (sub (sort (copy-list allSubs)
			     #'> 
			     :key #'(lambda (a)
				      (total-periodPremiumSubsInclAliases a :month))))
	    (dump-monthlySubHierarchy sub (+ 1 level) dumpfile))))
      )))

;;; 14-Jan-31 WSD added for annual analog - too lazy to merge into one function
;;; 14-Apr-07 WSD added breakout columns for separate year totals
;;; 14-Jul-16 WSD added include-polCounts?
(defun dump-annualSubHierarchy (agent level dumpfile &optional (include-zeroTotals? nil) (include-polCounts? nil) (startYear 0) (endYear 0))
  (unless (and (not include-zeroTotals?) (zerop (total-periodPremiumSubsInclAliases agent :annual)))
    (let* ((IMA-special-case? (eq agent *HomeOffice*))
	   (agentCode (AGD-100-AGENT agent))
	   (aliases (or (get-aliases agentCode)
			(list agentCode)))
	   (aggregateCode "")
	   (aggregateContract "")
	   (arrayDims (array-dimensions (periodPremiumSubs agent)))
	   (numPeriods (first arrayDims))
	   (subTotal (make-array numPeriods :initial-element 0))
	   )
    
      ;;setup totals & display labels
      (unless IMA-special-case?
	(loop for aCode in aliases
	    for iteration from 1 to 1000 ;just need a counter for first time
	    as aliasAgent = (getAgent aCode)
	    do
	      (setf aggregateCode (concatenate 'string aggregateCode 
					       (if (> iteration 1) "/" "")
					       aCode))
	      (setf aggregateContract (concatenate 'string aggregateContract 
						   (if (> iteration 1) "/" "")
						   (contractCode aliasAgent)))
	      (dotimes (i numPeriods)
		(incf (aref subTotal i) (aref (periodPremiumSubs aliasAgent) i)))
	      ))
    
      ;;dump this agent's line
      (format dumpfile "~%")
      (dotimes (i level) (princ #\TAB dumpfile))
      (if IMA-special-case?
	  (format dumpfile "Home Office")
	(format dumpfile "~a [~a : ~a]" 
	      	(agent-nameLastFirst agent)
		aggregateCode 
		aggregateContract))
      (dotimes (i (- 6 level)) (princ #\TAB dumpfile))
      (format dumpfile "~a~D"		;totals column
	      #\TAB
	      (round
	       (if IMA-special-case?
		   (- (total-periodPremiumSubsInclAliases *HomeOffice* :annual) 
		      (total-periodPremiumSubsInclAliases *CentralRegion* :annual)
		      (total-periodPremiumSubsInclAliases *WestRegion* :annual)
		      (total-periodPremiumSubsInclAliases *GLakesRegion* :annual))
		 (total-periodPremiumSubsInclAliases agent :annual)))
	      )
      (when (> numPeriods 1)
	(loop for i from (- numPeriods 1) downto 0 do
	      (format dumpfile "~a~D"	;yearly columns
		      #\TAB
		      (round
		       (if IMA-special-case?
			   (- (aref (periodPremiumSubs *HomeOffice*) i)
			      (aref (periodPremiumSubs *CentralRegion*) i)
			      (aref (periodPremiumSubs *WestRegion*) i)
			      (aref (periodPremiumSubs *GLakesRegion*) i))
			 (aref subTotal i)
			 )))))
      (format dumpfile "~a~a~a~a~a~a"	;ciy, state, and agent code(s)
	      #\TAB
	      (or (AGD-100-BUS-CITY agent) "")				    
	      #\TAB
	      (or (AGD-100-BUS-TERR-STATE agent) "")
	      #\TAB
	      aggregateCode
	      )
      (when include-polCounts?
	(loop for dumpYear from endYear downto startYear 
	    do (format dumpfile "~a~a" 
		       #\TAB
		       (loop for aCode in aliases
			   summing (length (get-policies :agentCode aCode :startYr dumpYear :endYr dumpYear :includeSplits? nil
							 :notstatuslist '(:declined)							 
							 ))
			   into polCount
			   finally (return polCount))
		       )))
      
      
      ;;dump all subs in decending order of their total hier premium
      ;; collect all subs, remove any "aliases", and sort
      (let ((allSubs (if IMA-special-case?
			 (delete *CentralRegion*
				 (delete *WestRegion*
					 (remove *GLakesRegion* (subagents *HomeOffice*)))) 
		       (if (= 1 (length aliases))
			 (subagents agent)
		       (loop for alias in aliases
			   appending (subagents (getAgent alias)))))))
      
	(when allSubs
	  ;;this is tricky to eliminate any "aliases" among the subs
	  (loop with index = 0 and indexedSub
	      until (>= index (length allSubs)) do
		(setf indexedSub (nth index allSubs))
		;;(format t "~%index=~s (out of ~s allSubs)" index (length allSubs))
		(dolist (subAlias (remove (AGD-100-AGENT indexedSub)
					  (get-aliases indexedSub)
					  :test #'string-equal))
		  (setf allSubs
		    (remove subAlias allSubs
			    :test #'string-equal
			    :key #'AGD-100-AGENT
			    :start (+ index 1))))
		(incf index))
      
	  ;;now, just recurse on the subs
	  (dolist (sub (sort (copy-list allSubs)
			     #'> 
			     :key #'(lambda (a)
				      (total-periodPremiumSubsInclAliases a :annual))))
	    (dump-annualSubHierarchy sub (+ 1 level) dumpfile include-zeroTotals? include-polCounts? startYear endYear))))
      )))

;;; 13-Jul-05 WSD added - should've been clever to write 1 macro and vary car/cdr, but I didn't
(defun sum-PolicyTotCount (policyCountValueArray)
  (reduce #'+  policyCountValueArray
	  :key #'(lambda (qty.val) (car qty.val))))

(defun sum-PolicyTotValue (policyCountValueArray)
  (reduce #'+  policyCountValueArray
	  :key #'(lambda (qty.val) (cdr qty.val))))

(defun agentTotPolicyHierCount (agent)
  (sum-PolicyTotCount (periodPolicyCountValsHier agent)))

(defun agentTotPolicyHierValue (agent)
  (sum-PolicyTotValue (periodPolicyCountValsHier agent)))


;;; 13-Jul-05 WSD policy-count/value version of dump-monthlySubHierarchy
(defun dump-monthlyPolicySubHierarchy (agent level dumpfile)
  (unless (zerop (agentTotPolicyHierCount agent))
    (let* ((IMA-special-case? (eq agent *HomeOffice*))
	   (agentCode (AGD-100-AGENT agent))
	   (aliases (or (get-aliases agentCode)
			(list agentCode)))
	   (aggregateCode "")
	   (aggregateContract "")
	   (aggregatePolicyCountValsHier (setup-zeroPolicyCountArray))	   
	   )
    
      ;;setup totals & display labels
      (unless IMA-special-case?
	(loop for aCode in aliases
	    for iteration from 1 to 1000 ;just need a counter for first time
	    as aliasAgent = (getAgent aCode)
	    do
	      (setf aggregateCode (concatenate 'string aggregateCode 
					       (if (> iteration 1) "/" "")
					       aCode))
	      (setf aggregateContract (concatenate 'string aggregateContract 
						   (if (> iteration 1) "/" "")
						   (contractCode aliasAgent)))
	      ;;OK, just fold the alias into the 'main' total, and zero out the alias while we're at it
	      (dotimes (i *QtyPlanCodesForPeriod*)
		(incf (car (aref aggregatePolicyCountValsHier i))
		      (car (aref (periodPolicyCountValsHier aliasAgent) i)))
		(incf (cdr (aref aggregatePolicyCountValsHier i))
		      (cdr (aref (periodPolicyCountValsHier aliasAgent) i))))))
    
      ;;dump this agent's line
      (format dumpfile "~%")
      (dotimes (i level) (princ #\TAB dumpfile))
      (if IMA-special-case?
	  (format dumpfile "Home Office")
	(format dumpfile "~a [~a : ~a]" 
	      	(agent-nameLastFirst agent)
		aggregateCode 
		aggregateContract))
      (dotimes (i (- 6 level)) (princ #\TAB dumpfile))
      (format dumpfile "~a~D~a~D"	;totals columns
	      #\TAB 
	      ;;policy count
	      (if IMA-special-case?
		  (-  (agentTotPolicyHierCount *HomeOffice*)
		      (agentTotPolicyHierCount *CentralRegion*)
		      (agentTotPolicyHierCount *WestRegion*)
		      (agentTotPolicyHierCount *GLakesRegion*))
		(sum-PolicyTotCount aggregatePolicyCountValsHier))
	      #\TAB
	      ;;policy aggregate value
	      (round
	       (if IMA-special-case?
		   (-  (agentTotPolicyHierValue *HomeOffice*)
		       (agentTotPolicyHierValue *CentralRegion*)
		       (agentTotPolicyHierValue *WestRegion*)
		       (agentTotPolicyHierValue *GLakesRegion*))
		 (sum-PolicyTotValue aggregatePolicyCountValsHier))))
      
      (dotimes (i *QtyPlanCodesForPeriod*) 
	(format dumpfile "~a~D~a~D"  
		#\TAB (if IMA-special-case?
			  (- (car (aref (periodPolicyCountValsHier *HomeOffice*) i))
			     (car (aref (periodPolicyCountValsHier *CentralRegion*) i))
			     (car (aref (periodPolicyCountValsHier *WestRegion*) i))
			     (car (aref (periodPolicyCountValsHier *GLakesRegion*) i)))
			(car (aref aggregatePolicyCountValsHier i)))
		#\TAB (round 
		       (if IMA-special-case?
			   (- (cdr (aref (periodPolicyCountValsHier *HomeOffice*) i))
			      (cdr (aref (periodPolicyCountValsHier *CentralRegion*) i))
			      (cdr (aref (periodPolicyCountValsHier *WestRegion*) i))
			      (cdr (aref (periodPolicyCountValsHier *GLakesRegion*) i))) 
			 (cdr (aref aggregatePolicyCountValsHier i)))
		       )))
      
      ;;dump all subs in decending order of their total policy counts
      ;; collect all subs, remove any "aliases", and sort
      (let ((allSubs (if IMA-special-case?
			 (delete *CentralRegion*
				 (delete *WestRegion*
					 (remove *GLakesRegion* (subagents *HomeOffice*))))
		       (subagents agent))))
      
	(when allSubs
	  ;;tricky again - just like for premium version - need to avoid revisiting the aliases as subs
	  (loop with index = 0 and indexedSub
	      until (>= index (length allSubs)) do
		(setf indexedSub (nth index allSubs))
		;;(format t "~%index=~s (out of ~s allSubs)" index (length allSubs))
		(dolist (subAlias (remove (AGD-100-AGENT indexedSub)
					  (get-aliases indexedSub)
					  :test #'string-equal))
		  (setf allSubs
		    (remove subAlias allSubs
			    :test #'string-equal
			    :key #'AGD-100-AGENT
			    :start (+ index 1))))
		(incf index))
	  ;;now, just recurse on the subs in decending order
	  (dolist (sub (sort (copy-list allSubs)
			     #'> 
			     :key #'(lambda (a)
				      (agentTotPolicyHierCount a))))
	    (dump-monthlyPolicySubHierarchy sub (+ 1 level) dumpfile))))
      )))


;;; 13-Jan-17 WSD added dumpYr
;;; 13-Jul-02 WSD removed *Judy* as a member of the rmd list through which to iterate
;;; 14-Mar-04 WSD changed RMD variables
;;; 15-Aug-10 WSD crediting 100% of premium to writing agent in compute-premiumsForPeriod
(defun dump-monthsYTDHierarchy (&optional (dumpPath (join outputDocumentPath "YTDhierarchy.tsv")) dumpYr)
  ;;each Month gets 2 columns for this agent and all agents
  (prep-mainAgentObjects)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hr day mo))
    (if dumpYr (setf yr dumpYr))	;if we have dumpYr, use it
    (compute-premiumsForPeriod :month  yr yr :include-splits? nil)) 
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dotimes (i 1) (format dumpfile "~%"))
    ;;header
    (format dumpfile "Name [Code : Contract]")
    (dotimes (i 7) (princ #\TAB dumpfile))
    (dolist (mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
      (format dumpfile "~a~a~a" #\TAB mo #\TAB))
    
    (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*)
	do
	  (dump-monthlySubHierarchy rmd 0 dumpfile))))




;;; 13-Jul-05 WSD similar to dump-monthsYTDHierarchy, but an agent-hierarchy of policy counts & values per planCode
(defun dump-YTDPolicyHierarchy (dumpPath)
  ;;each plan type gets 2 columns for this agent listing policy count and total value, respectively
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dotimes (i 1) (format dumpfile "~%"))
    ;;header
    (format dumpfile "Name [Code : Contract]")
    (dotimes (i 8) (princ #\TAB dumpfile)) ;;spacing for agent hiarchy columns and TWO totals column
    (dolist (pCodePair *PlanCodeIndex*)
      (format dumpfile "~a~a~a" #\TAB (car pCodePair) #\TAB))
    
    (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*)
	do
	  (dump-monthlyPolicySubHierarchy rmd 0 dumpfile))))



;;; 12-Jul-13 WSD added
;;; 13-Jan-17 WSD added dumpYr since running the data on prior year in new year blew up
;;; 16-Feb-02 WSD added TopAgent column, and subQuarters as list of any of '(1 2 3 4) to sum partial years
(defun dump-YTDAgentRanking (startYr endYr 
			     &optional (startMo 1) (endMo 12) 
					(dumpPath (join outputDocumentPath "YTDAgentRanking")))
  (prep-mainAgentObjects)
  (setf dumpPath
	(format nil "~a_~a~a-~a~a.tsv"
		dumpPath startYr (if (and (= startMo 1) (= endMo 12))
				     ""
				     startMo)
		endYr (if (and (= startMo 1) (= endMo 12))
			  ""
			  endMo)))
  (compute-premiumsForPeriod :month startYr endYr :include-splits? t)
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;;header
    (format dumpfile "~%Period Rank~aAgent~aTopAgent~aCity~aST~aCode~aRMD~aPeriod Premium~a% of Total"
	    #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB)
	 
    (let* ((collected-agents nil)
	   rankedAgents
	   totalPremium)
      (map-agents #'(lambda (a) 
		      (if (periodPremium a)
			    (push (cons a 
					(loop with totQtsThisAgent = 0
					   for YR from startYr to endYr do
					     (loop for MO 
						from (if (= YR startYr) startMo 1)
						to (if (= YR endYr) endMo 12) do
						  (incf totQtsThisAgent
							(aref (periodPremium a) (- YR startYr) (- MO 1))))
					   finally (return totQtsThisAgent))
					;;(if subQuarters
					 ;;   (loop for qtr in subQuarters
					  ;;     summing (aref (periodPremium a) 0 (- qtr 1))
					   ;;    into totQtsThisAgent
					    ;;   finally (return totQtsThisAgent))
					    ;;else :annual
					   ;; (aref (periodPremium a) 0))
					)
				  collected-agents))))
	(setf rankedAgents (sort collected-agents #'> :key #'cdr))
	(setf totalPremium (reduce #'+ rankedAgents :key #'cdr))
	
	(loop for i from 1
	   for (agent . premium) in rankedAgents 
	   do (format dumpfile "~%~s~a~a~a~a~a~a~a~a~a~a~a~a~a~,0D~a~,2D"
		      i #\TAB
		      (agent-nameLastFirst agent) #\TAB
		      (if (topMostAgentBelowRegion agent)
			  (string-trim "^^" 
				       (agent-nameLastFirst (topMostAgentBelowRegion agent)))
			  "N/A") #\TAB
			  (string-capitalize (or (AGD-100-BUS-CITY agent) "")) #\TAB
			  (or (AGD-100-BUS-TERR-STATE agent) "") #\TAB
			  (AGD-100-AGENT agent) #\TAB
			  (RMD-shortname agent) #\TAB
			  (round premium) #\TAB
			  (/ premium totalPremium))))))

;;; 15-Feb-19 WSD added
(defun dump-YTDTopLevelAgentRanking (&optional (dumpPath (join outputDocumentPath "YTDAgentRankingTL.tsv")) dumpYr)
  (prep-mainAgentObjects)
  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time (get-universal-time))
    (declare (ignore sec min hr day mo))
    (if dumpYr (setf yr dumpYr))	;use dumpYr if we have it
    (compute-premiumsForPeriod :annual  yr yr)
    (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
      ;;header
      (format dumpfile "~%Rank~aAgent~aCity~aST~aCode~aRMD~a~s Premium~a% of Total"
	      #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB yr #\TAB)
    
      (let* ((regions (list *GLakesRegion* *CentralRegion* *WestRegion* *HomeOffice*))
	     (collected-agents nil)
	     (visitedAgentCodes (make-hash-table :test #'Equal))
	     rankedAgents
	     totalPremium)
	(loop for region in regions
	   do 
	     (loop for sub in (subagents region)
		unless (or (member sub regions) ;other regions are under *HomeOffice*
			   (gethash (AGD-100-NAME sub) visitedAgentCodes))
		do
		;;record this sub's premium
		  (let ((hierPremium (total-periodPremiumSubsInclAliases sub :annual)))
		    (if (and hierPremium
			     (> hierPremium 0)) 
			(push (cons sub hierPremium)
			      collected-agents)))
		;;remember that we've seen him and all aliases
		  (setf (gethash (AGD-100-NAME sub) visitedAgentCodes) t)
		  (loop for alias in (get-aliases sub)
		     do (setf (gethash alias visitedAgentCodes) t))))
	
     	(setf rankedAgents (sort collected-agents #'> :key #'cdr))
	(setf totalPremium (reduce #'+ rankedAgents :key #'cdr))
	
	(loop for i from 1
	   for (agent . premium) in rankedAgents 
	   do (format dumpfile "~%~s~a~a~a~a~a~a~a~a~a~a~a~,0D~a~,2D"
		      i #\TAB
		      (agent-nameLastFirst agent) #\TAB
		      (string-capitalize (or (AGD-100-BUS-CITY agent) "")) #\TAB
		      (or (AGD-100-BUS-TERR-STATE agent) "") #\TAB
		      (AGD-100-AGENT agent) #\TAB
		      (RMD-shortname agent) #\TAB
		      (round premium) #\TAB
		      (/ premium totalPremium)))))))







;;; 13-Jul-05 WSD added
(defun dumpPolCounts (&optional (startYr 2013) (endYr 2013))
  (compute-policyCountsForPeriod startYr endYr)
  (dump-YTDPolicyHierarchy (join outputDocumentPath "YTDpoliciesHierarchy.tsv")))


;;; 13-Jul-17 WSD dumping data for Salesforce.com import
(defun dump-YTDBrokerDataForSF (&optional (year 2017))
  (prep-mainAgentObjects)
  (compute-premiumsForPeriod :annual  year year)
  (with-open-file (dumpfile
		   (format nil (join outputDocumentPath "agentsDumpedToSalesforce-~a.tsv") year)
		   :direction :output :if-exists :supersede :if-does-not-exist :create)
    
    (format dumpfile "AgentCode~aContractCode~aYTD_Production~aYTD_Production_Hier~aDateOfLastPolicy~aUpline~aUplineCode~aName~aEmail~aAddr1~aAddr2~aCity~aState~aZip~aBusPhone~aHomePhone~aRD"
	    #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB #\TAB) 
    
    (map-agents 
     #'(lambda (agent)
	 (princ ".") 
	 (let ((lastPolDate (date-ofLatestPolicy agent)))
	   (format dumpfile "~%~a~a~a~a~:D~a~:D~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a"
		 (AGD-100-AGENT agent) #\TAB
		 (if (contractCode agent) 
		     (contractCode agent)
		   "") #\TAB
		 (if (periodPremium agent)
		     (round (aref (periodPremium agent) 0))
		   0)
		 #\TAB
		 (if (periodPremiumSubs agent)
		     (round (aref (periodPremiumSubs agent) 0))
		   0) 
		 #\TAB
		 (if lastPolDate
		       (datestring lastPolDate)
		   "0000-00-00") 
		 #\TAB 
		 (if (parent agent) (AGD-100-NAME (parent agent)) "")
		 #\TAB
		 (if (parent agent) (AGD-100-AGENT (parent agent)) "")
		 #\TAB 
		 (AGD-100-NAME agent) #\TAB
		 (AGD-100-EMAIL-ADDR agent) #\TAB
		 (AGD-100-BUS-ADDR-1 agent) #\TAB
		 (AGD-100-BUS-ADDR-2 agent) #\TAB
		 (AGD-100-BUS-CITY agent) #\TAB
		 (AGD-100-BUS-TERR-STATE agent) #\TAB
		 (AGD-100-BUS-ZIP agent) #\TAB
		 (AGD-100-BUS-PHONE agent) #\TAB
		 (AGD-100-HOME-PHONE agent) #\TAB
		 (RMD-shortname agent)
		 ))))))


;;; 14-Jan-17 WSD faster version of dump raw counts & premium values using hash table
(defun dump-rawPolPremCountTable (&optional (year 2013) (by-receive-date? nil)) 
  (let ((polsHashedByPlan (make-hash-table :test #'EQUAL)))
    
    (map-policies #'(lambda (p) 
		     (let ((hashVal (gethash (EX-PLAN-AND-OPT p) polsHashedByPlan)))
		       (setf (gethash (EX-PLAN-AND-OPT p) polsHashedByPlan)
			    (if hashVal
				(push p hashVal)
			      (list p)))))
		  :startYr year :endYr year 
		  :by-recv-date? by-receive-date?
		  )
    
    (with-open-file (dumpfile
		     (format nil (join outputDocumentPath "rawPolicyDumpCounts_~a.csv") 
			     (if by-receive-date? "RECD" "ISSU"))
		     :direction :output :if-exists :supersede :if-does-not-exist :create)

      (format dumpfile "~%Plan,NotDeclined,Premium,Declined,Premium")
      (maphash #'(lambda (key polsList)
		   (let ((numPols 0)
			 (numPolsDecline 0)
			 (totPrem 0)
			 (totPremDecline 0))
		     (dolist (p polsList)
		       (incf numPols)
		       (incf totPrem (annualPremium p))
		       (when (string-equal "Z" (EX-STATUS p))
			 (incf numPolsDecline)
			 (incf totPremDecline (annualPremium p))))
		     (format dumpfile "~%~a, ~s, ~s, ~s, ~s" 
			     key (- numPols numPolsDecline) (- totPrem totPremDecline) numPolsDecline totPremDecline)))
	       polsHashedByPlan)
      
        )))

;;; 14-Sep-12 WSD added
;;; 15-Mar-10 WSD AD&D PlanCodes now included in BGL and VGL
;;; 16-Aug-17 WSD added plan code for HI and ACC
(defun productTypeKey (policyObj)
  (let ((planCode (if (string-equal "Z" (EX-STATUS policyObj)) ;declined
		      "DECLINED"
		    (EX-PLAN-AND-OPT policyObj))))

    (cond
     ((string-equal planCode "DECLINED") :DECLINE)
     ((find planCode *PlanCodes-CritIllness* :test #'string-equal) :CI)
     ((or (find planCode *PlanCodes-FPPTI* :test #'string-equal)
	  (find planCode *PlanCodes-FPPCI* :test #'string-equal)) :FPP)
     ((find planCode *PlanCodes-BasicGL* :test #'string-equal) :BAS)
     ((find planCode *PlanCodes-VolGL* :test #'string-equal) :VOL)
     ((find planCode *PlanCodes-HI* :test #'string-equal) :HI)
     ((find planCode *PlanCodes-ACC* :test #'string-equal) :ACC)
     
     ((or (string-equal "ELT10" planCode) 
	  (string-equal "ELT15" planCode)
	  (string-equal "ELT20" planCode)
	  (string-equal "ELT30" planCode)) :ELT)
     ((string-equal "GRPFRE" planCode) :FREE)
     (t (progn
	  (format t ".. ~a" planCode)
	  :OTHER)))
    ))



;;; 14-Sep-12 WSD added
;;; 15-Jan-27 WSD added bridge & recv dates, also blank agent counted as Home Office by default
;;; 16-Aug-17 WSD added HI and ACC, removed :ADD references
(defun dump-rmdPolCounts (&optional (endYear 2017) (by-recv-date? nil) (by-bridge-date? nil) (only-paid-policies? nil))
  (flet ((regionIndex (agent)
	   (cond 
	    ((null agent) 3)		;if no agent, assume Home Office
	    ((is-subagent-of? agent *CentralRegion*) 0)
	    ((is-subagent-of? agent *GLakesRegion*) 1)
	    ((is-subagent-of? agent *WestRegion*) 2)
	    ((is-subagent-of? agent *HomeOffice*) 3)))
	 (productIndex (policy)
	   (case (productTypeKey policy)
	     (:CI 0)
	     (:FPP 1)
	     (:VOL 2)
	     (:BAS 3)
	     (:HI 4)
	     (:ACC 5)
	     (:ELT 6)
	     (:FREE 7)
	     (:DECLINE 8)
	     (t (error "Encountered unexpected plan code: ~s in policy ~s" (productTypeKey policy)  (EX-POLICY policy))))))
    ;;do it
    ;; 4 regions, 12 months, 9 product types
    (let ((polCounts (make-array '(4 12 9) :initial-element 0)))
      (map-policies #'(lambda (p)
			(incf (aref polCounts 
				    (regionIndex (getAgent (EX-WRITING-AGENT1 p)))
				    (- (monthNum (if by-recv-date?
						     (received-date p)
						     (if by-bridge-date?
							 (bridge-date p)
							 (EX-ISSUE-DATE p))))
				       1)
				    (productIndex p))))
		    :startyr endYear
		    :endyr endYear
		    :by-recv-date? by-recv-date?
		    :by-bridge-date? by-bridge-date?
		    :only-paid-policies? only-paid-policies?)
      ;;now dump results
      (with-open-file (dumpfile
		       (format nil (join outputDocumentPath "RMD-MonthlyPolicy-Summary-~a-~a~a.tsv")
			       endYear
			       (if by-recv-date?
				   "received"
				   (if by-bridge-date?
				       "bridged"
				       "issued"))
			       (if only-paid-policies? "(paid)" ""))
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
	;;header
	(format dumpfile "~a-YTD Regional Policy Counts Monthly Summary~%" endYear)
	(format dumpfile "~a" #\TAB)
	(dolist (mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	  (format dumpfile "~a~a~a" #\TAB mo #\TAB))
	;;summary rows
	(format dumpfile "~%Total~a" #\TAB)
	(let ((tempTotals (make-array 12 :initial-element 0)))
	  (loop for mo from 0 to 11 
	      as totalMo = (loop for rmd from 0 to 3 
			       summing (loop for prod from 0 to 7 ;don't include DECLINEs in total sum 
					   summing (aref polCounts rmd mo prod)))
	      do (setf (aref tempTotals mo) totalMo)
		 (format dumpfile "~a~a~a" #\TAB totalMo #\TAB))
	  (loop for prod from 0 to 8
	      and prodType in '("CI" "FPP" "VOL" "BAS" "HI" "ACC" "ELT" "GRP FREE" "Declined")
	      do (format dumpfile "~%~a~a" #\TAB prodType)
		 (loop for mo from 0 to 11
		     as thisMonth = (loop for rmd from 0 to 3 summing (aref polCounts rmd mo prod))
		     do (format dumpfile "~a~a~a~,3f"
				#\TAB thisMonth
				#\TAB 
				(if (zerop (aref tempTotals mo))
				    0
				  (/ thisMonth (aref tempTotals mo))))))	
	  ;;matrix rows
	  (loop for rmd from 0 to 3
	      for region in '("Central South" "Great Lakes" "Western" "Home Office")
	      as rmdTotals = (make-array 12 :initial-element 0)
	      do (format dumpfile "~2%~a~a" region #\TAB)
		 (loop for mo from 0 to 11
		     as totalMo = (loop for prod from 0 to 7 summing (aref polCounts rmd mo prod)) ;don't include DECLINEs in total sum
		     do (setf (aref rmdTotals mo) totalMo)
			(format dumpfile "~a~a~a" #\TAB totalMo #\TAB))
		 
		 (loop for prod from 0 to 8
		     and prodType in '("CI" "FPP" "VOL" "BAS" "HI" "ACC" "ELT" "GRP FREE" "Declined")
		     do (format dumpfile "~%~a~a" #\TAB prodType)
			(loop for mo from 0 to 11
			    do (format dumpfile "~a~a~a~,3f"
				       #\TAB (aref polCounts rmd mo prod)
				       #\TAB (if (zerop (aref rmdTotals mo))
						 0
					       (/ (aref polCounts rmd mo prod) (aref rmdTotals mo)))))))      
	  )))))


;;; 14-Feb-20 WSD added
(defun map-dumpPolicies (accessorAList filetag &key startYr endYr startMo endMo agentCode agentCodes includeSplits 
					       planCodes insured status notstatuslist (by-recv-date? nil) (only-paid-policies? nil))
  
  (with-open-file (dumpfile
		   (format nil (join outputDocumentPath "policyDump-~a.tsv") filetag)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;;print header
    (format dumpfile "~%~a" (caar accessorAList))
    (dolist (accessor (cdr accessorAList))
      (format dumpfile "~a~a" #\TAB (car accessor)))
    ;;print rows
    (map-policies #'(lambda (p)
			(format dumpfile "~%~a" (funcall (cdar accessorAList) p))
			(dolist (accessor (cdr accessorAList))
			  (format dumpfile "~a~a" #\TAB (funcall (cdr accessor) p))))
		    :startYr startYr
		    :endYr endYr
		    :startMo startMo
		    :endMo endMo
		    :agentCode agentCode
		    :agentCodes agentCodes
		    :includeSplits includeSplits 
		    :planCodes planCodes
		    :insured insured
		    :status status
		    :notstatuslist notstatuslist
		    :by-recv-date? by-recv-date?
		    :only-paid-policies?  only-paid-policies?
		    )))


;;; 14-Feb-21 WSD added
;;; 14-Aug-07 WSD updated with RMD and a couple tweaks
(defun dump-policies (&optional (startYear 2017) (endYear nil)  &key (notstatuslist '(:declined))  (by-recv-date? nil))
  (map-dumpPolicies `(
		      ("Policy #" . EX-POLICY) 
		      ;;("SSN" . insuredSSN)
		      ("Insured" . EX-INS-NAME)
		      ("Group" . ,#'(lambda (p)
				      (if (gethash (employerID p) *EmployerGroups*)
					  (DEM-CO-NAME (gethash (employerID p) *EmployerGroups*))
					"none")))
		      ("Situs State" . ,#'(lambda (p)
					    (if (gethash (employerID p) *EmployerGroups*)
						(situsState (gethash (employerID p) *EmployerGroups*))
					      "")))
		      ("PlanCode" . EX-PLAN-AND-OPT) 
		      ("ProductType" . productTypeKey)
		      ("Issue Date" . ,#'(lambda (p) (if (EX-ISSUE-DATE p) (datestring (EX-ISSUE-DATE p))
						       "")))
		      ("Receive Date" . ,#'(lambda (p) (if (received-date p) (datestring (received-date p))
							 "")))
		      ("Paid-To Date" . ,#'(lambda (p) (if (EX-DUE-DATE p) (datestring (EX-DUE-DATE p))
							 "")))
		      ("Bridge Date" . ,#'(lambda (p) (if (bridge-date p) (datestring (bridge-date p))
							 "")))
		      ("Status Date" . ,#'(lambda (p) (if (EX-STATUS-DATE p) (datestring (EX-STATUS-DATE p))
							 "")))
		      ("Status" . EX-STATUS)
		      ("Benefit Amount" . EX-FACE-AMOUNT)
		      ("Annual Premium" . annualPremium)
		      ("Writing Agent" . EX-WRITING-AGENT1)
		      ("Agent" . ,#'(lambda (p) 
				      (let ((agent (getagent (EX-WRITING-AGENT1 p))))
					(if agent
					    (concatenate 'string 
					      (agent-nameLastFirst agent)
					      " ["
					      (contractCode agent)
					      "]")
					  "none"))))
		      ("TL Agent" . ,#'(lambda (p) 
				      (let* ((agent (getagent (EX-WRITING-AGENT1 p)))
					     (TLA (topMostAgentBelowRegion agent)))
					(if TLA 
					    (agent-nameLastFirst TLA)
					    "none"))))
		      #|
		      ("RD" . ,#'(lambda (p) 
				   (let ((agent (getagent (EX-WRITING-AGENT1 p))))
				     (if agent
					 (RMD-shortname agent)
				       "none"))))
		      |#
		      )
		    (if endYear (format nil "~a-~a" startYear endYear) (format nil "~a" startYear))
		    :startYr startYear
		    :endYr (if endYear endYear startYear)
		    ;;:planCodes (union *PlanCodes-CritIllness* nil)
		    :only-paid-policies? nil
		    :notstatuslist notstatuslist
		    :by-recv-date? by-recv-date?
		    )

              ;; -- use this to grab only the GL and AD&D pols
	      ; "2014-GL_ADD"
	      ; :startYr 2014
	      ; :endYr 2014
	      ; :planCodes (union *PlanCodes-GroupLife* *PlanCodes-ADD*))
  )

;;; 13-Jul-12 WSD changed to dump parseRecs log to same directory as other output
;;; 14-Feb-18 WSD added parseEmployerRecords and parsePolicyPRRecords
;;; 14-Mar-04 WSD added setup-NewRMDAgentObjects
;;; 16-Jul-26 WSD added prep-mainAgentObjects
;;; 16-Aug-04 WSD added setup-riderPolicies

;;; 16-Aug-28 WSD comment-out setup-riderPolicies for now
(defun parseALL (&key (path (join outputDocumentPath "parseRecs.txt")))
  (if path

      (with-open-file (dumpfile path :direction :output :if-exists :supersede :if-does-not-exist :create) 
	(format dumpfile "~%~a~%" (timestamp (get-universal-time)))
        (format t "~%[~a]...Parsing Input Files..." (timestamp (get-universal-time)))
	(parsePolicyAARecords dumpfile)
	(parsePolicyCORecords dumpfile)
	(parsePolicySBRecords dumpfile)
	(parsePolicyNBRecords dumpfile)
	(parseEmployerRecords dumpfile)	
	(parsePolicyPRRecords dumpfile)	
	(parsePolicyINRecords dumpfile)	
	(parseAgent100Records dumpfile)
	;;(parseHierarchyTuples dumpfile)
	(setup-NewRMDAgentObjects)
	(parseHierarchyFiles dumpfile)
	(setup-exceptionsToParsing)
	(setup-newBizDatesInPolicies)
	(establish-agentAliases)
	(compute-policySplits)
	;;(setup-riderPolicies)
	(prep-mainAgentObjects)
	;;(dumpHierarchyCheck)
	)
    ;;else, no dump
    (progn
      (parsePolicyAARecords)
      (parsePolicyCORecords)
      (parsePolicySBRecords)
      (parsePolicyNBRecords)
      (parseEmployerRecords)
      (parsePolicyPRRecords)	
      (parsePolicyINRecords)	
      (parseAgent100Records)
      (setup-NewRMDAgentObjects)
      (parseHierarchyFiles)
      (setup-exceptionsToParsing)
      (setup-newBizDatesInPolicies)
      (establish-agentAliases)
      (compute-policySplits)
      ;;(setup-riderPolicies)
      ;; also, set
      (prep-mainAgentObjects)
      )
    )
  (dump-orphanedAgents))

;;; 12-Jul-13 WSD single-call to dump new .csv files
;;; 15-Aug-10 WSD not using YTDAgentRanking for now
(defun dumpCSV (&optional (endYear 2015))
  (let ((currentPath outputDocumentPath))
    (dump-YoYAnnualSummary 2007 endYear (concatenate 'string currentPath "annualSummary.csv"))
    (dump-monthsYTDSummary (concatenate 'string currentPath "annualSummary.csv") endYear)
    ;;(dump-YTDAgentRanking (concatenate 'string currentPath "YTDAgentRanking.tsv") endYear)
    (dump-monthsYTDHierarchy (concatenate 'string currentPath "YTDhierarchy.tsv") endYear)
    ))

;;; 16-Mar-10 WSD new multi-year Month-Over-Month report
(defun dump-YoYMonthlySummary (startYear endYear &optional (dumpPath (join outputDocumentPath "annualSummary.csv")) (only-paid-policies? nil))
  (prep-mainAgentObjects)
  (compute-premiumsForPeriod :month  startYear endYear :only-paid-policies? only-paid-policies? :include-splits? nil)
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :append :if-does-not-exist :create)
    (dotimes (i 3) (format dumpfile "~%"))
    ;;header
    (format dumpfile "Issue Year")
    (dolist (mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" "TOTAL"))
      (format dumpfile ",~a" mo))
    ;;body
    (loop for Yr from startYear to endYear do
	 (format dumpfile "~%~a" Yr)
	 (let ((yearTot 0))
	   (dotimes (i 12)
	     (format dumpfile ",~D"
		     (round (aref (periodPremiumSubs *HomeOffice*) (- Yr startYear) i)))
	     (incf yearTot (round (aref (periodPremiumSubs *HomeOffice*) (- Yr startYear) i))))
	   (format dumpfile ",~D" yearTot)))
    ))


;;; 14-Feb-21 WSD grasping at straws to find a format that works for the customer :-)
;;; 14-Feb-24 WSD format tweaks
;;; 15-Aug-10 WSD add monthly Group premium totals to groupPolSummary/summaryAlist
;;; 15-Aug-10 WSD added groupPremMonthlyTotals to track group annual premium sales by issue-month
;;; 16-Jul-11 WSD add optional include-group-id? to possibly include group ID in dump
(defun dump-groupSalesSummary (year &optional (by-recv-date? nil) (breakout-plancodes? nil) (include-group-id? nil))
  ;; 1. build hash of all Employers accumulating pol count and premium in alist via plancode
  ;; 2. along the way, remember via hash all unique plancodes touched (anything "declined" will be treated as its own code)
  ;; 3. dump summary

  (maphash #'(lambda (employerID employer)
   	       (declare (ignore employerID))
   	       (setf (agents employer) nil))
   	   *EmployerGroups*)

  (prep-mainAgentObjects)
					;reset tracking of writing agents

  (let ((groupPolSummary (make-hash-table :test #'equal))
	(groupPremMonthlyTotals (make-hash-table :test #'equal))
	(planCodes (make-hash-table :test #'equal))

	;;compute premiums so we can list 'main agents' by premium amounts
	(agentPremiumHashTable (make-hash-table))
	)


    (compute-premiumsForPeriod :annual  year year :include-splits? nil)
    (map-agents #'(lambda (a)
		    (setf (gethash a agentPremiumHashTable)
			  (if (periodPremium a)
			      (periodPremium a)
			      0))))

    (map-policies #'(lambda (p)
		      (let* ((polAnnualPremium (annualPremium p))
			     (planCode (if (string-equal "Z" (EX-STATUS p)) ;declined
					   "DECLINED"
					 (EX-PLAN-AND-OPT p)))
			     (summaryAlist (gethash (employerID p) groupPolSummary))
			     (a (if summaryAlist (assoc planCode summaryAlist :test #'string-equal)))
			     (groupMonthsArray (gethash (employerID p) groupPremMonthlyTotals)))
			;;first time initiate monthly totals array
			(unless groupMonthsArray
			  (setf groupMonthsArray
				(setf (gethash (employerID p) groupPremMonthlyTotals)
				      (make-array 12 :initial-element 0))))
			;;then add to proper month index as long as not DECLINED
			(unless (string-equal planCode "DECLINED")
			  (multiple-value-bind (sec min hr day mo yr) (decode-universal-time (EX-ISSUE-DATE p))
			    (declare (ignore sec min hr day yr))
			    (incf (aref groupMonthsArray (- mo 1)) polAnnualPremium)))

			;;now summarize via plancode
			(if a
			    (progn
			      (incf (cadr a)) ;update policy tally
			      (incf (cddr a) polAnnualPremium)) ;update premium total
			  ;;else, update alist with new code (if nil summaryAlist, then will be first one)
			  (setf (gethash (employerID p) groupPolSummary)
			    (acons planCode (cons 1 polAnnualPremium) summaryAlist)))
			;;keep track of all plancodes visited
			(setf (gethash planCode planCodes) planCode)
			)

		      ;;while here, keep track of the writing agents also
		      (let ((employer (gethash (employerID p) *EmployerGroups*)))
			(when (and employer (EX-WRITING-AGENT1 p) (not (equal (EX-WRITING-AGENT1 p) "")))
			  (pushnew (EX-WRITING-AGENT1 p) (agents  employer) :test #'equal))))

		    :startYr year
		    :endYr year
		    :by-recv-date? by-recv-date?
		    )
    ;;now dump the table
    (with-open-file (dumpfile
		     (format nil (join outputDocumentPath "policySummary-~a.tsv") year)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
      ;;print header
      (let* ((planCodeList (sort (loop for val being the hash-values of planCodes collect val)
				 #'(lambda (a b)
				     (cond ;want to have DECLINED always leftmost in plan code headers
				      ((string-equal a "DECLINED") t)
				      ((string-equal b "DECLINED") nil)
				      (t (string-lessp a b)))))))

	(format dumpfile "Employer")
	(when include-group-id?
	  (format dumpfile "~a" #\TAB)
	  (format dumpfile "GroupID"))

	(dolist (header '("Region" "Top Agent" "Agent" "Agent Code(s)"
			  "Total Premium" "Total Pol Count"
			  "BGL Prem" "BGL Count" "VGL Prem" "VGL Count" "FPP Prem" "FPP Count" "CI Prem" "CI Count" "HI Prem" "HI Count" "ACC Prem" "ACC Count"))
	  (format dumpfile "~a~a" #\TAB header))
	;; to columns for each separate plancode - either just DECLINED, or else full breakout
	(format dumpfile "~a~a $~a~a #" #\TAB (car planCodeList) #\TAB (car planCodeList))
	(when breakout-plancodes?
	  (dolist (code (cdr planCodeList))
	    (format dumpfile "~a~a $~a~a #" #\TAB code #\TAB code)))
	;;now Month headers
	(dolist (mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
	  (format dumpfile "~a~a" #\TAB mo))


	(maphash #'(lambda (employerID summaryAlist)
		     (let* ((employer (gethash employerID *EmployerGroups*))
			    (employerPremMonthlyTotals (gethash employerID groupPremMonthlyTotals))
			    (multiple-agents? (and employer
						   (> (length (agents employer)) 1)))
			    (topMostEmployerAgent (if employer (topMostAgentAmongAgentList (agents employer)
											   agentPremiumHashTable)))
			    (totGLPrem 0)
			    (totGLNum 0)
			    (totVGPrem 0)
			    (totVGNum 0)
			    (totFPPrem 0)
			    (totFPNum 0)
			    (totCIPrem 0)
			    (totCINum 0)
			    (totHIPrem 0)
			    (totHINum 0)
			    (totACCPrem 0)
			    (totACCNum 0)
			    (otherPrem 0)
			    (otherNum 0))

		       (loop for (code . (count . prem)) in summaryAlist do
			     (cond
			      ((find code *PlanCodes-BasicGL* :test #'string-equal)
			       (incf totGLPrem prem)
			       (incf totGLNum count))
			      ((find code *PlanCodes-VolGL* :test #'string-equal)
			       (incf totVGPrem prem)
			       (incf totVGNum count))
			      ((or (find code *PlanCodes-FPPTI* :test #'string-equal)
				   (find code *PlanCodes-FPPCI* :test #'string-equal))
			       (incf totFPPrem prem)
			       (incf totFPNum count))
			      ((find code *PlanCodes-CritIllness* :test #'string-equal)
			       (incf totCIPrem prem)
			       (incf totCINum count))
			      ((find code *PlanCodes-HI* :test #'string-equal)
			       (incf totHIPrem prem)
			       (incf totHINum count))
			      ((find code *PlanCodes-ACC* :test #'string-equal)
			       (incf totACCPrem prem)
			       (incf totACCNum count))
			      ;;everything else that isn't in declined status
			      ((not (string-equal code "DECLINED"))
			       (incf otherPrem prem)
			       (incf otherNum count))))

		       (macrolet ((z* (qty) `(if (zerop ,qty) "" (round ,qty))))

			 (format dumpfile "~%~a~a~s~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~:D~a~:D~a~:D~a~:D~a~:D~a~:D~a~:D~a~:D~a~:D~a~:D"
				 (if employer (DEM-CO-NAME employer) "none") #\TAB
				 ;; groupID is optional, so don't include any column/TAB for it if ommitted
				 (if include-group-id? employerID "")
				 (if include-group-id? #\TAB "")


				 (if employer (DEM-REGION-ID employer) "--") #\TAB
				 (if topMostEmployerAgent
				     (if (topMostAgentBelowRegion (getagent topMostEmployerAgent))
				      (string-trim "^^" (agent-nameLastFirst (topMostAgentBelowRegion (getagent topMostEmployerAgent))))
				      topMostEmployerAgent)
				     "") #\TAB
				 ;;when printing agent name, strip any leading "**"
				 (if topMostEmployerAgent
				     (if multiple-agents?
					 "various"
					 (format nil "~a" (string-trim "^^" (agent-nameLastFirst topMostEmployerAgent))))
				     "") #\TAB
				 ;;agent Code(s)
				 (if multiple-agents?
				     (list-to-delimited-string (agents employer) ", ")
				     (if employer (car (agents employer)) "")) #\TAB
				 ;;tot prem and pol counts
				 (round (+ totGLPrem totVGPrem totFPPrem totCIPrem totHIPrem totACCPrem otherPrem)) #\TAB
				 (+ totGLNum totVGNum totFPNum totCINum totHINum totACCNum otherNum) #\TAB
				 ;;aggregate amounts
				 (z* totGLPrem) #\TAB (z* totGLNum) #\TAB (z* totVGPrem) #\TAB (z* totVGNum) #\TAB
				 (z* totFPPrem) #\TAB (z* totFPNum) #\TAB (z* totCIPrem) #\TAB (z* totCINum) #\TAB
				 (z* totHIPrem) #\TAB (z* totHINum) #\TAB (z* totACCPrem) #\TAB (z* totACCNum)))
		       ;;dump breakout amounts per individual code
		       ;; - either all codes if breakout, otherwise just DECLINED
		       (loop for code in (if breakout-plancodes?
					     PlanCodeList
					     (list (car PlanCodeList)))
			  as (nil qty . prem) = (assoc code summaryAlist :test #'string-equal) do
			    (format dumpfile "~a~:D~a~:D"
				    #\TAB
				    (or (and prem (round prem)) "")
				    #\TAB
				    (or qty "")))
		       ;;dump Monthly group Issues premium
		       (dotimes (i 12)
			 (format dumpfile "~a~D"
				 #\TAB (round (aref employerPremMonthlyTotals i))))
		       ))
		 groupPolSummary)
	)
	)
	)
)

;;; 14-Jan-31 WSD added - too lazy to merge into a single function with monthly version
;;; 14-Mar-04 WSD changed RMD variables
;;; 14-Jul-15 WSD added include-polCounts?
;;; 15-Aug-10 WSD possibly credit 100% of premiums to Writing agents via compute-premiumsForPeriod include-splits?

(defun dump-annualHierarchy (startYear endYear &optional (include-zeroTotals? nil) (include-polCounts? nil) (include-splits? nil))
  (format t "~%[~a]..prepping agent objects.." (timestamp (get-universal-time))) 
  (prep-mainAgentObjects)
  (format t "~%[~a]..agent objects prepped.." (timestamp (get-universal-time))) 

  (format t "~%[~a]..computing annual premiums.." (timestamp (get-universal-time)))
  (compute-premiumsForPeriod :annual  startYear endYear :include-splits? include-splits?)
  (format t "~%[~a]..annual premiums computed.." (timestamp (get-universal-time)))
  
  (with-open-file (dumpfile
		     (format nil (join outputDocumentPath "HierarchyDump_~a-~a.tsv") startYear endYear)
		     :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format t "~%[~a]..writing HierarchyDump_~a-~a.tsv.." (timestamp (get-universal-time)) startYear endYear)
  
    ;;each Month gets 2 columns for this agent and all agents
    (dotimes (i 1) (format dumpfile "~%"))
    ;;header
    (format dumpfile "Name [Code : Contract]")
    (dotimes (i 7) (princ #\TAB dumpfile))
    (loop for i from endYear downto startYear do
	  (format dumpfile "~a~a" #\TAB i))
    (loop for rmd in (list *CentralRegion* *WestRegion* *GLakesRegion* *HomeOffice*)
	do
	 (format t "~%[~a]..writing heirachy for ~a" (timestamp (get-universal-time)) rmd)
	 (dump-annualSubHierarchy rmd 0 dumpfile include-zeroTotals? include-polCounts? startYear endYear)
    )))

;;; 12-Sep-04 WSD added
;;; 14-Feb-05 WSD swapped dumpPolCounts for dump-rawPolPremCountTable
;;; 14-Jun-30 WSD changed name from doIMA
;;; 15-Aug-10 WSD tweaked call to dump-annualHierarchy to not include splits
(defun doMonthlyDump (&optional (endYear 2017))
  (format t "~%[~a]...start parsing..." (timestamp (get-universal-time))) 
  (parseAll)
  (format t "~%[~a]...parsing complete..." (timestamp (get-universal-time)))

  (format t "~%[~a]...running dumpCSV..." (timestamp (get-universal-time)))
  (dumpCSV endYear)
  (format t "~%[~a]...dumpCSV done..." (timestamp (get-universal-time)))

  (format t "~%[~a]...running YoYMonthlySummary..." (timestamp (get-universal-time)))
  (dump-YoYMonthlySummary 2007 endYear)
  (format t  "~%[~a]...YoYMonthlySummary done..." (timestamp (get-universal-time)))

  (format t "~%[~a]...running groupSalesSummary..." (timestamp (get-universal-time)))
  (dump-groupSalesSummary endYear)
  (format t "~%[~a]...groupSalesSummary done..." (timestamp (get-universal-time)))

  (format t "~%[~a]...running annualHierarchy..." (timestamp (get-universal-time)))
  (dump-annualHierarchy (- endYear 1) endYear nil t nil)
  (format t "~%[~a]...annualHierarchy done..." (timestamp (get-universal-time)))

  (format t "~%[~a]...running rmdPolCounts for ~a..." (timestamp (get-universal-time)) endYear)
  (dump-rmdPolCounts endYear)
  (format t "~%[~a]...done with rmdPolCounts for ~a ..." (timestamp (get-universal-time)) endYear)

  (format t "~%[~a]...running rmdPolCounts again for ~a..." (timestamp (get-universal-time)) endYear)
  (dump-rmdPolCounts endYear nil t nil)  ;; bridge date
  (format t "~%[~a]...running rmdPolCounts again for ~a..." (timestamp (get-universal-time)) endYear)

  (format t "~%[~a]...running rmdPolCounts by month for ~a..." (timestamp (get-universal-time)) endYear)
  (dump-rmdPolCounts endYear nil nil t)  ;; paid-pol only
  (format t "~%[~a]...done running rmdPolCounts by month for ~a..." (timestamp (get-universal-time)) endYear)

  ;;(dump-YTDBrokerDataForSF endYear)
  ;;(format t "~%[~a]...dumped Salesforce broker data...~%" (timestamp (get-universal-time)))
  ;;(dumpPolCounts)
  )
