
(defmacro tagHier-level (agentTuple)
  `(read-from-string (nth 6 ,agentTuple)))

(defclass HIERARCHY ()
;;; slotlist (AGD-200-AGENT AGD-200-EFF-DATE AGD-200-SUBCNT 
;;; AGD-200-HIER-COUNT AGD-200-FILLER-AGENT AGD-200-FILLER-SUBCNT 
;;; AGD-200-RPT-LEVEL AGD-200-CONT AGD-200-HT-AGENT-1 AGD-200-HT-SUBCNT-1
;;; AGD-200-HT-CONT-1 AGD-200-HT-AGENT-2 AGD-200-HT-SUBCNT-2 AGD-200-HT-CONT-2
;;; AGD-200-HT-AGENT-3 AGD-200-HT-SUBCNT-3 AGD-200-HT-CONT-3 AGD-200-HT-AGENT-4
;;; AGD-200-HT-SUBCNT-4 AGD-200-HT-CONT-4 AGD-200-HT-AGENT-5 AGD-200-HT-SUBCNT-5
;;; AGD-200-HT-CONT-5 AGD-200-HT-AGENT-6 AGD-200-HT-SUBCNT-6 AGD-200-HT-CONT-6
;;; AGD-200-HT-AGENT-7 AGD-200-HT-SUBCNT-7 AGD-200-HT-CONT-7)
  (
   (AGD-200-AGENT
    :accessor AGD-200-AGENT
    :initform nil
    :initarg :AGD-200-AGENT)
   (AGD-200-EFF-DATE
    :accessor AGD-200-EFF-DATE
    :initform nil
    :initarg :AGD-200-EFF-DATE)
   (AGD-200-SUBCNT
    :accessor AGD-200-SUBCNT
    :initform nil
    :initarg :AGD-200-SUBCNT)
   (AGD-200-HIER-COUNT
    :accessor AGD-200-HIER-COUNT
    :initform nil
    :initarg :AGD-200-HIER-COUNT)
   (AGD-200-FILLER-AGENT
    :accessor AGD-200-FILLER-AGENT
    :initform nil
    :initarg :AGD-200-FILLER-AGENT)
   (AGD-200-FILLER-SUBCNT
    :accessor AGD-200-FILLER-SUBCNT
    :initform nil
    :initarg :AGD-200-FILLER-SUBCNT)
   (AGD-200-RPT-LEVEL
    :accessor AGD-200-RPT-LEVEL
    :initform nil
    :initarg :AGD-200-RPT-LEVEL)
   (AGD-200-CONT
    :accessor AGD-200-CONT
    :initform nil
    :initarg :AGD-200-CONT)
   (AGD-200-HT-AGENT-1
    :accessor AGD-200-HT-AGENT-1
    :initform nil
    :initarg :AGD-200-HT-AGENT-1)
   (AGD-200-HT-SUBCNT-1
    :accessor AGD-200-HT-SUBCNT-1
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-1)
   (AGD-200-HT-CONT-1
    :accessor AGD-200-HT-CONT-1
    :initform nil
    :initarg :AGD-200-HT-CONT-1)
   (AGD-200-HT-AGENT-2
    :accessor AGD-200-HT-AGENT-2
    :initform nil
    :initarg :AGD-200-HT-AGENT-2)
   (AGD-200-HT-SUBCNT-2
    :accessor AGD-200-HT-SUBCNT-2
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-2)
   (AGD-200-HT-CONT-2
    :accessor AGD-200-HT-CONT-2
    :initform nil
    :initarg :AGD-200-HT-CONT-2)
   (AGD-200-HT-AGENT-3
    :accessor AGD-200-HT-AGENT-3
    :initform nil
    :initarg :AGD-200-HT-AGENT-3)
   (AGD-200-HT-SUBCNT-3
    :accessor AGD-200-HT-SUBCNT-3
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-3)
   (AGD-200-HT-CONT-3
    :accessor AGD-200-HT-CONT-3
    :initform nil
    :initarg :AGD-200-HT-CONT-3)
   (AGD-200-HT-AGENT-4
    :accessor AGD-200-HT-AGENT-4
    :initform nil
    :initarg :AGD-200-HT-AGENT-4)
   (AGD-200-HT-SUBCNT-4
    :accessor AGD-200-HT-SUBCNT-4
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-4)
   (AGD-200-HT-CONT-4
    :accessor AGD-200-HT-CONT-4
    :initform nil
    :initarg :AGD-200-HT-CONT-4)
   (AGD-200-HT-AGENT-5
    :accessor AGD-200-HT-AGENT-5
    :initform nil
    :initarg :AGD-200-HT-AGENT-5)
   (AGD-200-HT-SUBCNT-5
    :accessor AGD-200-HT-SUBCNT-5
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-5)
   (AGD-200-HT-CONT-5
    :accessor AGD-200-HT-CONT-5
    :initform nil
    :initarg :AGD-200-HT-CONT-5)
   (AGD-200-HT-AGENT-6
    :accessor AGD-200-HT-AGENT-6
    :initform nil
    :initarg :AGD-200-HT-AGENT-6)
   (AGD-200-HT-SUBCNT-6
    :accessor AGD-200-HT-SUBCNT-6
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-6)
   (AGD-200-HT-CONT-6
    :accessor AGD-200-HT-CONT-6
    :initform nil
    :initarg :AGD-200-HT-CONT-6)
   (AGD-200-HT-AGENT-7
    :accessor AGD-200-HT-AGENT-7
    :initform nil
    :initarg :AGD-200-HT-AGENT-7)
   (AGD-200-HT-SUBCNT-7
    :accessor AGD-200-HT-SUBCNT-7
    :initform nil
    :initarg :AGD-200-HT-SUBCNT-7)
   (AGD-200-HT-CONT-7
    :accessor AGD-200-HT-CONT-7
    :initform nil
    :initarg :AGD-200-HT-CONT-7)
   ))

;;; 12-Jun-21 WSD changed to lookup agent either by truncated 5-char code or full code
;;; 12-Jun-21 WSD also reworked to skip hierarchy if first record not found; change dumpfile diagnostic messages
;;; 12-Jun-22 WSD abstracted for use from multiple files -- now assumes an open file
;;; 14-Mar-03 WSD changed imaRec to homeOfficeRec
(defun parseTagHierarchyFile (filestream &optional (dumpfile t) (showLinks? nil))
  ;; Method:  
  ;; 1) process all "parent" / upward links, but they can/will be overwritten on subsequent rows
  ;; 2) once a full hierachy is "completed", store that latest hierarchy with (lowest) agent
  ;; 3) subagent lists will be built outside this routine
  (read-line filestream nil nil)		;ignore the header line
  (macrolet ((write-hierarchy (startingAgent newHierarchy)
	       `(when ,startingAgent
		  (when (and (hierarchy ,startingAgent)
			     (not (equal (hierarchy ,startingAgent) ,newHierarchy)))
		    (format dumpfile "  !!hier for ~a (~a) being changed from " 
			    (AGD-100-NAME ,startingAgent)
			    (AGD-100-AGENT ,startingAgent))
		    (print-listOfAgents (hierarchy ,startingAgent) :stream dumpfile)
		    (format dumpfile " to ")
		    (print-listOfAgents ,newHierarchy :stream dumpfile))
		  (setf (hierarchy ,startingAgent) ,newHierarchy))))
    (loop with homeOfficeRec = (getAgent "26Z1A") ;special case to catch false data
	with counter = 1
	with hierarchy = nil
	with startingAgent = nil
	with priorAgent = nil
	with skipToNextLevelOne? = nil
	as buffer = (read-line filestream nil nil)
	when buffer do
	  (incf counter)
	  (let* ((thisTuple (delimited-string-to-list buffer #\COMMA))
		 (thisLevel (tagHier-level thisTuple))
		 ;;attempt to find agent with either last-5 truncated code, or full code as-is from Hierarchy file
		 (thisAgent (or (getAgent (tagHier-agentCode5 thisTuple))
				(getAgent (tagHier-agentCode thisTuple)))))
	    
	    (format dumpfile "~%TAG-Hier row ~s: " counter)
	  (unless (or thisAgent (and skipToNextLevelOne? (> thisLevel 1)))
	    ;;problem if we don't recognize the agent?
	    (format dumpfile " import ~a (level ~s) NOT found in Dell Records" 
		    (tagHier-agentCode thisTuple) thisLevel))
	    
	  (if (= thisLevel 1)
	      (progn
		(format dumpfile "..")	;just something that says new hierarchy; rather than skip
		;;save the prior hierarchy stored up first
		(write-hierarchy startingAgent hierarchy)
		
		;;then reset the hierarchy each time we're at a "level 1"
		(setf startingAgent thisAgent)
		(setf hierarchy nil) 
		(if thisAgent (setf (contractCode thisAgent) (tagHier-contractCode thisTuple)))
		;;if this was a level 1 from hiearchy but not found among Agents, then skip whole hierachy
		(setf skipToNextLevelOne? (not thisAgent))
		)
	      
	    ;;otherwise, just assign latest constract code and add parent as the next upline
	    (when (and (not skipToNextLevelOne?) thisAgent priorAgent
		       ;; error case if priorAgent is IMA record, if so then skip to next hierarchy set
		       (not (if (eq priorAgent homeOfficeRec)
				(setf skipToNextLevelOne? t))))
	      (if showLinks?
		  (format dumpfile "connecting ~a --> ~a" (AGD-100-AGENT priorAgent) (AGD-100-AGENT thisAgent)))
	      (setf (contractCode thisAgent) (tagHier-contractCode thisTuple))
	      (setf (parent priorAgent) thisAgent)
	      (setf hierarchy (nconc hierarchy (list thisAgent)))))

	  (setf priorAgent thisAgent))
	(if (zerop (mod counter 10000))
	    (princ ","))
      until (or (> counter 999999) (null buffer))
      finally 
	  (write-hierarchy startingAgent hierarchy) 
	  (format dumpfile "~%============ counter=~s hierarchy records completed ============" counter))))


;;; 12-Jun-22 WSD added for parsing the 'official' Dell file and then the exception files
;;; 12-Jun-25 WSD added construction of subagent lists
;;; 13-Aug-12 WSD no longer using "...exceptions_handcoded_by_WSD.csv"
(defun parseHierarchyFiles (&optional (dumpfile t))
  (format dumpfile "~%-------------Processing DELL Spreadsheet Records-------------~%")
  (format t "~%[~a]Loading TAG_HIER_for_parsing.csv" (timestamp (get-universal-time)))
  (with-open-file (dataIn (join inputDocumentPath "TAG_HIER_for_parsing.csv") :direction :input)
    (parseTagHierarchyFile dataIn dumpfile t))
  (format t "~%[~a]Loading TAG_HIER_exceptions_handcoded_by_WSD.csv" (timestamp (get-universal-time)))
  (format dumpfile "~%-------------Processing DELL Spreadsheet Exception Records-------------~%")
  #|(with-open-file (dataIn (join inputDocumentPath "TAG_HIER_exceptions_handcoded_by_WSD.csv") :direction :input)
    (parseTagHierarchyFile dataIn dumpfile))|#
  
  (format dumpfile "~%-------------Constructing subagent lists from hierarchy-------------~%")
  (map-agents 
   #'(lambda (agent)
       (if (parent agent)
	   (pushnew agent (subagents (parent agent)))))))


(defun dumpHierarchyCheck (&optional (dumpPath (join outputDocumentPath "ACT_hier_check.txt")))
  (with-open-file (dumpfile dumpPath :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for hierTuple in *AgentHierarchyTuples*
      as thisAgent = (getAgent (agentHier-agentCode hierTuple))
      as parentAgent = (if thisAgent (parent thisAgent))
      as visited = (list thisAgent parentAgent)
      do
	  (format dumpfile "~%~a ~a, ~a [~a]"
		  (agentHier-firstName hierTuple)
		  (agentHier-lastName hierTuple)
		  (agentHier-Company hierTuple)
		  (agentHier-agentCode hierTuple))
	(unless thisAgent
	  (format dumpfile " *** agentcode ~s is not found among DELL data" (agentHier-agentCode hierTuple)))
	(loop while parentAgent
	    do
	      (format dumpfile " ==> ~a [~a]" (AGD-100-NAME parentAgent) (AGD-100-AGENT parentAgent))
	      (setf parentAgent (parent parentAgent))
	      (if (member parentAgent visited)
		  ;;uh-oh, hierarchy loop -- print loop and stop upward traversal
		  (progn
		    (format dumpfile " ***LOOP*** => ~a [~a]" (AGD-100-NAME parentAgent)  (AGD-100-AGENT parentAgent))
		    (setf parentAgent nil))
		;;otherwise, keep walking tree to null parent terminal
		(push parentAgent visited))))))