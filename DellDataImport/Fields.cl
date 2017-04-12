
;;;-----------------------------------------------------------------------
;;; Tables
;;;

(defparameter *Policies* (make-hash-table :size 150000 :test #'equal))
(defparameter *Splits* (make-hash-table :size 15000 :test #'equal))
;;; 12-Jun-28 WSD added *SupBenefits* - each element in table is a non-empty list of SUPBENEFIT objects
(defparameter *SupBenefits* (make-hash-table :size 75000 :test #'equal))
;;; 13-Nov-14 WSD added *Newbizrecs*
(defparameter *Newbizrecs* (make-hash-table :size 150000 :test #'equal))

(defparameter *Agents* (make-hash-table :size 1000 :test #'equal))
;;; 12-Jul-10 WSD added *Agent200Recs*
(defparameter *Agent200Recs* (make-hash-table :size 1500 :test #'equal))
;;; 12-Jun-20 WSD added *AgentsAlternateIDs*
(defparameter *AgentsAlternateIDs* (make-hash-table :size 1000 :test #'equal))
(defparameter *EmployerGroups* (make-hash-table :size 5000 :test 'equal))
(defparameter *AgentHierarchyTuples* '())
(defparameter *AgentAliases* '())	;sets of tuples with synonymous agentCodes
(defparameter *PlanCodeIndex* nil)
(defparameter *QtyPlanCodesForPeriod* 0)

;;Handles for RMDs / special agent records
(defparameter *HomeOffice* nil)
(defparameter *WestRegion* nil)
(defparameter *GLakesRegion* nil)
(defparameter *CentralRegion* nil)

;;; 14-Feb-19 WSD added - code tired of always looking up which codes are which
;;; 15-Nov-10 WSD  added new plan codes
(defparameter *PlanCodes-FPPTI* '("FPPTI"
				  "FPPTI MD"
				  "FPPTI UT"
				  "FPPTIA"
				  "FPPTIG"
				  "FPPTDG"
				  "FPPTID"
				  "FPPTIDMD"
				  "FPPTIDUT"
				  "FPPTID MD"
				  "FPPTID UT"
				  ;;new plan codes added 2015-11-10
				  "FPPTIW"
				  "FPPTDW"
				  "FPPTIB"
				  "FPPTDB"
				  "FPPTIY"
				  "FPPTDY"
				  ;;;new plan codes with riders 2015-11-10
				  "FPATI"
				  "FPATI/MD"
				  "FPATI/UT"
				  "FPATW"
				  "FPATB"
				  "FPATY"
				  "FPQTI3"
				  "FPQTI3/MD"
				  "FPQTI3/UT"
				  "FPQTIG/3"
				  "FPQTIW/3"
				  "FPQTIB/3"
				  "FPQTIY/3"
				  "FPQTI4"
				  "FPQTI4/MD"
				  "FPQTI4/UT"
				  "FPQTIG/4"
				  "FPQTIW/4"
				  "FPQTIB/4"
				  "FPQTIY/4"

				  "FPQTIW4"
				  "FPQTIW3"
				  "FPQTIG4"
				  "FPQTIG3"

				  "FPATI4"
				  "FPATI4/MD"
				  "FPATI4/UT"
				  "FPATW/4"
				  "FPATB/4"
				  "FPATY/4"
				  ))
(defparameter *PlanCodes-FPPCI* '("INDFPP"
				  "INDFPP UT"
				  "INDFPP MD"
				  "INDFPPUT"
				  "INDFPPMD"
				  "INDFPD"
				  "INDFPD UT"
				  "INDFPD MD"
				  "INDFPDUT"
				  "INDFPDMD"
				  "FPPCI"
				  "FPPCID"

				  ;;legacy codes
				  "INDFSD"
				  "INDFSPUT"
				  "FPNOCI"
				  "FPNOCD"
				  
				  "INDFSP")) ;; INDFSP PROBABLY TYPO

;;; 15-Nov-10 WSD added
(defparameter *PlanCodes-HI* '("HIL01" 
			       "HIL01 EC"
			       "HIL01 ES"
			       "HIL01 EF"
			       "HIM01"
			       "HIM01 EC"
			       "HIM01 ES"
			       "HIM01 EF"
			       "HIH01"
			       "HIH01 EC"
			       "HIH01 ES"
			       "HIH01 EF"

			       ;;
			       ;; HI Plan riders
			       ;; 2016-11-02
			       "MULTI" ;Multiplan
			       "VHC   B" ;Teledoc Basic
			       "VHC    B" ;(alternate formatting)
			       "VHC   BD" ;Teledoc Basic + Dental
			       "VHC   BV" ;Teledoc Basic + Vision
			       "VHC   DV" ;Teledoc Dental + Vision
			       
			       ))


;;; 16-Aug-17 WSD added
(defparameter *PlanCodes-ACC* '("ACL24"	; accident pols, 6 levels + 4 tiers
				"ACL24 EC"
				"ACL24 ES"
				"ACL24 EF"
				"ACM24"
				"ACM24 EC"
				"ACM24 ES"
				"ACM24 EF"
				"ACH24"
				"ACH24 EC"
				"ACH24 ES"
				"ACH24 EF"
				"ACLOF"
				"ACLOF EC"
				"ACLOF ES"
				"ACLOF EF"
				"ACMOF"
				"ACMOF EC"
				"ACMOF ES"
				"ACMOF EF"
				"ACHOF"
				"ACHOF EC"
				"ACHOF ES"
				"ACHOF EF"
				
				
				"DAD" ; AD&D rider for ACC
				"DAD EC"
				"DAD ES"
				"DAD EF"

				"DISAA" ; Disability riders for ACC
				"DISAA ES"
				"DISAA EF"
				"DISAB"
				"DISAB ES"
				"DISAB EF"
				"DISAC"
				"DISAC ES"
				"DISAC EF"
				"DISBA"
				"DISBA ES"
				"DISBA EF"
				"DISBB"
				"DISBB ES"
				"DISBB EF"
				"DISBC"
				"DISBC ES"
				"DISBC EF"
				"DISCA"
				"DISCA ES"
				"DISCA EF"
				"DISCB"
				"DISCB ES"
				"DISCB EF"
				"DISCC"
				"DISCC ES"
				"DISCC EF"))




(defparameter *PlanCodes-CritIllness* '("CILEGA" "CRTIL" "CRTIL CH" "CRTIL SP" "CRTILE" "CRTILESP" "CRTILA") )
(defparameter *PlanCodes-GroupLife* '("BAS01"
				      "BAS01 DP"
				      "BAS02"
				      "BAS02 D"
				      "BAS02 F"
				      "ADDB"
				      "ADDB D"
				      "ADDB F"
				      "ADDB SU"
				      "ADD2"
				      "VOL01"
				      "VOL01 CH"
				      "VOL01 SP"
				      "VOL02"
				      "VOL02 CH"
				      "VOL02 SP"
				      "ADD"
				      "ADDF"
				      "ADDV"
				      "ADDV SP"
				      "ADDV  SP" ;typo in dataset!
				      "ADDV CH"
				      ;;legacy codes
				      "BAS01 SP"
				      "BAS01 CH"
				      
				      ))
(defparameter *PlanCodes-BasicGL* '("BAS01"
				    "BAS01 DP"
				    "BAS02"
				    "BAS02 D"
				    "BAS02 F"
				    "ADDB"
				    "ADDB D"
				    "ADDB F"
				    "ADDB SU"
				    "ADD2"
				    ;;legacy codes
				    "BAS01 SP"
				    "BAS01 CH"))
(defparameter *PlanCodes-VolGL* '("VOL01"
				  "VOL01 CH"
				  "VOL01 SP"
				  "VOL02"
				  "VOL02 CH"
				  "VOL02 SP"
				  "ADD"
				  "ADDF"
				  "ADDV"
				  "ADDV SP"
				  "ADDV  SP"
				  "ADDV CH"))
(defparameter *PlanCodes-ADD* '("ADDB"
				"ADDB D"
				"ADDB F"
				"ADDB SU"
				"ADD2"
				"ADD"
				"ADDF"
				"ADDV"
				"ADDV SP"
				"ADDV  SP"
				"ADDV CH"))

;;; 15-Nov-10 WSD added to cleanup codes above
(defparameter *PlanCodes-Other* '("CMNCR" ; Common Carrier
				  "CHED" ; CHILD EDUCATION BENEFIT
				  "EXPDSP"  ; EXPOSURE & DISAPPEARANCE BNFT
				  ))



(defparameter *policyParsingControlList*
    '((EX-POLICY            3 10 :string)
      (EX-SEG-ID            13 2 :string)
      (EX-INS-NAME          15 30 :string)
      (EX-INS-SEX           45 1 :string)
      (EX-INS-DOB           46 8 :yyyymmdd)
      (EX-PAYER-NAME        54 30 :string)
      (EX-PAYER-ADDR1       84 30 :string)
      (EX-PAYER-ADDR2       114 30 :string)
      (EX-PAYER-CITY        144 26 :string)
      (EX-PAYER-STATE       170 2 :string)
      (EX-PAYER-ZIP         172 9 :string)
      (EX-DUE-DATE          186 8 :yyyymmdd)
      (EX-PAYCODE           194 2 :string)
      (EX-MODE              196 2 :string)
      (EX-BILL-KEYPOL       198 12 :string)
      (EX-SUSPENSE-AMT      210 11 :number)
      (EX-LAST-BILLDATE     225 8 :yyyymmdd)
      (EX-PLAN-AND-OPT      265 8 :string)
      (EX-ISSAGE            273 4 :number)
      (EX-ISSUE-DATE        277 8 :yyyymmdd)
      (EX-STATUS            285 1 :string)
      (EX-STATUS-DATE       286 8 :yyyymmdd)
      (EX-LIFE-ANNUAL-PREM   332 11 :number)
      (EX-FACE-AMOUNT        343 10 :number)
      (EX-BASIC-PREM         353 11 :number)
      (EX-WRITING-AGENT1     369 6 :string)
      (EX-SBCNT1             375 4 :number)
      (EX-SPLIT-PCT1         379 5 :number)
      (EX-WRITING-AGENT2     384 6 :string)
      (EX-SBCNT2             390 4 :number)))


;;; 14-Feb-18 WSD added to support EmployerGroups
;;; 14-Feb-20 WSD added DEM-ALT-STATE to support situsState
(defparameter *employerMasterParsingControlList*
    '((DEM-ID	         3	5 :string)
      (DEM-ALTERNATE-REPORTING-ID	8	20	:string)
      (DEM-CO-NAME	28	30 :string)
      (DEM-ATTN	58	30	:string)
      (DEM-ADDR1	88	30 :string)
      (DEM-ADDR2	118	30 :string)
      (DEM-CITY	148	26	:string)
      (DEM-STATE	174	2 :string)
      (DEM-ZIP-CODE	176	5 :string)
      (DEM-ALT-STATE	406	2 :string)
      (DEM-GRP-BEG-YYYY	522	4 :number)
      (DEM-GRP-BEG-MM	526	2 :number)
      (DEM-GRP-BEG-DD	528	2 :number)
      (DEM-BILL-YYYY	530	4 :number)
      (DEM-BILL-MM	534	2 :number)
      (DEM-BILL-DD	536	2 :number)
      (DEM-PLN-BEG-YYYY	538	4 :number)
      (DEM-PLN-BEG-MM	542	2 :number)
      (DEM-PLN-BEG-DD	544	2 :number)
      (DEM-PLAN-TYPE	546	1 :string)
      (DEM-BILL-MODE	547	2 :string)
      (DEM-STATUS	778	1 :string)
      (DEM-STATUS-YYYY	779	4 :number)
      (DEM-STATUS-MM	783	2 :number)
      (DEM-STATUS-DD	785	2 :number)
      (DEM-REGION-ID   1078	5 :string)
      ))

;;; 14-Feb-18 WSD added to support EmployerGroups
(defparameter *policyPayrollParsingControlList* 
    ;;for now, just using to match-up policy number and group ID
    '((EX-PR-POLICY     	3	10 :string)
      (EX-PR-EMPLOYERID 	15	5  :string)))

;;; 16-Mar-17 WSD added to support EE SSN parsing
(defparameter *policyInsuredParsingControlList* 
    ;;for now, just using to match-up policy number and SSN
    '((EX-IN-POLICY     	3	10 :string)
      (EX-IN-SSN   	      128	9  :string)))

;;; 12-Jun-27 WSD added
(defparameter *policyCoAgentParsingControlList*
    '((EX-CO-POLICY   3 10 :string)
      (EX-CO-SPLIT2   15 5 :number)
      (EX-CO-WAGT3    20 6 :string)
      (EX-CO-SPLIT3   30 5 :number)
      (EX-CO-WAGT4    35 6 :string)))

;;; 12-Jun-28 WSD added
(defparameter *policySupBenefitParsingControlList*
    '((EX-SB-POLICY      3 10  :string)
      (EX-SB-TYPE       15  4  :string)
      (EX-SB-PREM       19 11  :number)
      (EX-SB-BEN        30 11  :number)
      (EX-SB-BEGIN-DATE 41  8  :yyyymmdd)
      (EX-SB-END-DATE   49  8  :yyyymmdd)))

;;; 13-Nov-14 WSD added
(defparameter *policyNewBusinessParsingControlList*
    '((EX-NB-POLICY      3 10  :string)
      (EX-NB-BRIDGE-DATE 32  8  :yyyymmdd)
      (EX-NB-RECEIVED-DATE 48  8  :yyyymmdd)))

;;; 12-May-02 WSD added
;;; 14-May-22 WSD added email-addr
(defparameter *agentMaster100ParsingControlList*
    '((AGD-100-AGENT     3     6     :string)
      (AGD-100-TYPE     9     3     :string)
      (AGD-100-EFF-DATE     12     8     :yyyymmdd)
      (AGD-100-SUBCNT     20     4     :string)
      (AGD-100-SSAN     24     9     :string)
      (AGD-100-NAME     33     30     :string)
      (AGD-100-BUS-ADDR-1     63     30     :string)
      (AGD-100-BUS-ADDR-2     93     30     :string)
      (AGD-100-BUS-CITY     123     26     :string)
      (AGD-100-BUS-TERR-STATE     149     6     :string)
      (AGD-100-BUS-ZIP     155     9     :string)
      (AGD-100-BUS-COUNTRY     164     6     :string)
      (AGD-100-HOME-ADDR-1     170     30     :string)
      (AGD-100-HOME-ADDR-2     200     30     :string)
      (AGD-100-HOME-CITY     230     26     :string)
      (AGD-100-HOME-TERR-STATE     256     6     :string)
      (AGD-100-HOME-ZIP     262     9     :string)
      (AGD-100-HOME-COUNTRY     271     6     :string)
      (AGD-100-BUS-PHONE     277     10     :string)
      (AGD-100-HOME-PHONE     287     10     :string)
      (AGD-100-SEX     297     1     :string)
      (AGD-100-STATUS     298     1     :string)
      (AGD-100-AGENT-DATE-OF-BIRTH     299     8     :yyyymmdd)
      (AGD-100-DATE-HIRED     307     8     :yyyymmdd)
      (AGD-100-DATE-TERMINATED     315     8     :yyyymmdd)
      (AGD-100-ALTERNATE-ID    323     12     :string)
      (AGD-100-EMAIL-ADDR     494     50     :string)))

;;; 12-Jul-10 WSD added
(defparameter *agentMaster200ParsingControlList* 
    '((AGD-200-AGENT	        3	6	:string)
      (AGD-200-EFF-DATE	        12	8	:string)
      (AGD-200-SUBCNT	        20	4	:number)
      (AGD-200-HIER-COUNT	24	4	:number)
      (AGD-200-FILLER-AGENT	29	6	:string)
      (AGD-200-FILLER-SUBCNT    35	4	:number)
      (AGD-200-RPT-LEVEL        39	2	:string)
      (AGD-200-CONT	        41	5	:string)
      (AGD-200-HT-AGENT-1	55	6	:string)
      (AGD-200-HT-SUBCNT-1	61	4	:number)
      (AGD-200-HT-CONT-1	67	5	:string)
      (AGD-200-HT-AGENT-2	81	6	:string)
      (AGD-200-HT-SUBCNT-2	87	4	:number)
      (AGD-200-HT-CONT-2	93	5	:string)
      (AGD-200-HT-AGENT-3	107	6	:string)
      (AGD-200-HT-SUBCNT-3	113	4	:number)
      (AGD-200-HT-CONT-3	119	5	:string)
      (AGD-200-HT-AGENT-4	133	6	:string)
      (AGD-200-HT-SUBCNT-4	139	4	:number)
      (AGD-200-HT-CONT-4	145	5	:string)
      (AGD-200-HT-AGENT-5	159	6	:string)
      (AGD-200-HT-SUBCNT-5	165	4	:number)
      (AGD-200-HT-CONT-5	171	5	:string)
      (AGD-200-HT-AGENT-6	185	6	:string)
      (AGD-200-HT-SUBCNT-6	191	4	:number)
      (AGD-200-HT-CONT-6	197	5	:string)
      (AGD-200-HT-AGENT-7	211	6	:string)
      (AGD-200-HT-SUBCNT-7	217	4	:number)
      (AGD-200-HT-CONT-7	223	5	:string)))


;;bad agents we never want to include in production totals
;;
;;; 13-Jul-02 WSD added 26AKL BJ Spyksma to exclusions of counts
;;; 13-Jul-02 WSD added 26BJR Kerry Day (sub of Spyksma)
(defparameter *agentCodesExcludedFromCalculations* '("26AVB" "26Y3P" "26ARP" "26AKL" "26BJR"))
