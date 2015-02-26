class SOHQuestion(object):
    def __init__(self, label, question):
        self.label = label
        self.question = question

    def __repr__(self):
        return "{}('{}', '{}')".format(self.__class__.__name__, self.label, self.question.replace("'", "\\'"))
    
    def __eq__(self, other):
        return type(self) == type(other) and self.question == other.question and self.label == other.label
    

    #def __hash__(self):
    #    return hash((self.label, self.question))
    
#hospitalized_question = SOHQuestion(label="Hospital 90 days", question='Has any Applicant been hospitalized in the past 90 days?')

class ApplicationForm(object):
    def __init__(self, label, questions):
        self.label = label
        self.questions = questions

    def __repr__(self):
        return "ApplicationForm('{}', {})".format(self.label, '\n'.join([repr(q) for q in self.questions]))

    
FPP_health_questions_by_form = {
    "Generic": [
        "Has any Applicant been hospitalized in the past 90 days?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
        "Has any Applicant ever applied for and been rejected for life insurance?",
    ],
    "WS-UST App R409-CO": [
        "Has any Applicant been hospitalized in the past 90 days?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Have you had or been told by a member of the medical profession that you have AIDS or HIV infection?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-IL": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed, tested, or treated by a physician for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-OH": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-VA": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-PA": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-WI": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed by a physician as having Human Immuno-deficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)? (The applicant need not reveal HIV test results received from an anonymous counseling and testing site or the results of a home test kit.) ",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-FL": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has the Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has the Applicant under this application of coverage tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?",
        "Has any Applicant ever applied for and been rejected for life insurance?"
    ],
    "WS-UST App R409-MO": [
        "Has any Applicant been hospitalized in the past 90 days? ",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
        "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
        "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?"
    ]
}


from collections import defaultdict
unique_questions = defaultdict(list)

applications = []
application_printout = defaultdict(list)
for form, questions in FPP_health_questions_by_form.items():
    soh_questions = []
    app_unique_questions = []
    #print " {}".format(form)
    for q in questions:
        if 'in the past 90 days' in q:
            label = 'Hospital 90 days'
        elif 'stroke' in q:
            label = 'Heart'
        elif 'cancer' in q:
            label = 'Cancer'
        elif 'pulmonary' in q:
            label = 'Respiratory'
        elif 'Alcohol' in q:
            label = 'Liver'
        elif 'HIV' in q or 'AIDS' in q:
            label = 'HIV/AIDS'
        elif 'rejected' in q:
            label = 'Ever been rejected'
        else:
            raise Exception("Could not determine label for {}".format(q))
        
        soh_question = SOHQuestion(label, q)
        if soh_question not in unique_questions[label]:
            unique_questions[label].append(soh_question)

        application_printout[form].append("unique_questions['{}'][{}]".format(label, unique_questions[label].index(soh_question)))
                
        soh_questions.append(soh_question)
        
    app = ApplicationForm(form, soh_questions)
    applications.append(app)
    
from pprint import pprint, pformat
print "unique_questions = {}".format(pformat(dict(unique_questions)))
for form, printout in application_printout.items():
    print """ApplicationForm('{}', [
{}
])""".format(form, ',\n    '.join(printout))
    