
from dateutil.parser import parse as dateutil_parse
from dateutil.relativedelta import *
from datetime import date

# TODO: Don't rely on user here, assumes it is the agent
from flask.ext.stormpath import user

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_template_id

# Generic replacement form template ID: 3E0CF882-8678-4476-A6B3-D60AA4111C85

class FPPTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["agent_data"]["state"]
        template_id = get_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients)

        self.data = enrollment_data

    def generate_tabs(self, recipient):

        if not recipient.is_employee():
            return {}

        tabs = {}

        self.add_employer_tabs(tabs)
        self.add_employee_tabs(tabs)
        self.add_spouse_tabs(tabs)
        self.add_coverage_tabs(tabs)
        self.make_all_beneficiary_tabs(tabs)

        self.add_general_tabs(tabs)


        return tabs

    def add_general_tabs(self, ds_tabs):

        tabs = [

            DocuSignRadioTab('productType', "FPPTI" if self.data['product_type'] == "FPP-Gov" else self.data['product_type']),
            DocuSignRadioTab('existingIns', self.data["existing_insurance"]),
            DocuSignRadioTab('replace', self.data["replacing_insurance"]),
        ]

        for (prefix_short, prefix_long) in {("ee", "employee"), ("sp", "spouse")}:

            tabs.append(DocuSignRadioTab(prefix_short + "Gender", self.data[prefix_long]["gender"]))

            if self.data[prefix_long] and "is_smoker" in self.data[prefix_long]:
                smoker_button = "smoker" if self.data[prefix_long]["is_smoker"] else "nonsmoker"
                tabs.append(DocuSignRadioTab(prefix_short + "Smoking", smoker_button))

            # only include Owner checkbox if coverage was selected
            if ((prefix_short == "ee" and self.data.did_employee_select_coverage()) or
                (prefix_short == "sp" and self.data.did_spouse_select_coverage())):
                tabs.append(DocuSignRadioTab(prefix_short + "Owner", self.data[prefix_long + "_owner"]))

        for tab in tabs:
            tab.add_to_tabs(ds_tabs)

    def add_employer_tabs(self, tabs):
        emp_tabs = [
            DocuSignTextTab('employer', self.data.get_employer_name()),
            DocuSignTextTab('group_number', self.data['agent_data']['group_number'] if self.data['agent_data']['group_number'] else "")
        ]

        for tab in emp_tabs:
            tab.add_to_tabs(tabs)

    def add_employee_tabs(self, ds_tabs):

        ee_tabs_list = []
        ee_tabs_list += self.make_applicant_tabs(prefix="ee", data=self.data['employee'])
        ee_tabs_list += self.make_contact_tabs('ee', self.data["employee"])
        ee_tabs_list += self.make_payment_mode_tabs()
        ee_tabs_list += self.make_generic_tabs()

        if self.data.did_employee_select_coverage():
            ee_tabs_list += self.generate_SOH_tabs("ee", self.data['employee']['soh_questions'])
            ee_tabs_list += self.generate_SOH_GI_tabs("ee", self.data['employee']['soh_questions'])

        for tab in ee_tabs_list:
            tab.add_to_tabs(ds_tabs)



    def add_spouse_tabs(self, ds_tabs):
        sp_tabs_list = []

        sp_tabs_list += self.make_applicant_tabs(prefix="sp", data=self.data['spouse'])
        #sp_tabs_list += self.make_contact_tabs('sp', self.data["spouse"])

        if self.data.did_spouse_select_coverage():
            sp_tabs_list += self.generate_SOH_tabs("sp", self.data['spouse']['soh_questions'])
            sp_tabs_list += self.generate_SOH_GI_tabs("sp", self.data['spouse']['soh_questions'])

        for tab in sp_tabs_list:
            tab.add_to_tabs(ds_tabs)


    def add_coverage_tabs(self, ds_tabs):
        coverage_tabs = []

        # Employee
        employee_coverage = self.data.get_employee_coverage() if self.data.did_employee_select_coverage() else 'NONE'
        ee_premium = self.data.get_employee_premium() if self.data.did_employee_select_coverage() else ''
        coverage_tabs += [
            DocuSignTextTab('eeCoverage', employee_coverage),
            DocuSignTextTab('eePremium', ee_premium),
        ]

        spouse_coverage = self.data.get_spouse_coverage() if self.data.did_spouse_select_coverage() else 'NONE'
        spouse_premium = self.data.get_spouse_premium() if self.data.did_spouse_select_coverage() else ''
        coverage_tabs += [
            DocuSignTextTab('spCoverage', spouse_coverage),
            DocuSignTextTab('spPremium', spouse_premium),
        ]

    def make_all_beneficiary_tabs(self, ds_tabs):
        tabs = (
            self.make_applicant_beneficiary_tabs("ee", "employee") +
            self.make_applicant_beneficiary_tabs("sp", "spouse")
        )
        for tab in tabs:
            tab.add_to_tabs(ds_tabs)

    def make_applicant_beneficiary_tabs(self, short_prefix, long_prefix):
        tabs = []
        if ((long_prefix == "employee" and not self.data.did_employee_select_coverage()) or
            (long_prefix == "spouse" and not self.data.did_spouse_select_coverage())):
            return tabs

        tabs += self.make_old_style_beneficiary_tabs(short_prefix, long_prefix)

        # Contingent
        contingent_key = '{}_contingent_beneficiary'.format(long_prefix)
        if contingent_key in self.data and self.data[contingent_key]:
            beneficiary_data = self.data[contingent_key]
            tabs += self.make_beneficiary_tabs(prefix='{}Cont'.format(short_prefix),
                                       name=beneficiary_data['name'],
                                       relationship=beneficiary_data['relationship'],
                                       ssn=beneficiary_data['ssn'],
                                       dob=beneficiary_data['date_of_birth']
            )

        return tabs

    def make_spouse_beneficiary_tabs(self):
        tabs = []

        if not self.data.did_spouse_select_coverage():
            return tabs

        tabs += self.make_old_style_beneficiary_tabs("sp", "employee")

        # Contingent
        if 'employee_contingent_beneficiary' in self.data:
            bene_data = self.data['employee_contingent_beneficiary']
            tabs += self.make_beneficiary_tabs(prefix='eeCont',
                                       name=bene_data['name'],
                                       relationship=bene_data['relationship'],
                                       ssn=bene_data['ssn'],
                                       dob=bene_data['date_of_birth']
            )

        return tabs

    def make_old_style_beneficiary_tabs(self, short_prefix, long_prefix):

        if self.data["{}_beneficiary".format(long_prefix)] == "spouse":
            spouse_data = self.data["employee"] if long_prefix == "spouse" else self.data["spouse"]
            return self.make_beneficiary_tabs(
                prefix=short_prefix,
                name=spouse_data["first"] + " " + spouse_data["last"],
                relationship="spouse",
                dob=spouse_data["birthdate"],
                ssn=spouse_data["ssn"],
            )
        else:
            return self.make_beneficiary_tabs(
                prefix = short_prefix,
                name = self.data["{}_beneficiary_name".format(long_prefix)],
                relationship = self.data["{}_beneficiary_relationship".format(long_prefix)],
                dob = self.data["{}_beneficiary_dob".format(long_prefix)],
                ssn = self.data["{}_beneficiary_ssn".format(long_prefix)],
            )

    def make_payment_mode_tabs(self):
        return [
            DocuSignRadioTab(group_name='payment_mode', value=self.data['payment_mode'], is_selected=True)
        ]

    def make_generic_tabs(self):

        ee_email_part_1, ee_email_part_2 = self.data.get_employee_email_parts()

        agent_code = 'TESTCODE'#user.custom_data["agent_code"]
        agent_signing_name = 'SIGNING NAME' # user.custom_data["signing_name"]

        return [
            DocuSignTextTab('eeEnrollCityState', self.data["enrollCity"] + ", " + self.data["enrollState"]),
            DocuSignTextTab('date_of_hire', self.data['identityToken']),
            DocuSignTextTab('agentCode', agent_code),
            DocuSignTextTab('agentSignName', agent_signing_name),
            DocuSignTextTab('Employer', self.data["agent_data"]["company_name"]),
            DocuSignTextTab('eeOtherOwnerName', self.data["employee_other_owner_name"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('eeOtherOwnerName2', self.data["employee_other_owner_name"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('eeOtherOwnerSSN', self.data["employee_other_owner_ssn"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('eeEmailPart1', ee_email_part_1),
            DocuSignTextTab('eeEmailPart2', ee_email_part_2),
            DocuSignRadioTab('actively_at_work', "yes" if self.data['is_employee_actively_at_work'] else "no"),
        ]


    def make_contact_tabs(self, prefix, data):
        return [
            DocuSignTextTab(prefix+'Address', data['address1'] + " " + data['address2'] if 'address2' in data else ''),
            DocuSignTextTab(prefix + 'City', data['city']),
            DocuSignTextTab(prefix + 'State', data['state']),
            DocuSignTextTab(prefix + 'Zip', data['zip']),
            DocuSignTextTab(prefix + 'Phone', data['phone']),
            DocuSignTextTab(prefix + 'Email', data['email']),
        ]

    def make_beneficiary_tabs(self, prefix, name, relationship, dob, ssn):

        return [
            DocuSignTextTab(prefix+"BeneFullName", name),
            DocuSignTextTab(prefix+"BeneAge", self.get_age_from_dob(dob)),
            DocuSignTextTab(prefix+"BeneRelationship", relationship),
            DocuSignTextTab(prefix+"BeneDOB", dob),
            DocuSignTextTab(prefix+"BeneSSN", ssn),
        ]

    def get_age_from_dob(self, dob):
        if dob and dateutil_parse(dob):
            return "%d"%relativedelta(date.today(), dateutil_parse(dob)).years
        else:
            return ""

    def generate_SOH_tabs(self, prefix, soh_questions):
        "Statement of health radio button answers"

        radio_tabs = []
        for i, soh_question in enumerate(soh_questions):
            if soh_question['answer'] and soh_question['answer'].upper() == "GI":
                # GI - skip for now
                selected = "False"
                answer = "GI"
            else:
                selected = "True"
                answer = "no"

            radio_tabs.append(DocuSignRadioTab(prefix + "SOH" + str(i+1), answer, selected))

        return radio_tabs

    def generate_SOH_GI_tabs(self, prefix, soh_questions):
        tabs = []
        for i, soh_question in enumerate(soh_questions):
            if soh_question['answer'] and soh_question['answer'].upper() == "GI":
                # GI - skip for now
                answer = "GI"
            else:
                answer = ""

            tabs.append(DocuSignTextTab("{prefix}SOH{i}gi".format(prefix=prefix, i=i+1), answer))

        return tabs

    def make_applicant_tabs(self, prefix, data):
        tabs = [
            DocuSignTextTab(prefix + 'Name', data["first"] + " " + data["last"]),
            DocuSignTextTab(prefix + 'FName', data["first"]),
            DocuSignTextTab(prefix + 'LName', data["last"]),
            DocuSignTextTab(prefix + 'DOB', data["birthdate"]),
            DocuSignTextTab(prefix + 'SSN', data["ssn"]),
        ]

        if data.get('height'):
            height_ft = "%s" % int(data['height'] / 12.0)
            height_in = "%s" % int(data['height'] % 12.0)

            tabs += [
                DocuSignTextTab(prefix + 'HeightFt', height_ft),
                DocuSignTextTab(prefix + 'HeightIn', height_in),
            ]
        if data.get('weight'):
            tabs += [DocuSignTextTab(prefix + 'Weight', data['weight'])]

        return tabs

    # [
    #     DocuSignTextTab('Employer', ), # Company Name
    #
    #     'employee_name',
    #     'eeSSN',
    #     'eeCoverage',
    #     'eePremium',
    #
    # ]
    # +'date_of_hire',
    # +'group_number',
    # +'actively_at_work', # radio: yes, no
    # +'payment_mode', # radio
    #
    # +'eeBeneAge',
    # +'eeContBeneFullName',
    # +'eeContBeneRelationship',
    # +'eeContBeneSSN',
    # +'eeContBeneAge',
    # +'eeContBeneDOB',
    #
    #
    #
    # 'spouse_hospital_six_months', # Radio yes no
    # 'spouse_disability_six_months', # Radio yes no
    #
    #
    # 'child1Name' # not fname, lname
    #
    # 'totalChildPremium',
    # 'totalAllPremium',
    #
    #
    # # Replacement Form
    # 'read_aloud', # yes, no
    # 'considering_terminating_existing', # yes, no
    # 'considering_using_funds', # yes, no
    #
    # 'policy_insurer_name',
    # 'policy_number',
    # 'policy_insured',
    # 'policy_replaced_or_financing', # R or F
    # 'policy_reason',
    # 'eeName',


if __name__ == "__main__":
    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, get_docusign_transport, create_envelope
    from taa.services.docusign.docusign_envelope import build_callback_url, EnrollmentDataWrap

    # Sample pulled from site
    true = True
    false = False
    null = None
    wizard_data = {"health_questions": [
        {"label": "Hospital 90 days", "question_text": "Has any Applicant been hospitalized in the past 90 days?",
         "skip_if_coverage_at_most": null}, {"label": "Heart",
                                             "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
                                             "skip_if_coverage_at_most": null}, {"label": "Cancer",
                                                                                 "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                                                                                 "skip_if_coverage_at_most": null},
        {"label": "Respiratory",
         "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
         "skip_if_coverage_at_most": null}, {"label": "Liver",
                                             "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                                             "skip_if_coverage_at_most": null}, {"label": "HIV/AIDS",
                                                                                 "question_text": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                                                                                 "skip_if_coverage_at_most": null},
        {"label": "Ever been rejected",
         "question_text": "Has any Applicant ever applied for and been rejected for life insurance?",
         "skip_if_coverage_at_most": null}], "agent_data": {
    "children_data": [{"birthdate": "03/29/1995", "existing_coverages": [], "first": "Anna", "last": "Roberts"},
                      {"birthdate": "02/13/2007", "existing_coverages": [], "first": "Kelly", "last": "Woods"}],
    "company_name": "ABC",
    "employee_data": {"birthdate": "04/05/1955", "city": "Naples", "email": "kwoods25w@chron.com",
                      "existing_coverages": [], "first": "Katherine", "gender": "female", "height": null,
                      "is_smoker": null, "last": "Adams", "phone": "4-(566)054-7761", "ssn": "287288500", "state": "FL",
                      "street_address": "23 Washington Crossing", "street_address2": "", "weight": null,
                      "zip": "34114"}, "enroll_city": "Indianapolis", "group_number": null, "health_questions": {"2": [
        {"label": "Hospital 90 days", "question_text": "Has any Applicant been hospitalized in the past 90 days?",
         "skip_if_coverage_at_most": null}, {"label": "Heart",
                                             "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
                                             "skip_if_coverage_at_most": null}, {"label": "Cancer",
                                                                                 "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                                                                                 "skip_if_coverage_at_most": null},
        {"label": "Respiratory",
         "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
         "skip_if_coverage_at_most": null}, {"label": "Liver",
                                             "question_text": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                                             "skip_if_coverage_at_most": null}, {"label": "HIV/AIDS",
                                                                                 "question_text": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                                                                                 "skip_if_coverage_at_most": null},
        {"label": "Ever been rejected",
         "question_text": "Has any Applicant ever applied for and been rejected for life insurance?",
         "skip_if_coverage_at_most": null}]}, "is_in_person": true, "payment_mode": null,
    "payment_mode_choices": [{"immutable": true, "mode": 52, "name": "Weekly"},
                             {"immutable": true, "mode": 26, "name": "Biweekly"},
                             {"immutable": true, "mode": 24, "name": "Semimonthly"},
                             {"immutable": true, "mode": 12, "name": "Monthly"},
                             {"immutable": false, "mode": -1, "name": "Leave For Applicant To Select"}], "products": [
        {"base_product_type": "FPPCI", "bypassed_soh_questions": [], "code": "FPPCI", "gi_criteria": [], "id": 2,
         "is_guaranteed_issue": false, "name": "Family Protection Plan - Critical Illness", "product_type": "base",
         "restricted_agents": [], "visible_to_agents": true}],
    "spouse_data": {"birthdate": "05/07/1985", "city": "Lexington", "email": "kwoods25w@cornell.edu",
                    "existing_coverages": [], "first": "Joan", "gender": "female", "height": null, "is_smoker": null,
                    "last": "Ray", "phone": "3-(608)259-3463", "ssn": "387500865", "state": "KY",
                    "street_address": "993 Lien Center", "street_address2": "54800 Grim Parkway", "weight": null,
                    "zip": "40591"}, "state": "IN"},
        "enrollCity": "Indianapolis",
        "enrollState": "IN",
                   "product_type": "FPPCI", "payment_mode": "biweekly", "method": "in_person", "did_decline": false,
                   "identityToken": "12/12/2014", "identityType": "",
                   "employee": {"first": "Katherine", "last": "Adams", "email": "kwoods25w@chron.com", "age": 60,
                                "weight": null, "height": null, "is_smoker": null, "birthdate": "04/05/1955",
                                "ssn": "287288500", "gender": "female", "phone": "4-(566)054-7761",
                                "address1": "23 Washington Crossing", "address2": "", "city": "Naples", "state": "FL",
                                "zip": "34114", "soh_questions": [
                       {"question": "Has any Applicant been hospitalized in the past 90 days?", "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                       "answer": "No"}, {
                       "question": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                       "answer": "No"},
                       {"question": "Has any Applicant ever applied for and been rejected for life insurance?",
                        "answer": "No"}]},
                   "spouse": {"first": "Joan", "last": "Ray", "email": "kwoods25w@cornell.edu", "age": 30,
                              "weight": null, "height": null, "is_smoker": null, "birthdate": "05/07/1985",
                              "ssn": "387500865", "gender": "female", "phone": "3-(608)259-3463",
                              "address1": "993 Lien Center", "address2": "54800 Grim Parkway", "city": "Lexington",
                              "state": "KY", "zip": "40591", "soh_questions": [
                       {"question": "Has any Applicant been hospitalized in the past 90 days?", "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
                       "answer": "No"}, {
                       "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                       "answer": "No"}, {
                       "question": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                       "answer": "No"},
                       {"question": "Has any Applicant ever applied for and been rejected for life insurance?",
                        "answer": "No"}]}, "existing_insurance": false, "replacing_insurance": false,
                   "is_employee_actively_at_work": false, "has_spouse_been_treated_6_months": false,
                   "has_spouse_been_disabled_6_months": false, "employee_owner": "self",
                   "employee_other_owner_name": "", "employee_other_owner_ssn": "", "spouse_owner": "employee",
                   "spouse_other_owner_name": "", "spouse_other_owner_ssn": "", "employee_beneficiary": "other",
                   "spouse_beneficiary": "spouse", "employee_contingent_beneficiary_type": "spouse",
                   "employee_contingent_beneficiary": {}, "employee_beneficiary_name": "Little John",
                   "employee_beneficiary_relationship": "Friend", "employee_beneficiary_ssn": "123-12-1234",
                   "employee_beneficiary_dob": "10/10/1980", "spouse_contingent_beneficiary_type": "other",
                   "spouse_contingent_beneficiary": {"name": "John Smith", "relationship": "Cousin",
                                                     "ssn": "098-09-8098", "date_of_birth": "09/09/1995"}, "children": [
        {"first": "Anna", "last": "Roberts", "email": "", "age": 20, "weight": null, "height": null, "is_smoker": null,
         "birthdate": "03/29/1995", "ssn": "123-12-1234", "gender": "female", "phone": "", "address1": "",
         "address2": "", "city": "", "state": "", "zip": "",
         "soh_questions": [{"question": "Has any Applicant been hospitalized in the past 90 days?", "answer": "No"}, {
         "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
         "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                           "answer": "No"}, {
                           "question": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                           "answer": "No"},
                           {"question": "Has any Applicant ever applied for and been rejected for life insurance?",
                            "answer": "No"}]},
        {"first": "Kelly", "last": "Woods", "email": "", "age": 8, "weight": null, "height": null, "is_smoker": null,
         "birthdate": "02/13/2007", "ssn": "", "gender": "female", "phone": "", "address1": "", "address2": "",
         "city": "", "state": "", "zip": "",
         "soh_questions": [{"question": "Has any Applicant been hospitalized in the past 90 days?", "answer": "No"}, {
         "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
         "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                           "answer": "No"}, {
                           "question": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                           "answer": "No"},
                           {"question": "Has any Applicant ever applied for and been rejected for life insurance?",
                            "answer": "No"}]},
        {"first": "Third", "last": "Child", "email": "", "age": 4, "weight": null, "height": null, "is_smoker": null,
         "birthdate": "12/28/2010", "ssn": "", "gender": null, "phone": "", "address1": "", "address2": "", "city": "",
         "state": "", "zip": "",
         "soh_questions": [{"question": "Has any Applicant been hospitalized in the past 90 days?", "answer": "No"}, {
         "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?",
         "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?",
                           "answer": "No"}, {
                           "question": "In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?",
                           "answer": "No"}, {
                           "question": "Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?",
                           "answer": "No"},
                           {"question": "Has any Applicant ever applied for and been rejected for life insurance?",
                            "answer": "No"}]}],
                   "child_coverages": [{"premium": 2.3, "face_value": 10000}, {"premium": 2.3, "face_value": 10000},
                                       {"premium": 2.3, "face_value": 10000}],
                   "employee_coverage": {"premium": 35.3, "face_value": 25000},
                   "spouse_coverage": {"premium": 4, "face_value": 10000},
                   "product_data": {"base_product_type": "FPPCI", "bypassed_soh_questions": [], "code": "FPPCI",
                                    "gi_criteria": [], "id": 2, "is_guaranteed_issue": false,
                                    "name": "Family Protection Plan - Critical Illness", "product_type": "base",
                                    "restricted_agents": [], "visible_to_agents": true}}
    wrap_data = EnrollmentDataWrap(wizard_data, None)

    owner_agent = "Agent Mason" # census_record.case.owner_agent
    agent = AgentDocuSignRecipient(name=owner_agent, email="agent@zachmason.com")
    employee = EmployeeDocuSignRecipient(name=wrap_data.get_employee_name(),
                                         email="zach@zachmason.com")
    recipients = [
        agent,
        employee,
        # TODO Check if BCC's needed here
    ]

    #child_attachment_form = ChildAttachmentForm(test_recipients)
    #child_attachment_form.add_child("Joe", "Johnson", child_dob="12/01/2010", child_ssn='123-12-1234', child_soh_answers=[])
    #child_attachment_form.add_child("Susie", "Johnson", child_dob="12/01/2012", child_ssn='123-12-3234', child_soh_answers=[])
    #child_attachment_form.add_child("Christy", "Johnson", child_dob="12/01/2014", child_ssn='223-12-3234', child_soh_answers=[])

    # Use generic template for now
    general_template = FPPTemplate(recipients, wrap_data)

    transport = get_docusign_transport()
    envelope_result = create_envelope(email_subject="Signature needed",
                                      components=[
                                          general_template,
                                          #child_attachment_form
                                      ],
                                      docusign_transport=transport,
                                     )

    redirect_url = envelope_result.get_signing_url(
        employee,
        callback_url=build_callback_url(wizard_data, wrap_data.get_session_type()),
        docusign_transport=transport
    )


    print(redirect_url)