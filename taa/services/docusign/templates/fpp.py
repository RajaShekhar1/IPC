
from dateutil.parser import parse as dateutil_parse
from dateutil.relativedelta import *
from datetime import date

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_template_id


class FPPTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["agent_data"]["state"]
        template_id = get_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients)

        self.data = enrollment_data

    def is_child_attachment_form_needed(self):
        return self.data.get_num_covered_children() > 2

    def is_replacement_form_needed(self):
        return self.data['replacing_insurance'] or self.data['existing_insurance']

    def is_additional_replacment_policy_attachment_needed(self):
        return len(self.data['replacement_policies']) > 1

    def get_attachment_children(self):
        return self.data.get_covered_children()[2:] if len(self.data.get_covered_children()) > 2 else []

    def generate_tabs(self, recipient):

        if recipient.is_agent():
            tabs = self.make_agent_tabs()
        else:

            lists_of_tabs = [
                self.make_employer_tabs(),
                self.make_employee_tabs(),
                self.make_spouse_tabs(),
                self.make_coverage_tabs(),
                self.make_all_beneficiary_tabs(),
                self.make_children_tabs(),
                self.make_general_tabs(),
            ]
            tabs = []
            for tab_list in lists_of_tabs:
                tabs.extend(tab_list)

        # Format the tabs for docusign
        docusign_tabs = {}
        for tab in tabs:
            tab.add_to_tabs(docusign_tabs)

        return docusign_tabs

    def make_agent_tabs(self):
        return [
            DocuSignRadioTab('existingInsAgent', 'yes' if self.data['existing_insurance'] else 'no'),
            DocuSignRadioTab('replaceAgent', 'yes' if self.data['replacement_policies'] else 'no'),
        ]

    def make_general_tabs(self):
        tabs = [
            DocuSignRadioTab('enrollType', "self" if self.data.is_self_enroll() else "assist"),
            DocuSignRadioTab('productType', "FPPTI" if self.data['product_type'] == "FPP-Gov" else self.data['product_type']),
            DocuSignRadioTab('existingIns', 'yes' if self.data["existing_insurance"] else 'no'),
            DocuSignRadioTab('existingInsAgent', 'yes' if self.data['existing_insurance'] else 'no'),
            DocuSignRadioTab('replaceAgent', 'yes' if self.data['replacement_policies'] else 'no'),
            DocuSignRadioTab('replace', 'yes' if self.data["replacing_insurance"] else 'no'),
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

        return tabs

    def make_employer_tabs(self):
        return [
            DocuSignTextTab('employer', self.data.get_employer_name()),
            DocuSignTextTab('group_number', self.data['agent_data']['group_number'] if self.data['agent_data']['group_number'] else "")
        ]


    def make_employee_tabs(self):

        ee_tabs_list = []
        ee_tabs_list += self.make_applicant_tabs(prefix="ee", data=self.data['employee'])
        ee_tabs_list += self.make_contact_tabs('ee', self.data["employee"])
        ee_tabs_list += self.make_payment_mode_tabs()
        ee_tabs_list += self.make_generic_tabs()

        if self.data.did_employee_select_coverage():
            ee_tabs_list += self.generate_SOH_tabs("ee", self.data['employee']['soh_questions'])
            ee_tabs_list += self.generate_SOH_GI_tabs("ee", self.data['employee']['soh_questions'])

        return ee_tabs_list



    def make_spouse_tabs(self):

        if not self.data.did_spouse_select_coverage():
            return

        sp_tabs_list = []

        sp_tabs_list += self.make_applicant_tabs(prefix="sp", data=self.data['spouse'])
        #sp_tabs_list += self.make_contact_tabs('sp', self.data["spouse"])

        if self.data.did_spouse_select_coverage():
            sp_tabs_list += self.generate_SOH_tabs("sp", self.data['spouse']['soh_questions'])
            sp_tabs_list += self.generate_SOH_GI_tabs("sp", self.data['spouse']['soh_questions'])

        sp_tabs_list += [
            DocuSignRadioTab('spouse_hospital_six_months', 'yes' if self.data['has_spouse_been_treated_6_months'] else 'no'),
            DocuSignRadioTab('spouse_disability_six_months', 'yes' if self.data['has_spouse_been_disabled_6_months'] else 'no'),
        ]

        return sp_tabs_list


    def make_children_tabs(self):
        tabs = []

        for i, child in enumerate(self.data['children']):
            if not self.data['children'][i] or not self.data['child_coverages'][i]:
                continue

            tabs += self.add_child_data_tabs(i)
            tabs += self.generate_SOH_tabs("c%s"%(i+1), self.data["children"][i]['soh_questions'])
            tabs += self.generate_SOH_GI_tabs("c%s"%(i+1), self.data["children"][i]['soh_questions'])

        if len(self.data['children']) > 2:
            tabs += [DocuSignTextTab('extra_children_notice', "SEE ATTACHED FOR ADDITIONAL CHILDREN")]

        return tabs

    def add_child_data_tabs(self, child_index):
        child_prefix = "child" + str(child_index + 1)
        child_coverage = self.data["child_coverages"][child_index]
        child_data = self.data["children"][child_index]
        return [
            DocuSignTextTab(child_prefix + "Name", child_data['first'] + " " + child_data['last']),
            DocuSignTextTab(child_prefix + "DOB", child_data['birthdate']),
            DocuSignTextTab(child_prefix + "SSN", self.format_ssn(child_data['ssn'])),
            DocuSignTextTab(child_prefix + "Coverage", format(child_coverage["face_value"], ",.0f") if child_coverage else ""),
            DocuSignTextTab(child_prefix + "Premium", format(child_coverage["premium"], ",.2f") if child_coverage else ""),
            DocuSignRadioTab(child_prefix + "Gender", child_data["gender"]),
        ]

    def make_coverage_tabs(self):
        coverage_tabs = []

        # Employee
        employee_coverage = self.data.get_employee_coverage() if self.data.did_employee_select_coverage() else 'NONE'
        ee_premium = self.data.get_formatted_employee_premium() if self.data.did_employee_select_coverage() else ''
        coverage_tabs += [
            DocuSignTextTab('eeCoverage', employee_coverage),
            DocuSignTextTab('eePremium', ee_premium),
        ]

        spouse_coverage = self.data.get_spouse_coverage() if self.data.did_spouse_select_coverage() else 'NONE'
        spouse_premium = self.data.get_formatted_spouse_premium() if self.data.did_spouse_select_coverage() else ''
        coverage_tabs += [
            DocuSignTextTab('spCoverage', spouse_coverage),
            DocuSignTextTab('spPremium', spouse_premium),
        ]

        # Totals
        total_children_coverage = sum(child_coverage.get('premium', 0) for child_coverage in self.data["child_coverages"])
        total = 0.0
        if self.data.did_employee_select_coverage():
            total += self.data.get_employee_premium()
        if self.data.did_spouse_select_coverage():
            total += self.data.get_spouse_premium()
        if total_children_coverage > 0.0:
            total += total_children_coverage

        if total_children_coverage > 0.0:
            formatted_children_total = format(total_children_coverage, ".2f")
        else:
            formatted_children_total = ""

        coverage_tabs += [
            DocuSignTextTab('eePremiumTotal', ee_premium),
            DocuSignTextTab('spPremiumTotal', spouse_premium),
            DocuSignTextTab('childPremiumTotal', formatted_children_total),
            DocuSignTextTab('totalAllPremium', format(total, ".2f"))
        ]

        return coverage_tabs

    def make_all_beneficiary_tabs(self):
        return (
            self.make_applicant_beneficiary_tabs("ee", "employee") +
            self.make_applicant_beneficiary_tabs("sp", "spouse")
        )

    def make_applicant_beneficiary_tabs(self, short_prefix, long_prefix):
        tabs = []

        if ((long_prefix == "employee" and not self.data.did_employee_select_coverage()) or
            (long_prefix == "spouse" and not self.data.did_spouse_select_coverage())):
            return tabs

        tabs += self.make_old_style_beneficiary_tabs(short_prefix, long_prefix)

        # Contingent
        contingent_type_key = '{}_contingent_beneficiary_type'.format(long_prefix)
        if contingent_type_key in self.data and self.data[contingent_type_key] == 'spouse':
            spouse_data = self.data["employee"] if long_prefix == "spouse" else self.data["spouse"]
            tabs += self.make_beneficiary_tabs(
                prefix=short_prefix+"Cont",
                name=spouse_data["first"] + " " + spouse_data["last"],
                relationship="Spouse",
                dob=spouse_data.get("birthdate"),
                ssn=spouse_data.get("ssn"),
            )
        elif contingent_type_key in self.data and self.data[contingent_type_key] == 'other':

            beneficiary_data = self.data['{}_contingent_beneficiary'.format(long_prefix)]
            tabs += self.make_beneficiary_tabs(prefix='{}Cont'.format(short_prefix),
                                       name=beneficiary_data.get('name'),
                                       relationship=beneficiary_data.get('relationship'),
                                       ssn=beneficiary_data.get('ssn'),
                                       dob=beneficiary_data.get('date_of_birth'),
            )

        return tabs

    def make_old_style_beneficiary_tabs(self, short_prefix, long_prefix):

        if self.data["{}_beneficiary".format(long_prefix)] == "spouse":
            spouse_data = self.data["employee"] if long_prefix == "spouse" else self.data["spouse"]
            return self.make_beneficiary_tabs(
                prefix=short_prefix,
                name=spouse_data["first"] + " " + spouse_data["last"],
                relationship="Spouse",
                dob=spouse_data["birthdate"],
                ssn=spouse_data["ssn"],
            )
        else:
            return self.make_beneficiary_tabs(
                prefix = short_prefix,
                name = self.data["{}_beneficiary_name".format(long_prefix)],
                relationship = self.data["{}_beneficiary_relationship".format(long_prefix)],
                dob = self.data.get("{}_beneficiary_dob".format(long_prefix)),
                ssn = self.data.get("{}_beneficiary_ssn".format(long_prefix)),
            )

    def make_payment_mode_tabs(self):
        return [
            DocuSignRadioTab(group_name='payment_mode', value=self.data['payment_mode_text'], is_selected=True)
        ]

    def make_generic_tabs(self):

        ee_email_part_1, ee_email_part_2 = self.data.get_employee_email_parts()

        agent_code = self.data.get_agent_code()
        agent_signing_name = self.data.get_agent_signing_name()

        if self.data['spouse_owner'] == "other":
            spouse_owner_notice = "SPOUSE POLICY OWNER: {}, {}".format(self.data['spouse_other_owner_name'], self.data['spouse_other_owner_ssn'])
        elif self.data['spouse_owner'] == "self":
            # Spouse data
            spouse_owner_notice = "SPOUSE POLICY OWNER: {}, {}".format(self.data['spName'], self.data['spSSN'])
        else:
            spouse_owner_notice = ""

        return [
            DocuSignTextTab('eeEnrollCityState', self.data["enrollCity"] + ", " + self.data["enrollState"]),
            DocuSignTextTab('eeEnrollCity', self.data['enrollCity']),
            DocuSignTextTab('eeEnrollState', self.data['enrollState']),
            DocuSignTextTab('date_of_hire', self.data['identityToken']),
            DocuSignTextTab('agentCode', agent_code),
            DocuSignTextTab('agentSignName', agent_signing_name),
            DocuSignTextTab('Employer', self.data.get_employer_name()),
            DocuSignTextTab('eeOtherOwnerName', self.data["employee_other_owner_name"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('eeOtherOwnerName2', self.data["employee_other_owner_name"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('eeOtherOwnerSSN', self.data["employee_other_owner_ssn"] if self.data["employee_owner"] == "other" else  ""),
            DocuSignTextTab('spouse_owner_notice', spouse_owner_notice),
            DocuSignTextTab('eeEmailPart1', ee_email_part_1),
            DocuSignTextTab('eeEmailPart2', ee_email_part_2),
            DocuSignRadioTab('actively_at_work', "yes" if self.data['is_employee_actively_at_work'] else "no"),
        ]


    def make_contact_tabs(self, prefix, data):

        if data.get('address2'):
            address = data['address1'] + " " + data['address2']
        else:
            address = data['address1']

        return [
            DocuSignTextTab(prefix+'Address', address),
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
            DocuSignTextTab(prefix+"BeneSSN", self.format_ssn(ssn)),
        ]

    def format_ssn(self, ssn):
        if not ssn:
            return ssn

        digits = [c for c in ssn if c.isdigit()]
        if len(digits) < 9:
            # Invalid - just return what was given
            return ssn

        return "{}-{}-{}".format(
            ''.join(digits[:3]),
            ''.join(digits[3:5]),
            ''.join(digits[5:9])
        )


    def get_age_from_dob(self, dob):
        if dob and dateutil_parse(dob):
            return "%d"%relativedelta(date.today(), dateutil_parse(dob)).years
        else:
            return ""

    def generate_SOH_tabs(self, prefix, soh_questions):
        "Statement of health radio button answers"

        radio_tabs = []
        for i, soh_question in enumerate(soh_questions):
            if soh_question['answer'] and soh_question['answer'].lower() == "no":
                radio_tabs.append(DocuSignRadioTab(prefix + "SOH" + str(i+1), "no"))

        return radio_tabs

    def generate_SOH_GI_tabs(self, prefix, soh_questions):
        tabs = []
        for i, soh_question in enumerate(soh_questions):
            if soh_question['answer'] and soh_question['answer'].upper() == "GI" or soh_question['answer'] == None:
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
            DocuSignTextTab(prefix + 'SSN', self.format_ssn(data["ssn"])),
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


if __name__ == "__main__":
    from taa.services.docusign.service import AgentDocuSignRecipient, EmployeeDocuSignRecipient, get_docusign_transport, create_envelope
    from taa.services.docusign.docusign_envelope import build_callback_url, EnrollmentDataWrap
    from taa.services.docusign.templates.fpp_replacement import FPPReplacementFormTemplate
    from taa.services.docusign.documents.extra_children import ChildAttachmentForm

    # Sample pulled from site
    true = True
    false = False
    null = None
    wizard_data = {"health_questions":[{"label":"Hospital 90 days","question_text":"Has any Applicant been hospitalized in the past 90 days?","skip_if_coverage_at_most":null},{"label":"Heart","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","skip_if_coverage_at_most":null},{"label":"Cancer","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","skip_if_coverage_at_most":null},{"label":"Respiratory","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","skip_if_coverage_at_most":null},{"label":"Liver","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","skip_if_coverage_at_most":null},{"label":"HIV/AIDS","question_text":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","skip_if_coverage_at_most":null},{"label":"Ever been rejected","question_text":"Has any Applicant ever applied for and been rejected for life insurance?","skip_if_coverage_at_most":null}],"agent_data":{"children_data":[{"birthdate":"01/04/1989","existing_coverages":[],"first":"Betty","last":"Harrison"},{"birthdate":"04/16/2008","existing_coverages":[],"first":"Sharon","last":"Richards"}],"company_name":"ABC","employee_data":{"birthdate":"02/27/1965","city":"Birmingham","email":"srichards1i2@edublogs.org","existing_coverages":[],"first":"Melissa","gender":"female","height":null,"is_smoker":null,"last":"Alexander","phone":"2-(291)154-4439","ssn":"801618499","state":"AL","street_address":"2809 Shopko Court","street_address2":"","weight":null,"zip":"35220"},"enroll_city":"Indianapolis","group_number":"ABC-XYZ-12334","health_questions":{"2":[{"label":"Hospital 90 days","question_text":"Has any Applicant been hospitalized in the past 90 days?","skip_if_coverage_at_most":null},{"label":"Heart","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","skip_if_coverage_at_most":null},{"label":"Cancer","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","skip_if_coverage_at_most":null},{"label":"Respiratory","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","skip_if_coverage_at_most":null},{"label":"Liver","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","skip_if_coverage_at_most":null},{"label":"HIV/AIDS","question_text":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","skip_if_coverage_at_most":null},{"label":"Ever been rejected","question_text":"Has any Applicant ever applied for and been rejected for life insurance?","skip_if_coverage_at_most":null}]},"is_in_person":true,"payment_mode":12,"payment_mode_choices":[{"immutable":true,"mode":12,"name":"Monthly"}],"products":[{"base_product_type":"FPPCI","bypassed_soh_questions":[],"code":"FPPCI","gi_criteria":[],"id":2,"is_guaranteed_issue":false,"name":"Family Protection Plan - Critical Illness","product_type":"base","restricted_agents":[],"visible_to_agents":true}],"spouse_data":{"birthdate":"11/03/1960","city":"Evansville","email":"srichards1i2@arstechnica.com","existing_coverages":[],"first":"Ruby","gender":"female","height":null,"is_smoker":null,"last":"Watson","phone":"1-(607)104-9073","ssn":"602243564","state":"IN","street_address":"6 Kim Park","street_address2":"1412 Ridgeview Pass","weight":null,"zip":"47719"},"state":"IN"},"enrollCity":"Indianapolis","enrollState":"IN","product_type":"FPPCI","payment_mode":12,"payment_mode_text":"monthly","method":"in_person","did_decline":false,"identityToken":"12/12/2000","identityType":"","employee":{"first":"Melissa","last":"Alexander","email":"srichards1i2@edublogs.org","age":50,"weight":null,"height":null,"is_smoker":null,"birthdate":"02/27/1965","ssn":"801618499","gender":"female","phone":"2-(291)154-4439","address1":"2809 Shopko Court","address2":"","city":"Birmingham","state":"AL","zip":"35220","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},"spouse":{"first":"Ruby","last":"Watson","email":"srichards1i2@arstechnica.com","age":54,"weight":null,"height":null,"is_smoker":null,"birthdate":"11/03/1960","ssn":"602243564","gender":"female","phone":"1-(607)104-9073","address1":"6 Kim Park","address2":"1412 Ridgeview Pass","city":"Evansville","state":"IN","zip":"47719","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},"is_spouse_address_same_as_employee":false,"is_spouse_email_same_as_employee":false,"existing_insurance":false,"replacing_insurance":true,"is_employee_actively_at_work":false,"has_spouse_been_treated_6_months":false,"has_spouse_been_disabled_6_months":false,"employee_owner":"self","employee_other_owner_name":"","employee_other_owner_ssn":"","spouse_owner":"employee","spouse_other_owner_name":"","spouse_other_owner_ssn":"","employee_beneficiary":"spouse","spouse_beneficiary":"spouse","employee_contingent_beneficiary_type":"none","employee_contingent_beneficiary":{},"spouse_contingent_beneficiary_type":"none","spouse_contingent_beneficiary":{},"children":[{"first":"Betty","last":"Harrison","email":"","age":16,"weight":null,"height":null,"is_smoker":null,"birthdate":"01/04/1999","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},{"first":"Sharon","last":"Richards","email":"","age":7,"weight":null,"height":null,"is_smoker":null,"birthdate":"04/16/2008","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},{"first":"eoui","last":"Alexander","email":"","age":16,"weight":null,"height":null,"is_smoker":null,"birthdate":"01/01/1999","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]}],"child_coverages":[{"premium":4.99,"face_value":10000},{"premium":4.99,"face_value":10000},{"premium":4.99,"face_value":10000}],"employee_coverage":{"premium":118.34,"face_value":75000},"spouse_coverage":{"premium":52.74,"face_value":25000},"product_data":{"base_product_type":"FPPCI","bypassed_soh_questions":[],"code":"FPPCI","gi_criteria":[],"id":2,"is_guaranteed_issue":false,"name":"Family Protection Plan - Critical Illness","product_type":"base","restricted_agents":[],"visible_to_agents":true},"replacement_read_aloud":true,"replacement_is_terminating":true,"replacement_using_funds":true,"replacement_policies":[{"name":"oeuaoue","policy_number":"aoeuoeua","insured":"oueuoe","replaced_or_financing":"replaced","replacement_reason":"oeueieuieuoi"},{"name":"',.p,.p'","policy_number":".py.p,y","insured":"iddu","replaced_or_financing":"replaced","replacement_reason":"eu"}]}
    enrollment_data = wrap_data = EnrollmentDataWrap(wizard_data, None)

    owner_agent = "Agent Mason" # census_record.case.owner_agent
    agent = AgentDocuSignRecipient(name=owner_agent, email="agent@zachmason.com")
    employee = EmployeeDocuSignRecipient(name=wrap_data.get_employee_name(),
                                         email="zach@zachmason.com")
    recipients = [
        agent,
        employee,
        # TODO Check if BCC's needed here
    ]

    fpp_form = FPPTemplate(recipients, wrap_data)
    components = [fpp_form]

    # Replacement Form
    if fpp_form.is_replacement_form_needed():
        replacement_form = FPPReplacementFormTemplate(recipients, wrap_data)
        components.append(replacement_form)

    # Additional Children
    if fpp_form.is_child_attachment_form_needed():
        child_attachment_form = ChildAttachmentForm(recipients, wrap_data)

        for i, child in enumerate(fpp_form.get_attachment_children()):
            child.update(dict(
                coverage=format(enrollment_data['child_coverages'][i+2]['face_value'], ",.0f"),
                premium=format(enrollment_data['child_coverages'][i+2]['premium'], ".2f"),
            ))
            child_attachment_form.add_child(child)

        components.append(child_attachment_form)

    transport = get_docusign_transport()
    envelope_result = create_envelope(email_subject="Signature needed",
                                      components=components,
                                      docusign_transport=transport,
                                     )

    redirect_url = envelope_result.get_signing_url(
        employee,
        callback_url=build_callback_url(wizard_data, wrap_data.get_session_type()),
        docusign_transport=transport
    )


    print(redirect_url)

