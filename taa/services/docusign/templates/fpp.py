
from dateutil.parser import parse as dateutil_parse
from dateutil.relativedelta import *
from datetime import date
from decimal import Decimal

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_template_id


class FPPTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, use_docusign_renderer):

        product_type = enrollment_data["product_type"]
        state = enrollment_data["enrollState"]
        template_id = get_template_id(product_type, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, use_docusign_renderer)

        self.data = enrollment_data

    def is_child_attachment_form_needed(self):
        return self.data.get_num_covered_children() > 2

    def is_replacement_form_needed(self):
        """
        Implement the slightly complicated rules dictating whether or not the replacement form
        for the appropriate state will be shown, based mostly on what state the
        enrollment application was taken in.
        :return: True if the form is needed.
        """

        # These states don't do replacements
        if self.data['enrollState'] in ['KY', 'KS', 'CT', 'DC', 'ND', 'VI']:
            return False

        # NAIC states and MI have a special rule if the replacement question is 'No'
        from taa.services.products.product_forms import generic_fpp_replacement_form
        if (self.data['enrollState'] in (generic_fpp_replacement_form.statecodes + ['MI'])
                and not self.data['replacing_insurance']):
            self.data['replacement_is_terminating'] = False
            self.data['replacement_using_funds'] = False
            self.data['replacement_policies'] = []
            return True

        # Self-enroll needs to be given the form if the existing question is Yes.
        if self.data.is_self_enroll() and self.data['existing_insurance']:
            return True

        # Otherwise just include form if replacing insurance question is yes.
        return self.data['replacing_insurance']

    def is_additional_replacment_policy_attachment_needed(self):
        return len(self.data['replacement_policies']) > 1

    def get_attachment_children(self):
        return self.data.get_covered_children()[2:] if len(self.data.get_covered_children()) > 2 else []

    def generate_tabs(self, recipient):
        tabs = super(FPPTemplate, self).generate_tabs(recipient)

        if recipient.is_agent():
            tabs += self.make_agent_tabs()
        elif recipient.is_employee():

            lists_of_tabs = [
                self.make_employer_tabs(),
                self.make_employee_tabs(),
                self.make_spouse_tabs(),
                self.make_coverage_tabs(),
                self.make_all_beneficiary_tabs(),
                self.make_children_tabs(),
                self.make_general_tabs(),
            ]
            for tab_list in lists_of_tabs:
                tabs.extend(tab_list)

        return tabs

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

            if prefix_short == 'ee' or (prefix_short == 'sp' and self.data.did_spouse_select_coverage()):
                tabs.append(DocuSignRadioTab(prefix_short + "Gender", self.data[prefix_long]["gender"]))

            if self.data[prefix_long] and "is_smoker" in self.data[prefix_long]:
                smoker_button = "smoker" if self.data[prefix_long]["is_smoker"] else "nonsmoker"
                tabs.append(DocuSignRadioTab(prefix_short + "Smoking", smoker_button))

            # only include Owner checkbox if coverage was selected
            if ((prefix_short == "ee" and self.data.did_employee_select_coverage())
                    or (prefix_short == "sp" and self.data.did_spouse_select_coverage())):

                tabs.append(DocuSignRadioTab(prefix_short + "Owner", self.data[prefix_long + "_owner"]))

        return tabs

    def make_employer_tabs(self):
        return [
            DocuSignTextTab('employer', self.data.get_employer_name()),
            DocuSignTextTab('group_number', self.data.case.group_number if self.data.case.group_number else "")
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
            return []

        sp_tabs_list = []

        sp_tabs_list += self.make_applicant_tabs(prefix="sp", data=self.data['spouse'])

        if self.data['is_spouse_address_same_as_employee']:
            sp_tabs_list += [DocuSignTextTab('sp_address_same_as_employee', "SAME AS EMPLOYEE")]
        else:
            sp_tabs_list += self.make_address_tabs(prefix="sp", data=self.data['spouse'])

        if self.data['is_spouse_email_same_as_employee']:
            sp_tabs_list += [DocuSignTextTab('sp_email_same_as_employee', 'SAME AS EMPLOYEE')]
        else:
            sp_tabs_list += [DocuSignTextTab('spEmail', self.data['spouse']['email'])]

        if self.data.did_spouse_select_coverage():
            # Special treatment for first two questions (non-health questions)
            if self.data['has_spouse_been_treated_6_months'] in ['Yes', 'No']:
                sp_tabs_list += [DocuSignRadioTab('spouse_hospital_six_months', self.data['has_spouse_been_treated_6_months'].lower())]
            elif self.data['has_spouse_been_treated_6_months'].upper() == 'GI':
                sp_tabs_list += [DocuSignTextTab('spouse_hospital_six_months_gi', 'GI')]

            if self.data['has_spouse_been_disabled_6_months'] in ['Yes', 'No']:
                sp_tabs_list += [DocuSignRadioTab('spouse_disability_six_months', self.data['has_spouse_been_disabled_6_months'].lower())]
            elif self.data['has_spouse_been_disabled_6_months'].upper() == 'GI':
                sp_tabs_list += [DocuSignTextTab('spouse_disability_six_months_gi', 'GI')]

            sp_tabs_list += self.generate_SOH_tabs("sp", self.data['spouse']['soh_questions'])
            sp_tabs_list += self.generate_SOH_GI_tabs("sp", self.data['spouse']['soh_questions'])

        return sp_tabs_list


    def make_children_tabs(self):
        tabs = []

        for i, child in enumerate(self.data['children']):
            if not self.data['children'][i] or not self.data['child_coverages'][i]:
                continue

            tabs += self.add_child_data_tabs(i)
            tabs += self.generate_SOH_tabs("c%s"%(i+1), self.data["children"][i]['soh_questions'])
            tabs += self.generate_SOH_GI_tabs("c%s"%(i+1), self.data["children"][i]['soh_questions'])

        if self.is_child_attachment_form_needed():
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
            DocuSignTextTab(child_prefix + "Coverage", format(Decimal(unicode(child_coverage["face_value"])), ",.0f") if child_coverage else ""),
            DocuSignTextTab(child_prefix + "Premium", format(Decimal(unicode(child_coverage["premium"])), ",.2f") if child_coverage else ""),
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
        total_children_coverage = sum(Decimal(unicode(child_coverage.get('premium', '0.00'))) for child_coverage in self.data["child_coverages"])
        total = Decimal('0.00')
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

        tabs = self.make_address_tabs(prefix, data)
        tabs += [
            DocuSignTextTab(prefix + 'Phone', data['phone']),
            DocuSignTextTab(prefix + 'Email', data['email']),
        ]
        return tabs

    def make_address_tabs(self, prefix, data):

        if data.get('address2'):
            address = data['address1'] + " " + data['address2']
        else:
            address = data['address1']

        return [
            DocuSignTextTab(prefix+'Address', address),
            DocuSignTextTab(prefix + 'City', data['city']),
            DocuSignTextTab(prefix + 'State', data['state']),
            DocuSignTextTab(prefix + 'Zip', data['zip']),
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
        for i, soh_question in enumerate(q for q in soh_questions if not q.get('is_spouse_only')):
            if soh_question['answer'] and soh_question['answer'].lower() == "no":
                radio_tabs.append(DocuSignRadioTab(prefix + "SOH" + str(i+1), "no"))

        return radio_tabs

    def generate_SOH_GI_tabs(self, prefix, soh_questions):
        tabs = []
        for i, soh_question in enumerate(q for q in soh_questions if not q.get('is_spouse_only')):
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
            height_total_inches = int(data['height'])
            height_ft = "%s" % int(height_total_inches / 12.0)
            height_in = "%s" % int(height_total_inches % 12.0)

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
    wizard_data = {"health_questions":[{"label":"Hospital 90 days","question_text":"Has any Applicant been hospitalized in the past 90 days?","skip_if_coverage_at_most":null},{"label":"Heart","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","skip_if_coverage_at_most":null},{"label":"Cancer","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","skip_if_coverage_at_most":null},{"label":"Respiratory","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","skip_if_coverage_at_most":null},{"label":"Liver","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","skip_if_coverage_at_most":null},{"label":"HIV/AIDS","question_text":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","skip_if_coverage_at_most":null},{"label":"Ever been rejected","question_text":"Has any Applicant ever applied for and been rejected for life insurance?","skip_if_coverage_at_most":null}],"agent_data":{"children_data":[],"company_name":"ABC","employee_data":{"birthdate":"","city":null,"email":null,"existing_coverages":[],"first":null,"gender":"","height":null,"is_smoker":null,"last":null,"phone":null,"ssn":"111111116","state":null,"street_address":null,"street_address2":null,"weight":null,"zip":null},"enroll_city":"Indianapolis","group_number":"ABC-XYZ-12334","health_questions":{"2":[{"label":"Hospital 90 days","question_text":"Has any Applicant been hospitalized in the past 90 days?","skip_if_coverage_at_most":null},{"label":"Heart","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","skip_if_coverage_at_most":null},{"label":"Cancer","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","skip_if_coverage_at_most":null},{"label":"Respiratory","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","skip_if_coverage_at_most":null},{"label":"Liver","question_text":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","skip_if_coverage_at_most":null},{"label":"HIV/AIDS","question_text":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","skip_if_coverage_at_most":null},{"label":"Ever been rejected","question_text":"Has any Applicant ever applied for and been rejected for life insurance?","skip_if_coverage_at_most":null}]},"is_in_person":true,"payment_mode":12,"payment_mode_choices":[{"immutable":true,"mode":12,"name":"Monthly"}],"products":[{"base_product_type":"FPPCI","bypassed_soh_questions":[],"code":"FPPCI","gi_criteria":[],"id":2,"is_guaranteed_issue":false,"name":"Family Protection Plan - Critical Illness","product_type":"base","restricted_agents":[],"visible_to_agents":true}],"spouse_data":{"birthdate":"","city":null,"email":null,"existing_coverages":[],"first":null,"gender":"","height":null,"is_smoker":null,"last":null,"phone":null,"ssn":null,"state":null,"street_address":null,"street_address2":null,"weight":null,"zip":null},"state":"IN"},"enrollCity":"Indianapolis","enrollState":"IN","product_type":"FPPCI","payment_mode":12,"payment_mode_text":"monthly","method":"in_person","did_decline":false,"identityToken":"01/01/1999","identityType":"","employee":{"first":"John","last":"Smith","email":null,"age":35,"weight":null,"height":null,"is_smoker":null,"birthdate":"01/01/1980","ssn":"111111116","gender":"male","phone":null,"address1":"Test Street","address2":null,"city":"ABC","state":"IN","zip":"12345","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},"spouse":{"first":"Jane","last":"Smith","email":null,"age":27,"weight":null,"height":null,"is_smoker":null,"birthdate":"01/01/1988","ssn":"111-12-2222","gender":"female","phone":null,"address1":null,"address2":null,"city":null,"state":"","zip":null,"soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},"is_spouse_address_same_as_employee":true,"is_spouse_email_same_as_employee":true,"existing_insurance":false,"replacing_insurance":true,"is_employee_actively_at_work":false,"has_spouse_been_treated_6_months":false,"has_spouse_been_disabled_6_months":false,"employee_owner":"self","employee_other_owner_name":"","employee_other_owner_ssn":"","spouse_owner":"employee","spouse_other_owner_name":"","spouse_other_owner_ssn":"","employee_beneficiary":"spouse","spouse_beneficiary":"spouse","employee_contingent_beneficiary_type":"none","employee_contingent_beneficiary":{},"spouse_contingent_beneficiary_type":"none","spouse_contingent_beneficiary":{},"children":[{"first":"Johnny","last":"Smith","email":"","age":4,"weight":null,"height":null,"is_smoker":null,"birthdate":"10/10/2010","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},{"first":"Janet","last":"Smith","email":"","age":3,"weight":null,"height":null,"is_smoker":null,"birthdate":"11/11/2011","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]},{"first":"Julie","last":"Smith","email":"","age":2,"weight":null,"height":null,"is_smoker":null,"birthdate":"11/11/2012","ssn":"","gender":null,"phone":"","address1":"","address2":"","city":"","state":"","zip":"","soh_questions":[{"question":"Has any Applicant been hospitalized in the past 90 days?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?","answer":"No"},{"question":"In the past 10 years, has any Applicant had or been hospitalized for, been medically diagnosed, treated, or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?","answer":"No"},{"question":"Has any Applicant been diagnosed or treated by a physician, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?","answer":"No"},{"question":"Has any Applicant ever applied for and been rejected for life insurance?","answer":"No"}]}],"child_coverages":[{"premium":4.99,"face_value":10000},{"premium":4.99,"face_value":10000},{"premium":4.99,"face_value":10000}],"employee_coverage":{"premium":69.55,"face_value":125000},"spouse_coverage":{"premium":24.35,"face_value":50000},"product_data":{"base_product_type":"FPPCI","bypassed_soh_questions":[],"code":"FPPCI","gi_criteria":[],"id":2,"is_guaranteed_issue":false,"name":"Family Protection Plan - Critical Illness","product_type":"base","restricted_agents":[],"visible_to_agents":true},"replacement_read_aloud":false,"replacement_is_terminating":true,"replacement_using_funds":false,"replacement_policies":[{"name":"uuu","policy_number":"uuu","insured":"uuu","replaced_or_financing":"replaced","replacement_reason":"aoeu"},{"name":"iii","policy_number":"iii","insured":"iii","replaced_or_financing":"financing","replacement_reason":"iii"}]}
    enrollment_data = wrap_data = EnrollmentDataWrap(wizard_data, None, None)

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
