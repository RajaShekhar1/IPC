from itertools import ifilter

from dateutil.parser import parse as dateutil_parse
from dateutil.relativedelta import *
from datetime import date
from decimal import Decimal

from taa.services.docusign.service import DocuSignServerTemplate, DocuSignTextTab, DocuSignRadioTab
from taa.services.docusign.DocuSign_config import get_template_id


class FPPTemplate(DocuSignServerTemplate):
    def __init__(self, recipients, enrollment_data, use_docusign_renderer):

        product_code = enrollment_data.get_product().get_base_product_code()
        state = enrollment_data["enrollState"]
        template_id = get_template_id(product_code, state)

        DocuSignServerTemplate.__init__(self, template_id, recipients, use_docusign_renderer)

        self.data = enrollment_data

    def is_child_attachment_form_needed(self):
        return self.data.get_num_covered_children() > 2

    def is_beneficiary_attachment_needed(self):
        return self.data.has_multiple_beneficiaries()

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

        # NAIC states and MI have a special rule to attach the form if the replacement question is 'No'
        from taa.services.products.product_forms import generic_fpp_replacement_form
        if (self.data['enrollState'] in (generic_fpp_replacement_form.statecodes + ['MI'])
            and not self.data['replacing_insurance']):
            return True

        # Self-enroll needs to be given the form if the existing question is Yes.
        if self.data.is_self_enroll() and self.data['existing_insurance']:
            return True

        # Otherwise just include form if replacing insurance question is yes.
        return self.data['replacing_insurance']

    def is_additional_replacement_policy_attachment_needed(self):
        return len(self.data['replacement_policies']) > 1

    def should_include_bank_draft(self):
        return self.data.should_include_bank_draft()

    def get_attachment_children(self):
        return self.data.get_covered_children()[2:] if len(self.data.get_covered_children()) > 2 else []

    def generate_tabs(self, recipient, purpose):
        tabs = super(FPPTemplate, self).generate_tabs(recipient, purpose)

        if recipient.is_agent():
            tabs += self.make_agent_tabs(tabs, purpose)

        if recipient.is_employee() or self.data.should_use_call_center_workflow():

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

    def make_agent_tabs(self, tabs, purpose):
        if self.data.should_use_call_center_workflow() and purpose == self.PDF_TABS:
            # Don't render any overlay for the pdf
            return []
        elif self.data.should_use_call_center_workflow() and purpose == self.DOCUSIGN_TABS:

            # find the existingInsAgent and replaceAgent tabs and set the value to what the employee indicated as a default.
            existing_tab_yes = next(ifilter(
                lambda t: isinstance(t, DocuSignRadioTab) and t.group_name == 'existingInsAgent' and t.value == "yes",
                tabs), None)
            existing_tab_no = next(ifilter(
                lambda t: isinstance(t, DocuSignRadioTab) and t.group_name == 'existingInsAgent' and t.value == "no",
                tabs), None)

            replace_tab_yes = next(ifilter(
                lambda t: isinstance(t, DocuSignRadioTab) and t.group_name == 'replaceAgent' and t.value == "yes",
                tabs), None)
            replace_tab_no = next(ifilter(
                lambda t: isinstance(t, DocuSignRadioTab) and t.group_name == 'replaceAgent' and t.value == "no", tabs),
                                  None)

            if existing_tab_yes:
                existing_tab_yes.is_selected = self.data['existing_insurance']
            if existing_tab_no:
                existing_tab_no.is_selected = not self.data['existing_insurance']

            if replace_tab_yes:
                replace_tab_yes.is_selected = self.data['replacing_insurance']
            if replace_tab_no:
                replace_tab_no.is_selected = not self.data['replacing_insurance']

            # Don't add any new tabs.
            return []

        return [
            DocuSignRadioTab('existingInsAgent', 'yes' if self.data['existing_insurance'] else 'no'),
            DocuSignRadioTab('replaceAgent', 'yes' if self.data['replacing_insurance'] else 'no'),
        ]

    def make_general_tabs(self):

        tabs = [
            DocuSignRadioTab('enrollType', "assist" if self.data.is_enrollment_type_agent_assisted() else "self"),
            DocuSignRadioTab('productType', self.get_product_type()),
            DocuSignRadioTab('existingIns', 'yes' if self.data["existing_insurance"] else 'no'),
            DocuSignRadioTab('existingInsAgent', 'yes' if self.data['existing_insurance'] else 'no'),
            DocuSignRadioTab('replaceAgent', 'yes' if self.data['replacing_insurance'] else 'no'),
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
            ee_tabs_list += self.generate_SOH_tabs("ee", self.data.get_employee_soh_questions())
            ee_tabs_list += self.generate_SOH_GI_tabs("ee", self.data.get_employee_soh_questions())

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
                sp_tabs_list += [DocuSignRadioTab('spouse_hospital_six_months',
                                                  self.data['has_spouse_been_treated_6_months'].lower())]
            elif str(self.data['has_spouse_been_treated_6_months']).upper() == 'GI':
                sp_tabs_list += [DocuSignTextTab('spouse_hospital_six_months_gi', 'GI')]

            if self.data['has_spouse_been_disabled_6_months'] in ['Yes', 'No']:
                sp_tabs_list += [DocuSignRadioTab('spouse_disability_six_months',
                                                  self.data['has_spouse_been_disabled_6_months'].lower())]
            elif str(self.data['has_spouse_been_disabled_6_months']).upper() == 'GI':
                sp_tabs_list += [DocuSignTextTab('spouse_disability_six_months_gi', 'GI')]

            sp_tabs_list += self.generate_SOH_tabs("sp", self.data.get_spouse_soh_questions())
            sp_tabs_list += self.generate_SOH_GI_tabs("sp", self.data.get_spouse_soh_questions())

        return sp_tabs_list

    def make_children_tabs(self):
        tabs = []

        for i, child in enumerate(self.data['children']):
            if not self.data['children'][i] or not self.data['child_coverages'][i]:
                continue

            tabs += self.add_child_data_tabs(i)
            tabs += self.generate_SOH_tabs("c%s" % (i + 1), self.data.get_child_soh_questions(i))
            tabs += self.generate_SOH_GI_tabs("c%s" % (i + 1), self.data.get_child_soh_questions(i))

        if self.is_child_attachment_form_needed():
            tabs += [DocuSignTextTab('extra_children_notice', "SEE ATTACHED FOR ADDITIONAL CHILDREN")]

        return tabs

    def add_child_data_tabs(self, child_index):
        child_prefix = "child" + str(child_index + 1)
        child_coverage = self.data["child_coverages"][child_index]
        child_data = self.data["children"][child_index]
        return [
            DocuSignTextTab(child_prefix + "Name", child_data['first'] + " " + child_data['last']),
            # For old form and Group CI compatibility, also send separate first and last
            DocuSignTextTab(child_prefix + "FName", child_data['first']),
            DocuSignTextTab(child_prefix + "LName", child_data['last']),
            DocuSignTextTab(child_prefix + "DOB", child_data['birthdate']),
            DocuSignTextTab(child_prefix + "SSN", self.format_ssn(child_data['ssn'])),
            DocuSignTextTab(child_prefix + "Coverage", format(Decimal(unicode(child_coverage["face_value"]), 'utf-8'),
                                                              ",.0f") if child_coverage else ""),
            DocuSignTextTab(child_prefix + "Premium", format(Decimal(unicode(child_coverage["premium"]), 'utf-8'),
                                                             ",.2f") if child_coverage else ""),
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
        total_children_coverage = self.data.get_total_children_premium()
        total = self.data.get_total_modal_premium()

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

        if self.data.has_multiple_beneficiaries():
            # All beneficiary data is attached to a separate document in this case.
            tabs.append(DocuSignTextTab('eeBene_notice', 'SEE ATTACHED'))
            tabs.append(DocuSignTextTab('spBene_notice', 'SEE ATTACHED'))
            return tabs

        tabs += self.make_old_style_beneficiary_tabs(short_prefix, long_prefix)

        # Contingent Beneficiaries are in slightly different format
        contingent_type_key = '{}_contingent_beneficiary_type'.format(long_prefix)
        if contingent_type_key in self.data and self.data[contingent_type_key] == 'spouse':
            spouse_data = self.data["employee"] if long_prefix == "spouse" else self.data["spouse"]
            tabs += self.make_beneficiary_tabs(
                prefix=short_prefix + "Cont",
                name=spouse_data["first"] + " " + spouse_data["last"],
                relationship="Spouse",
                dob=spouse_data.get("birthdate", ''),
                ssn=spouse_data.get("ssn", ''),
            )
        elif contingent_type_key in self.data and self.data[contingent_type_key] == 'other':
            key_prefix = '{}_contingent_beneficiary1'.format(long_prefix)
            bene_name = self.data['{}_name'.format(key_prefix)]
            bene_rel = self.data[key_prefix + '_relationship']
            bene_ssn = self.data[key_prefix + '_ssn']
            bene_dob = self.data[key_prefix + '_dob']

            tabs += self.make_beneficiary_tabs(prefix='{}Cont'.format(short_prefix),
                                               name=bene_name,
                                               relationship=bene_rel,
                                               ssn=bene_ssn,
                                               dob=bene_dob,
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
                prefix=short_prefix,
                name=self.data.get("{}_beneficiary1_name".format(long_prefix), ''),
                relationship=self.data.get("{}_beneficiary1_relationship".format(long_prefix), ''),
                dob=self.data.get("{}_beneficiary1_dob".format(long_prefix), ''),
                ssn=self.data.get("{}_beneficiary1_ssn".format(long_prefix), ''),
            )

    def make_payment_mode_tabs(self):
        return [
            DocuSignRadioTab(group_name='payment_mode', value=self.data['payment_mode_text'].lower(), is_selected=True)
        ]

    def make_generic_tabs(self):

        ee_email_part_1, ee_email_part_2 = self.data.get_employee_email_parts()

        agent_code = self.data.get_agent_code()
        agent_signing_name = self.data.get_agent_signing_name()

        if self.data['spouse_owner'] == "other":
            spouse_owner_notice = u"SPOUSE POLICY OWNER: {}, {}".format(self.data['spouse_other_owner_name'],
                                                                        self.data['spouse_other_owner_ssn'])
        elif self.data['spouse_owner'] == "self":
            # Spouse data
            spouse_owner_notice = u"SPOUSE POLICY OWNER: {}, {}".format(self.data.get_spouse_name(),
                                                                        self.data.get_spouse_ssn())
        else:
            spouse_owner_notice = ""

        rider_tabs = []
        for rider_person, riders in self.data['rider_data'].iteritems():
            def fix_rider_code(code):
                if code.startswith("QOL"):
                    # QOL rider is "CHR" on the form.
                    return "CHR"

                return code

            if rider_person == "emp" and self.data.did_employee_select_coverage():
                for rider in riders:
                    tab_name = 'ee_rider_{}'.format(fix_rider_code(rider.get('code')))
                    rider_tabs.append(DocuSignRadioTab(tab_name, 'yes'))
            if rider_person == "sp" and self.data.did_spouse_select_coverage():
                for rider in riders:
                    tab_name = 'sp_rider_{}'.format(fix_rider_code(rider.get('code')))
                    rider_tabs.append(DocuSignRadioTab(tab_name, 'yes'))

        return [
                   DocuSignTextTab('eeEnrollCityState', self.data["enrollCity"] + ", " + self.data["enrollState"]),
                   DocuSignTextTab('eeEnrollCity', self.data['enrollCity']),
                   DocuSignTextTab('eeEnrollState', self.data['enrollState']),
                   DocuSignTextTab('date_of_hire', self.data['identityToken']),
                   DocuSignTextTab('agentCode', agent_code),
                   DocuSignTextTab('agentSignName', agent_signing_name),
                   DocuSignTextTab('Employer', self.data.get_employer_name()),
                   DocuSignTextTab('eeOtherOwnerName', self.data["employee_other_owner_name"] if self.data[
                                                                                                     "employee_owner"] == "other" else  ""),
                   DocuSignTextTab('eeOtherOwnerName2', self.data["employee_other_owner_name"] if self.data[
                                                                                                      "employee_owner"] == "other" else  ""),
                   DocuSignTextTab('eeOtherOwnerSSN', self.data["employee_other_owner_ssn"] if self.data[
                                                                                                   "employee_owner"] == "other" else  ""),
                   DocuSignTextTab('spouse_owner_notice', spouse_owner_notice),
                   DocuSignTextTab('eeEmailPart1', ee_email_part_1),
                   DocuSignTextTab('eeEmailPart2', ee_email_part_2),
                   DocuSignRadioTab('actively_at_work', self.data.get_actively_at_work()),
               ] + rider_tabs

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
            DocuSignTextTab(prefix + 'Address', address),
            DocuSignTextTab(prefix + 'City', data['city']),
            DocuSignTextTab(prefix + 'State', data['state']),
            DocuSignTextTab(prefix + 'Zip', data['zip']),
        ]

    def make_beneficiary_tabs(self, prefix, name, relationship, dob, ssn):

        return [
            DocuSignTextTab(prefix + "BeneFullName", name),
            DocuSignTextTab(prefix + "BeneAge", self.get_age_from_dob(dob)),
            DocuSignTextTab(prefix + "BeneRelationship", relationship),
            DocuSignTextTab(prefix + "BeneDOB", dob),
            DocuSignTextTab(prefix + "BeneSSN", self.format_ssn(ssn)),
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
            return "%d" % relativedelta(date.today(), dateutil_parse(dob)).years
        else:
            return ""

    def generate_SOH_tabs(self, prefix, soh_questions):
        "Statement of health radio button answers"

        radio_tabs = []
        for i, soh_question in enumerate(q for q in soh_questions if not q.get('is_spouse_only')):
            if soh_question['answer'] and soh_question['answer'].lower() == "no":
                radio_tabs.append(DocuSignRadioTab(prefix + "SOH" + str(i + 1), "no"))
            elif soh_question['answer'] and soh_question['answer'].lower() == "yes":
                radio_tabs.append(DocuSignRadioTab(prefix + "SOH" + str(i + 1), "yes"))

        return radio_tabs

    def generate_SOH_GI_tabs(self, prefix, soh_questions):
        tabs = []
        for i, soh_question in enumerate(q for q in soh_questions if not q.get('is_spouse_only')):
            if soh_question['answer'] and soh_question['answer'].upper() == "GI" or soh_question['answer'] == None:
                # GI - skip for now
                answer = "GI"
            else:
                answer = ""

            tabs.append(DocuSignTextTab("{prefix}SOH{i}gi".format(prefix=prefix, i=i + 1), answer))

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

    def get_product_type(self):
        "Usually FPPTI, but can be FPPCI if CI product"
        product_code = self.data.get_product_code()
        if product_code == 'FPPCI':
            return 'FPPCI'

        return "FPPTI"
