from flask import json
from taa.services.cases import CaseService
from unittest2 import TestCase
from mock import Mock

from taa import app
from tests.db_util import create_case

class TestWizardSubmission(TestCase):
    """
    Some regression tests that just pass data taken from a failed wizard submission that should now succeed.

    If these start failing due to changes in data structure, it is probably OK to remove them, otherwise
    the data can be tweaked.
    """

    def test_wizard_submission__succeeds(self):
        with app.test_client() as c:
            with c.session_transaction() as sess:
                sess['active_case_id'] = self.case_id

            res = c.post("/submit-wizard-data",
                         data=json.dumps(dict(wizard_results=self.wizard_data)),
                         content_type="application/json")
            self.assertEqual(res.status_code, 200)


    def test_wizard_submission__doesnt_crash_on_beneficiary(self):
        with app.test_client() as c:
            with c.session_transaction() as sess:
                sess['active_case_id'] =  self.case_id

            res = c.post("/submit-wizard-data",
                         data=json.dumps(dict(wizard_results=self.ee_cont_data)),
                         content_type="application/json")
            self.assertEqual(res.status_code, 200)

    def setUp(self):

        # Initialize the testing app
        app.config['TESTING'] = True
        self.app = app.test_client()

        self.case = create_case()
        self.case_id = self.case.id

        self.wizard_data = {
            u'employee_beneficiary_name': u'John Doe',

            u'replacement_policies': [
            {u'replaced_or_financing': None, u'insured': u'', u'policy_number': u'', u'replacement_reason': u'',
             u'name': u''}],
            u'spouse_contingent_beneficiary_type': u'none',
                            u'spouse_coverage': {u'face_value': 15030, u'premium': 25},
                            u'spouse_other_owner_ssn': u'',
                            u'agent_data': {u'case_id': self.case_id, u'state': u'IL', u'enroll_city': u'Lombard',
                                            u'spouse_questions': {u'1': [{
                                                u'question_text': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?',
                                                u'label': u'Spouse Treated 6 Months',
                                                u'is_spouse_only': True,
                                                u'is_ignored': True}, {
                                                u'question_text': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?',
                                                u'label': u'Spouse Disabled 6 Months',
                                                u'is_spouse_only': True,
                                                u'is_ignored': False}]},
                                            u'children_data': [{u'first': u'Diane', u'birthdate': u'05/28/2013',
                                                                u'existing_coverages': [], u'last': u'Flores'},
                                                               {u'first': u'Katherine', u'birthdate': u'08/21/2010',
                                                                u'existing_coverages': [], u'last': u'Taylor'}],
                                            u'company_name': u'TrainingCo 123', u'selected_riders': [],
                                            u'payment_mode': 12, u'payment_mode_choices': [
                                    {u'name': u'Monthly', u'mode': 12, u'immutable': True}],
                                            u'employee_data': {u'ssn': u'222226095', u'state': u'TX',
                                                               u'phone': u'0-(611)438-7571', u'zip': u'75323',
                                                               u'first': u'Rebecca',
                                                               u'street_address': u'889 Boyd Place',
                                                               u'email': u'ktaylor4@cloudflare.com', u'city': u'Dallas',
                                                               u'street_address2': u'', u'existing_coverages': [],
                                                               u'last': u'Wells', u'gender': u'female',
                                                               u'height': u'67', u'birthdate': u'05/12/1969',
                                                               u'is_smoker': False, u'weight': u'143'}, u'riders': [
                                    {u'code': u'AIR', u'restrict_to': [u'FPPTI'], u'enrollment_level': True,
                                     u'name': u'Automatic Increase Rider'}], u'group_number': u'654321',
                                            u'spouse_data': {u'ssn': u'111487778', u'state': u'NC',
                                                             u'phone': u'8-(837)235-7559', u'zip': u'27157',
                                                             u'first': u'Theresa', u'street_address': u'8 Truax Alley',
                                                             u'email': u'ktaylor4@narod.ru', u'city': u'Winston Salem',
                                                             u'street_address2': u'', u'existing_coverages': [],
                                                             u'last': u'Patterson', u'gender': u'female',
                                                             u'height': u'67', u'birthdate': u'12/01/1959',
                                                             u'is_smoker': False, u'weight': u'143'},
                                            u'is_in_person': True, u'health_questions': {u'1': [{
                                    u'question_text': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?',
                                    u'label': u'HIV/AIDS',
                                    u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u'Has any Applicant ever applied for and been rejected for life insurance?',
                                        u'label': u'Ever been rejected',
                                        u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u'Has any Applicant been hospitalized in the past 90 days?',
                                        u'label': u'Hospital 90 days',
                                        u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?',
                                        u'label': u'Heart',
                                        u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                                        u'label': u'Cancer',
                                        u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?',
                                        u'label': u'Respiratory',
                                        u'skip_if_coverage_at_most': None},
                                    {
                                        u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?',
                                        u'label': u'Liver',
                                        u'skip_if_coverage_at_most': None}]}},
                            u'spouse_other_owner_name': u'', u'is_employee_actively_at_work': False,
                            u'spouse_beneficiary_relationship': u'Brother', u'rider_data': {u'sp': [
                {u'code': u'AIR', u'restrict_to': [u'FPPTI'], u'enrollment_level': True,
                 u'name': u'Automatic Increase Rider'}], u'emp': [
                {u'code': u'AIR', u'restrict_to': [u'FPPTI'], u'enrollment_level': True,
                 u'name': u'Automatic Increase Rider'}]}, u'employee_other_owner_name': u'Tom Wells',
                            u'employee_beneficiary': u'spouse', u'is_spouse_address_same_as_employee': False,
                            u'replacement_is_terminating': None, u'spouse_owner': u'employee',
                            u'employee': {u'ssn': u'222226095', u'state': u'TX', u'phone': u'0-(611)438-7571',
                                          u'address2': u'', u'soh_questions': [
                                    {u'answer': None, u'label': u'Spouse Treated 6 Months', u'is_spouse_only': True,
                                     u'question': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?'},
                                    {u'answer': None, u'label': u'Spouse Disabled 6 Months', u'is_spouse_only': True,
                                     u'question': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?'},
                                    {u'answer': u'No', u'label': u'HIV/AIDS', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'},
                                    {u'answer': u'No', u'label': u'Ever been rejected', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant ever applied for and been rejected for life insurance?'},
                                    {u'answer': u'No', u'label': u'Hospital 90 days', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant been hospitalized in the past 90 days?'},
                                    {u'answer': u'No', u'label': u'Heart', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'},
                                    {u'answer': u'No', u'label': u'Cancer', u'is_spouse_only': False,
                                     u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?"},
                                    {u'answer': u'No', u'label': u'Respiratory', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'},
                                    {u'answer': u'No', u'label': u'Liver', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'}],
                                          u'address1': u'889 Boyd Place', u'first': u'Rebecca',
                                          u'email': u'ktaylor4@cloudflare.com', u'city': u'Dallas', u'weight': u'143',
                                          u'last': u'Wells', u'age': 55, u'gender': u'female', u'height': 67,
                                          u'birthdate': u'05/12/1960', u'is_smoker': False, u'zip': u'75323'},
                            u'method': u'in_person', u'did_decline': False, u'enrollState': u'IL',
                            u'spouse_beneficiary_name': u'John Wells', u'employee_beneficiary_relationship': u'Brother',
                            u'spouse_contingent_beneficiary': {}, u'payment_mode_text': u'monthly',
                            u'is_spouse_email_same_as_employee': False,
                            u'employee_contingent_beneficiary': {u'relationship': u'Brother', u'name': u'John Doe'},
                            u'has_spouse_been_treated_6_months': u'No',
                            u'spouse': {u'ssn': u'111487778', u'state': u'NC', u'phone': u'8-(837)235-7559',
                                        u'address2': u'', u'soh_questions': [
                                    {u'answer': u'No', u'label': u'HIV/AIDS', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'},
                                    {u'answer': u'No', u'label': u'Ever been rejected', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant ever applied for and been rejected for life insurance?'},
                                    {u'answer': u'No', u'label': u'Hospital 90 days', u'is_spouse_only': False,
                                     u'question': u'Has any Applicant been hospitalized in the past 90 days?'},
                                    {u'answer': u'No', u'label': u'Heart', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'},
                                    {u'answer': u'No', u'label': u'Cancer', u'is_spouse_only': False,
                                     u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?"},
                                    {u'answer': u'No', u'label': u'Resp', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'},
                                    {u'answer': u'No', u'label': u'Liver', u'is_spouse_only': False,
                                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'}],
                                        u'address1': u'8 Truax Alley', u'first': u'Tom', u'email': u'ktaylor4@narod.ru',
                                        u'city': u'Winston Salem', u'weight': u'143', u'last': u'Patterson', u'age': 52,
                                        u'gender': u'male', u'height': 67, u'birthdate': u'12/01/1962',
                                        u'is_smoker': False, u'zip': u'27157'}, u'replacement_using_funds': None,
                            u'payment_mode': 12, u'replacement_read_aloud': False, u'product_type': u'FPPTI',
                            u'existing_insurance': False, u'employee_coverage': {u'face_value': 26296, u'premium': 50},
                            u'spouse_beneficiary': u'spouse', u'identityToken': u'10/10/2010',
                            u'employee_other_owner_ssn': u'111-48-7778',
                            u'employee_contingent_beneficiary_type': u'none', u'product_data': {u'gi_criteria': [],
                                                                                                u'brochure_name': u'Family Protection Plan with Terminal Illness Benefit',
                                                                                                u'restricted_agents': [],
                                                                                                u'is_fpp_gov': False,
                                                                                                u'product_type': u'base',
                                                                                                u'visible_to_agents': True,
                                                                                                u'id': 1,
                                                                                                u'code': u'FPPTI',
                                                                                                u'is_guaranteed_issue': False,
                                                                                                u'name': u'Family Protection Plan - Terminal Illness',
                                                                                                u'bypassed_soh_questions': [],
                                                                                                u'base_product_type': u'FPPTI',
                                                                                                u'brochure_url': u'http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'},
                            u'employee_owner': u'self', u'child_coverages': [{}, {}], u'enrollCity': u'Lombard',
                            u'replacing_insurance': False, u'children': [
                {u'ssn': u'', u'state': u'', u'phone': u'', u'address2': u'', u'soh_questions': [
                    {u'answer': None, u'label': u'Spouse Treated 6 Months', u'is_spouse_only': True,
                     u'question': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?'},
                    {u'answer': None, u'label': u'Spouse Disabled 6 Months', u'is_spouse_only': True,
                     u'question': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?'},
                    {u'answer': u'GI', u'label': u'HIV/AIDS', u'is_spouse_only': False,
                     u'question': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'},
                    {u'answer': u'GI', u'label': u'Ever been rejected', u'is_spouse_only': False,
                     u'question': u'Has any Applicant ever applied for and been rejected for life insurance?'},
                    {u'answer': u'GI', u'label': u'Hospital 90 days', u'is_spouse_only': False,
                     u'question': u'Has any Applicant been hospitalized in the past 90 days?'},
                    {u'answer': u'GI', u'label': u'Heart', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'},
                    {u'answer': u'GI', u'label': u'Cancer', u'is_spouse_only': False,
                     u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?"},
                    {u'answer': u'GI', u'label': u'Respiratory', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'},
                    {u'answer': u'GI', u'label': u'Liver', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'}],
                 u'address1': u'', u'first': u'Diane', u'email': u'', u'city': u'', u'weight': None, u'last': u'Flores',
                 u'age': 2, u'gender': None, u'height': None, u'birthdate': u'05/28/2013', u'is_smoker': None,
                 u'zip': u''}, {u'ssn': u'', u'state': u'', u'phone': u'', u'address2': u'', u'soh_questions': [
                    {u'answer': None, u'label': u'Spouse Treated 6 Months', u'is_spouse_only': True,
                     u'question': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?'},
                    {u'answer': None, u'label': u'Spouse Disabled 6 Months', u'is_spouse_only': True,
                     u'question': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?'},
                    {u'answer': u'GI', u'label': u'HIV/AIDS', u'is_spouse_only': False,
                     u'question': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?'},
                    {u'answer': u'GI', u'label': u'Ever been rejected', u'is_spouse_only': False,
                     u'question': u'Has any Applicant ever applied for and been rejected for life insurance?'},
                    {u'answer': u'GI', u'label': u'Hospital 90 days', u'is_spouse_only': False,
                     u'question': u'Has any Applicant been hospitalized in the past 90 days?'},
                    {u'answer': u'GI', u'label': u'Heart', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?'},
                    {u'answer': u'GI', u'label': u'Cancer', u'is_spouse_only': False,
                     u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?"},
                    {u'answer': u'GI', u'label': u'Respiratory', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?'},
                    {u'answer': u'GI', u'label': u'Liver', u'is_spouse_only': False,
                     u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?'}],
                                u'address1': u'', u'first': u'Katherine', u'email': u'', u'city': u'', u'weight': None,
                                u'last': u'Taylor', u'age': 5, u'gender': None, u'height': None,
                                u'birthdate': u'08/21/2010', u'is_smoker': None, u'zip': u''}],
                            u'has_spouse_been_disabled_6_months': u'No', u'identityType': u'', u'health_questions': [{
                u'question_text': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?',
                u'label': u'Spouse Treated 6 Months',
                u'is_spouse_only': True,
                u'is_ignored': True},
                {
                    u'question_text': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?',
                    u'label': u'Spouse Disabled 6 Months',
                    u'is_spouse_only': True,
                    u'is_ignored': False},
                {
                    u'question_text': u'Has any Applicant been diagnosed or treated by a member of the medical profession, or tested positive for: Human Immunodeficiency Virus (HIV), Acquired Immune Deficiency Syndrome (AIDS), or AIDS-Related Complex (ARC)?',
                    u'label': u'HIV/AIDS',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u'Has any Applicant ever applied for and been rejected for life insurance?',
                    u'label': u'Ever been rejected',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u'Has any Applicant been hospitalized in the past 90 days?',
                    u'label': u'Hospital 90 days',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?',
                    u'label': u'Heart',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?",
                    u'label': u'Cancer',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?',
                    u'label': u'Respiratory',
                    u'skip_if_coverage_at_most': None},
                {
                    u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?',
                    u'label': u'Liver',
                    u'skip_if_coverage_at_most': None}]}


        self.ee_cont_data = {u'child_coverages': [], u'agent_data': {u'riders': [{u'code': u'AIR', u'name': u'Automatic Increase Rider', u'restrict_to': [u'FPPTI'], u'enrollment_level': True}], u'selected_riders': [], u'spouse_questions': {u'1': [{u'is_spouse_only': True, u'question_text': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?', u'is_ignored': True, u'label': u'Spouse Treated 6 Months'}, {u'is_spouse_only': True, u'question_text': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?', u'is_ignored': False, u'label': u'Spouse Disabled 6 Months'}]}, u'is_in_person': True, u'payment_mode_choices': [{u'mode': 52, u'name': u'Weekly', u'immutable': True}], u'case_id': self.case_id, u'company_name': u'Test Postal', u'health_questions': {u'1': [{u'question_text': u'Has any Applicant tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?', u'skip_if_coverage_at_most': None, u'label': u'HIV/AIDS'}, {u'question_text': u'Has any Applicant ever applied for and been rejected for life insurance?', u'skip_if_coverage_at_most': None, u'label': u'Ever been rejected'}, {u'question_text': u'Has any Applicant been hospitalized in the past 90 days?', u'skip_if_coverage_at_most': None, u'label': u'Hospital 90 days'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?', u'skip_if_coverage_at_most': None, u'label': u'Heart'}, {u'question_text': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?", u'skip_if_coverage_at_most': None, u'label': u'Cancer'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?', u'skip_if_coverage_at_most': None, u'label': u'Respiratory'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?', u'skip_if_coverage_at_most': None, u'label': u'Liver'}]}, u'enroll_city': u'Tampa', u'group_number': u'0', u'children_data': [], u'payment_mode': 52, u'spouse_data': {u'city': None, u'street_address2': None, u'first': None, u'weight': None, u'gender': u'', u'is_smoker': None, u'zip': None, u'existing_coverages': [], u'phone': None, u'ssn': None, u'last': None, u'height': None, u'state': None, u'email': None, u'street_address': None, u'birthdate': u''}, u'employee_data': {u'city': None, u'street_address2': None, u'first': None, u'weight': None, u'gender': u'', u'is_smoker': None, u'zip': None, u'existing_coverages': [], u'phone': None, u'ssn': u'111111111', u'last': None, u'height': None, u'state': None, u'email': None, u'street_address': None, u'birthdate': u''}, u'state': u'FL'}, u'replacement_policies': [{u'insured': u'Test', u'replacement_reason': u'This is a reason', u'name': u'Test Carrier', u'replaced_or_financing': u'financing', u'policy_number': u'0123'}], u'spouse': {u'address1': None, u'city': None, u'address2': None, u'first': u'Jane', u'weight': None, u'gender': u'female', u'zip': None, u'is_smoker': None, u'soh_questions': [{u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?', u'label': u'HIV/AIDS'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant ever applied for and been rejected for life insurance?', u'label': u'Ever been rejected'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant been hospitalized in the past 90 days?', u'label': u'Hospital 90 days'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?', u'label': u'Heart'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?", u'label': u'Cancer'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?', u'label': u'Respiratory'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?', u'label': u'Liver'}], u'phone': None, u'ssn': None, u'age': 33, u'last': u'Doe', u'height': None, u'state': u'', u'email': None, u'birthdate': u'02/02/1982'}, u'employee_owner': u'self', u'product_type': u'FPPTI', u'spouse_beneficiary_relationship': u'sister', u'spouse_beneficiary': u'other', u'replacement_is_terminating': True, u'replacement_using_funds': False, u'payment_mode': 52, u'employee': {u'address1': None, u'city': u'Tampa', u'address2': None, u'first': u'John', u'weight': None, u'gender': u'male', u'zip': u'87654', u'is_smoker': None, u'soh_questions': [{u'answer': None, u'is_spouse_only': True, u'question': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?', u'label': u'Spouse Treated 6 Months'}, {u'answer': None, u'is_spouse_only': True, u'question': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?', u'label': u'Spouse Disabled 6 Months'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?', u'label': u'HIV/AIDS'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant ever applied for and been rejected for life insurance?', u'label': u'Ever been rejected'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'Has any Applicant been hospitalized in the past 90 days?', u'label': u'Hospital 90 days'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?', u'label': u'Heart'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?", u'label': u'Cancer'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?', u'label': u'Respiratory'}, {u'answer': u'No', u'is_spouse_only': False, u'question': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?', u'label': u'Liver'}], u'phone': u'(800) 555-1212', u'ssn': u'111111111', u'age': 35, u'last': u'Doe', u'height': None, u'state': u'FL', u'email': u'no@thankyou.com', u'birthdate': u'01/01/1980'}, u'rider_data': {u'sp': [{u'code': u'AIR', u'name': u'Automatic Increase Rider', u'restrict_to': [u'FPPTI'], u'enrollment_level': True}], u'emp': []}, u'spouse_coverage': {u'face_value': 50000, u'premium': 5.43}, u'employee_coverage': {u'face_value': 125000, u'premium': 13.55}, u'did_decline': False, u'employee_other_owner_name': u'', u'employee_contingent_beneficiary_type': u'other', u'existing_insurance': False, u'is_spouse_email_same_as_employee': True, u'spouse_beneficiary_dob': u'03/03/1983', u'employee_contingent_beneficiary': {u'name': u'Fred Jones', u'relationship': u'brother'}, u'employee_beneficiary': u'spouse', u'replacement_read_aloud': False, u'has_spouse_been_treated_6_months': u'No', u'spouse_contingent_beneficiary_type': u'spouse', u'has_spouse_been_disabled_6_months': u'No', u'identityType': u'', u'is_spouse_address_same_as_employee': True, u'spouse_beneficiary_name': u'Mary Jones', u'spouse_owner': u'employee', u'spouse_contingent_beneficiary': {}, u'payment_mode_text': u'weekly', u'employee_other_owner_ssn': u'', u'identityToken': u'10/10/2012', u'product_data': {u'is_guaranteed_issue': False, u'restricted_agents': [], u'bypassed_soh_questions': [], u'brochure_name': u'Family Protection Plan with Terminal Illness Benefit', u'visible_to_agents': True, u'name': u'Family Protection Plan - Terminal Illness', u'gi_criteria': [], u'is_fpp_gov': False, u'product_type': u'base', u'code': u'FPPTI', u'id': 1, u'base_product_type': u'FPPTI', u'brochure_url': u'http://5starlifeinsurance.com/wp-content/uploads/2015/02/5Star-Life-FPP-TI-brochure.pdf'}, u'spouse_other_owner_name': u'', u'children': [], u'enrollCity': u'Tampa', u'spouse_other_owner_ssn': u'', u'health_questions': [{u'is_spouse_only': True, u'question_text': u'During the prior 6 months, other than for routine medical care, has your spouse been diagnosed or treated by a member of the medical profession in a hospital or any other medical facility?', u'is_ignored': True, u'label': u'Spouse Treated 6 Months'}, {u'is_spouse_only': True, u'question_text': u'Has <span data-bind="$root.spouse().name"></span> been <a href="#modal-disabled-definition" data-toggle="modal">disabled</a> in the prior 6 months or received disability payments?', u'is_ignored': False, u'label': u'Spouse Disabled 6 Months'}, {u'question_text': u'Has any Applicant tested positive for exposure to the HIV infection or been diagnosed as having ARC or AIDS caused by the HIV infection or other sickness or condition derived from such infection?', u'skip_if_coverage_at_most': None, u'label': u'HIV/AIDS'}, {u'question_text': u'Has any Applicant ever applied for and been rejected for life insurance?', u'skip_if_coverage_at_most': None, u'label': u'Ever been rejected'}, {u'question_text': u'Has any Applicant been hospitalized in the past 90 days?', u'skip_if_coverage_at_most': None, u'label': u'Hospital 90 days'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Angina, heart attack, stroke, heart bypass surgery, angioplasty, coronary artery stenting, or coronary artery disease?', u'skip_if_coverage_at_most': None, u'label': u'Heart'}, {u'question_text': u"In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for any form of cancer to include leukemia or Hodgkin's Disease (excluding non-invasive, non-melanoma skin cancer)?", u'skip_if_coverage_at_most': None, u'label': u'Cancer'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Chronic obstructive pulmonary disease (COPD), emphysema, or any other chronic respiratory disorder, excluding asthma?', u'skip_if_coverage_at_most': None, u'label': u'Respiratory'}, {u'question_text': u'In the past 5 years, has any Applicant been hospitalized for, been diagnosed or treated by a licensed member of the medical profession or taken prescription medication for Alcoholism or drug or alcohol abuse, cirrhosis, hepatitis, or any other disease of the liver?', u'skip_if_coverage_at_most': None, u'label': u'Liver'}], u'is_employee_actively_at_work': True, u'method': u'in_person', u'enrollState': u'FL', u'replacing_insurance': True}

    def tearDown(self):
        pass
        #CaseService().delete_case(CaseService().get(self.case_id))