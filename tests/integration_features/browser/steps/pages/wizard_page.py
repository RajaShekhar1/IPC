
import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class WizardPage(PageBase):
    EE_FIRST = '#eeBenefitFName'
    EE_LAST = '#eeBenefitLName'
    EE_BIRTHDATE = '#eeDOB'
    EE_SSN = '#eessn'
    EE_MARRIED_CHECK = '.is_married'
    EE_INCLUDE_CHILDREN_CHECK = '.include_children'

    EE_GENDER_MALE = 'input[type=radio,name=gender,value=male]'
    EE_GENDER_FEMALE = 'input[type=radio,name=gender,value=female]'

    EE_STREET_1 = "#eeStreet1"
    EE_STREET_2 = "#eeStreet2"
    EE_CITY = "#eeCity"
    EE_STATE_SELECT = '#eeState'
    EE_ZIP = "#eeZip"
    EE_EMAIL = "#email"
    EE_PHONE = "#phone"

    EE_BENE_TYPE_OTHER_RADIO = "#eeBeneOther"
    EE_BENE_NAME = "#eeBeneOtherName"
    EE_BENE_REL = "#eeBeneOtherRelation"
    EE_BENE_SSN = "#eeBeneOtherSSN"
    EE_BENE_BIRTHDATE = "#eeBeneOtherDOB"

    EE_CONT_TYPE_NONE = "input[name=eeContBeneficiary,value=none]"
    EE_CONT_TYPE_SPOUSE = "input[name=eeContBeneficiary,value=spouse]"
    EE_CONT_TYPE_OTHER = "input[name=eeContBeneficiary,value=other]"
    EE_CONT_BENE_NAME = "#eeContBeneOtherName"
    EE_CONT_BENE_REL = "#eeContBeneOtherRelation"
    EE_CONT_BENE_SSN = "#eeContBeneOtherSSN"
    EE_CONT_BENE_BIRTHDATE = "#eeContBeneOtherDOB"

    SP_FIRST = '#spFName'
    SP_LAST = '#spLName'
    SP_SSN = '#spssn'
    SP_GENDER_MALE = 'input[type=radio,name=spGender,value=male]'
    SP_GENDER_FEMALE = 'input[type=radio,name=spGender,value=female]'
    SP_ADDR_SAME_ATTR = 'is_spouse_address_same_as_employee'
    SP_EMAIL_SAME_ATTR = 'is_spouse_email_same_as_employee'
    SP_STREET_1 = "#spStreet1"
    SP_STREET_2 = "#spStreet2"
    SP_CITY = "#spCity"
    SP_STATE_SELECT = "#spState"

    CH1_FIRST = '.child-0 .child_first'
    CH1_LAST = '.child-0 .child_last'

    CH2_FIRST = '.child-1 .child_first'
    CH2_LAST = '.child-1 .child_last'

    SHOW_RATES_BTN = 'button.show-rates'

    NEXT_BTN = 'button.btn-next'

    EE_COVERAGE_SELECT = 'select.emp-coverage'
    SP_COVERAGE_SELECT = 'select.sp-coverage'
    CH_COVERAGE_SELECT = 'select.ch-coverage'

    REC_COVERAGE_GOOD = '.pricing-span.good .widget-main'
    REC_COVERAGE_BETTER = '.pricing-span.better .widget-main'
    REC_COVERAGE_BEST = '.pricing-span.best .widget-main'

    ENROLL_CITY = "#enrollCity"
    ACK_DISCLOSURE = "#confirmDisclaimer"
    ACK_PAYROLL_DEDUCTION = "#confirmPayrollDeductions"
    def test_navigation_succeeded(self):
        return EC.element_to_be_clickable((By.CSS_SELECTOR, self.EE_FIRST))

    def wait_until_loaded(self):
        try:
            WebDriverWait(self.browser, 10).until(self.test_navigation_succeeded())
        except TimeoutException:
            return False

        return True

    def fill_out_step1_data(self, **data):
        self.enter_emp_data(data)
        self.enter_spouse_data(data)
        self.enter_children_data(data)

    def enter_children_data(self, data):
        ch_fields = [
            (self.CH1_FIRST, data.get('ch1_first')),
            (self.CH1_LAST, data.get('ch1_last')),
            (self.CH2_FIRST, data.get('ch2_first')),
            (self.CH2_LAST, data.get('ch2_last')),
        ]
        if data.get('include_children'):
            self.lookup(self.EE_INCLUDE_CHILDREN_CHECK).click()
        time.sleep(1)
        self.enter_text_data_if_provided(ch_fields)
        for child_num in range(1, 6 + 1):
            if data.get('ch{}_birthdate'.format(child_num)):
                self.enter_child_val_js(child_num - 1, "birthdate", data['ch{}_birthdate'.format(child_num)])

    def enter_spouse_data(self, data):
        sp_fields = [
            (self.SP_FIRST, data.get('sp_first')),
            (self.SP_LAST, data.get('sp_last')),
        ]
        if data.get('is_married'):
            self.lookup(self.EE_MARRIED_CHECK).click()
        time.sleep(1)
        self.enter_text_data_if_provided(sp_fields)
        if data.get('sp_birthdate'):
            self.enter_spouse_val_js("birthdate", data['sp_birthdate'])

    def enter_emp_data(self, data):
        text_fields = [
            (self.EE_FIRST, data.get('emp_first')),
            (self.EE_LAST, data.get('emp_last')),
        ]
        self.enter_text_data_if_provided(text_fields)
        # Handle birthdate specially due to mask
        if data.get('emp_birthdate'):
            self.enter_employee_val_js("birthdate", data['emp_birthdate'])

    def enter_text_data_if_provided(self, text_fields):
        for field, value in text_fields:
            if value:
                self.lookup(field).send_keys(value)

    def enter_employee_val_js(self, attr, val):
        self.browser.execute_script("""
            window.ui.employee().{0}("{1}");
            """.format(attr, val)
        )

    def enter_spouse_val_js(self, attr, val):
        self.browser.execute_script("""
            window.ui.spouse().{0}("{1}");
            """.format(attr, val)
        )

    def enter_child_val_js(self, child_num, attr, val):
        self.browser.execute_script("""
            window.ui.children()[{0}].{1}("{2}");
            """.format(child_num, attr, val)
        )


    def click_show_rates(self):
        btn = self.lookup(self.SHOW_RATES_BTN)
        btn.click()

    def select_recommended_coverage(self, recommendation):
        coverage_divs = {
            'good': self.REC_COVERAGE_GOOD,
            'better': self.REC_COVERAGE_BETTER,
            'best': self.REC_COVERAGE_BEST,
        }

        self.click_show_rates()
        self.wait_for_rate_table()
        el = self.lookup(coverage_divs[recommendation])
        el.click()

    def wait_for_rate_table(self):
        return self.wait_until_condition(EC.element_to_be_clickable((By.CSS_SELECTOR, self.REC_COVERAGE_BETTER)))

    def fill_out_fpp_wizard_with_basic_data(self):
        self.fill_out_step1_data(**dict(
            employee_first='Joe',
            employee_last='Smith',
            employee_birthdate='01311980',
            is_married=False,
            include_children=False,
        ))

    def select_custom_coverage(self, applicant, amount):
        self.click_show_rates()
        self.wait_for_rate_table()

        if applicant == 'employee':
            el = self.lookup(self.EE_COVERAGE_SELECT)

            # TODO: get child with matching amount as value and select it

    def click_next(self):
        btn = self.lookup(self.NEXT_BTN)
        btn.click()

    def wait_until_condition(self, condition):
        try:
            WebDriverWait(self.browser, 10).until(condition)
        except TimeoutException:
            return False

        return True

    def wait_until_step_2_loaded(self):
        return self.wait_until_condition(EC.element_to_be_clickable((By.CSS_SELECTOR, "button.flagBtn")))

    def wait_until_step_3_loaded(self):
        return self.wait_until_condition(EC.element_to_be_clickable((By.CSS_SELECTOR, "#eessn")))

    def wait_until_step_4_loaded(self):
        return self.wait_until_condition(EC.visibility_of_element_located((By.CSS_SELECTOR, "#step4")))

    def wait_until_step_5_loaded(self):
        return self.wait_until_condition(EC.visibility_of_element_located((By.CSS_SELECTOR, "#step5")))

    def wait_until_step_6_loaded(self):
        return self.wait_until_condition(EC.visibility_of_element_located((By.CSS_SELECTOR, "#step6")))

    def wait_until_docusign_redirect(self):
        return self.wait_until_condition(lambda browser: "docusign" in browser.current_url)

    def get_all_no_buttons(self):
        return self.lookup_multiple("button.flagBtn.val_No")

    def select_no_for_all_questions(self):
        btns = self.get_all_no_buttons()
        for btn in btns:
            if btn.is_displayed():
                btn.click()

    def fill_out_step3_data(self, **kwargs):
        text_fields = [
            (self.EE_STREET_1, kwargs.get('emp_street1')),
            (self.EE_STREET_2, kwargs.get('emp_street2')),
            (self.EE_CITY, kwargs.get('emp_city')),
            (self.EE_EMAIL, kwargs.get('emp_email')),
            (self.EE_PHONE, kwargs.get('emp_phone'))
        ]

        self.enter_text_data_if_provided(text_fields)

        if kwargs.get('emp_gender'):
            if kwargs['emp_gender'].lower().startswith('m'):
                gender_val = 'male'
            else:
                gender_val = 'female'
            self.enter_employee_val_js("gender", gender_val)

        if kwargs.get('emp_zip'):
            self.enter_employee_val_js("zip", kwargs['emp_zip'])

        # Select state
        if kwargs.get('emp_state'):
            el = self.lookup("option[value={}]".format(kwargs['emp_state']),
                             root_element=self.lookup(self.EE_STATE_SELECT))
            el.click()


    def fill_out_step_4_data(self, **kwargs):
        text_fields = [
            (self.SP_SSN, kwargs.get('sp_ssn')),
        ]
        self.enter_text_data_if_provided(text_fields)

        if kwargs.get('sp_gender'):
            if kwargs['sp_gender'].lower().startswith('m'):
                gender_val = 'male'
            else:
                gender_val = 'female'
            self.enter_spouse_val_js("gender", gender_val)

    def fill_out_step_5_data(self, **kwargs):

        ee_bene_other_text_fields = [
            (self.EE_BENE_NAME, "bene_name"),
            (self.EE_BENE_REL, "bene_relationship"),
            (self.EE_BENE_SSN, "bene_ssn"),
            (self.EE_BENE_BIRTHDATE, "bene_birthdate"),
        ]

        # If any of these are provided, select other
        if filter(lambda t: kwargs.get(t[1]), ee_bene_other_text_fields):
            self.browser.execute_script("""
            window.ui.employee_beneficiary_type("other")
            """)

        self.enter_text_data_if_provided(ee_bene_other_text_fields)

    def fill_out_step_6_data(self, **kwargs):
        if 'date_of_hire' in kwargs:
            self.browser.execute_script("""
            window.ui.identityToken("{}")
            """.format(kwargs['date_of_hire']))

        if kwargs.get('ack_benefit_disclosure', '').upper() == 'Y':
            self.lookup(self.ACK_DISCLOSURE).click()

        if kwargs.get('enroll_city'):
            self.lookup(self.ENROLL_CITY).send_keys(kwargs['enroll_city'])

        if kwargs.get('authorize_payroll_deduction'):
            el = self.lookup(self.ACK_PAYROLL_DEDUCTION, none_if_missing=True)
            if el and el.is_displayed():
                el.click()







