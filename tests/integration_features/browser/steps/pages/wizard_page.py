import time

from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait

from page import PageBase


class WizardPage(PageBase):
    EE_FIRST = '#eeBenefitFName'
    EE_LAST = '#eeBenefitLName'
    EE_BIRTHDATE = '#eeDOB'
    EE_SSN = '#eessn'
    EE_MARRIED_CHECK = '.is_married'
    EE_INCLUDE_CHILDREN_CHECK = '.include_children'
    EE_ACTIVELY_AT_WORK_YES = '.aaw-yes'
    EE_ACTIVELY_AT_WORK_NO = '.aaw-no'

    EE_GENDER_MALE = 'input[type=radio,name=gender,value=male]'
    EE_GENDER_FEMALE = 'input[type=radio,name=gender,value=female]'

    EE_STREET_1 = "#eeStreet1"
    EE_STREET_2 = "#eeStreet2"
    EE_CITY = "#eeCity"
    EE_STATE_SELECT = '#eeState'
    EE_ZIP = "#eeZip"
    EE_EMAIL = "#email"
    EE_PHONE = "#phone"

    EE_BENE_TYPE_OTHER_RADIO = "input[name=eeBeneficiary,value=other]"
    EE_BENE_TYPE_SPOUSE_RADIO = "input[name=eeBeneficiary,value=spouse]"

    EE_BENE_NAME = ".ee-bene-name"
    EE_BENE_REL = ".ee-bene-rel"
    EE_BENE_SSN = ".ee-bene-ssn"
    EE_BENE_BIRTHDATE = ".ee-bene-dob"

    EE_CONT_TYPE_NONE = "input[name=eeContBeneficiary,value=none]"
    EE_CONT_TYPE_SPOUSE = "input[name=eeContBeneficiary,value=spouse]"
    EE_CONT_TYPE_OTHER = "input[name=eeContBeneficiary,value=other]"
    EE_CONT_BENE_NAME = ".ee-cont-bene-name"
    EE_CONT_BENE_REL = ".ee-cont-bene-rel"
    EE_CONT_BENE_SSN = ".ee-cont-bene-ssn"
    EE_CONT_BENE_BIRTHDATE = ".ee-cont-bene-dob"

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

    SP_BENE_TYPE_OTHER_RADIO = "input[name=spBeneficiary,value=other]"
    SP_BENE_TYPE_SPOUSE_RADIO = "input[name=spBeneficiary,value=spouse]"
    SP_BENE_NAME = ".sp-bene-name"
    SP_BENE_REL = ".sp-bene-rel"
    SP_BENE_SSN = ".sp-bene-ssn"
    SP_BENE_DOB = ".sp-bene-dob"
    SP_CONT_TYPE_NONE = "input[name=spContBeneficiary,value=none]"
    SP_CONT_TYPE_SPOUSE = "input[name=spContBeneficiary,value=spouse]"
    SP_CONT_TYPE_OTHER = "input[name=spContBeneficiary,value=other]"
    SP_CONT_BENE_NAME = ".sp-cont-bene-name"
    SP_CONT_BENE_REL = ".sp-cont-bene-rel"
    SP_CONT_BENE_SSN = ".sp-cont-bene-ssn"
    SP_CONT_BENE_DOB = ".sp-cont-bene-dob"

    CH1_FIRST = '.child-0 .child_first'
    CH1_LAST = '.child-0 .child_last'

    CH2_FIRST = '.child-1 .child_first'
    CH2_LAST = '.child-1 .child_last'

    SHOW_RATES_BTN = 'button.show-rates'

    NEXT_BTN = 'button.btn-next'

    EE_COVERAGE_SELECT = 'select.emp-coverage'
    SP_COVERAGE_SELECT = 'select.sp-coverage'
    CH_COVERAGE_SELECT = 'select.ch-coverage'

    REC_COVERAGE_GOOD = '.pricing-span.good .widget-box'
    REC_COVERAGE_BETTER = '.pricing-span.better .widget-box'
    REC_COVERAGE_BEST = '.pricing-span.best .widget-box'

    ENROLL_CITY = "#enrollCity"
    ACK_DISCLOSURE = "#confirmDisclaimer"
    ACK_PAYROLL_DEDUCTION = "#confirmPayrollDeductions"

    def test_navigation_succeeded(self):
        return EC.visibility_of_element_located((By.CSS_SELECTOR, '.page-content'))

    def wait_until_loaded(self):
        try:
            WebDriverWait(self.browser, 10).until(self.test_navigation_succeeded())
        except TimeoutException:
            return False

        return True

    def fill_out_step1_data(self, **data):
        time.sleep(1)

        self.enter_emp_data(data)
        self.enter_spouse_data(data)
        self.enter_children_data(data)

    def enter_children_data(self, data):
        ch_fields = [
            (self.CH1_FIRST, 'ch1_first'),
            (self.CH1_LAST, 'ch1_last'),
            (self.CH2_FIRST, 'ch2_first'),
            (self.CH2_LAST, 'ch2_last'),
        ]
        if data.get('include_children') == 'Y':
            self.lookup(self.EE_INCLUDE_CHILDREN_CHECK).click()
        time.sleep(1)
        self.enter_text_data_if_provided(ch_fields, data)
        for child_num in range(1, 6 + 1):
            if data.get('ch{}_birthdate'.format(child_num)):
                self.enter_child_val_js(child_num - 1, "birthdate", data['ch{}_birthdate'.format(child_num)])

    def enter_spouse_data(self, data):
        sp_fields = [
            (self.SP_FIRST, 'sp_first'),
            (self.SP_LAST, 'sp_last'),
        ]
        if data.get('is_married'):
            self.lookup(self.EE_MARRIED_CHECK).click()
        time.sleep(1)
        self.enter_text_data_if_provided(sp_fields, data)
        if data.get('sp_birthdate'):
            self.enter_spouse_val_js("birthdate", data['sp_birthdate'])

    def enter_emp_data(self, data):
        text_fields = [
            (self.EE_FIRST, 'emp_first'),
            (self.EE_LAST, 'emp_last'),
        ]
        self.enter_text_data_if_provided(text_fields, data)

        # Handle birthdate specially due to mask
        if data.get('emp_birthdate'):
            self.enter_employee_val_js("birthdate", data['emp_birthdate'])

        if data.get('actively_at_work'):
            self.lookup(self.EE_ACTIVELY_AT_WORK_YES).click()
        elif 'actively_at_work' in data and not data['actively_at_work']:
            self.lookup(self.EE_ACTIVELY_AT_WORK_NO).click()

    def enter_text_data_if_provided(self, text_fields, data):
        for vals in text_fields:
            if len(vals) == 2:
                field, value_key = vals
                js_observable_name = None
            else:
                field, value_key, js_observable_name = vals

            value = data.get(value_key)

            if value and not js_observable_name:
                for ele in self.lookup_multiple(field):
                    ele.send_keys(value)
                self.browser.execute_script("""
                    $('{selector}').blur();
                """.format(selector=field))
            elif value and js_observable_name:
                self.browser.execute_script("""
                window.vm.{0}("{1}")
                """.format(js_observable_name, value))
            time.sleep(0.25)
            self.lookup('body').click()
            time.sleep(0.25)

    def enter_employee_val_js(self, attr, val):
        self.browser.execute_script("""
            window.vm.employee().{0}("{1}");
            """.format(attr, val)
                                    )

    def enter_spouse_val_js(self, attr, val):
        self.browser.execute_script("""
            window.vm.spouse().{0}("{1}");
            """.format(attr, val)
                                    )

    def enter_child_val_js(self, child_num, attr, val):
        self.browser.execute_script("""
            window.vm.children()[{0}].{1}("{2}");
            """.format(child_num, attr, val)
                                    )

    def click_show_rates(self):
        self.lookup('button.show-rates').click()
        # self.browser.execute_script("$('button.show-rates').click();")

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
            actively_at_work=True,
        ))

    def select_custom_coverage(self, applicant, amount):
        self.click_show_rates()

        self.wait_for_rate_table()

        if applicant == 'employee':
            el = self.lookup(self.EE_COVERAGE_SELECT)

            # TODO: get child with matching amount as value and select it

    def click_next(self):
        self.browser.execute_script("""$('button.btn.btn-success.btn-next').click()""")
        # btn = self.lookup(self.NEXT_BTN)
        # btn.click()

    def wait_until_condition(self, condition, timeout=10):
        try:
            WebDriverWait(self.browser, timeout).until(condition)
        except TimeoutException:
            return False

        return True

    def wait_until_step_1_loaded(self):
        return self.wait_until_condition(EC.element_to_be_clickable((By.CSS_SELECTOR, '#show-coverage-options')))

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
        return self.wait_until_condition(lambda browser: "docusign" in browser.current_url, timeout=20)

    def get_all_no_buttons(self):
        return self.lookup_multiple("button.val_No")

    def select_no_for_all_questions(self):
        self.browser.execute_script("""$('button:contains(No):visible').click()""")
        # btns = self.get_all_no_buttons()
        # for btn in btns:
        #     if btn.is_displayed():
        #         btn.click()

    def fill_out_step3_data(self, **kwargs):
        text_fields = [
            (self.EE_STREET_1, 'emp_street1'),
            (self.EE_STREET_2, 'emp_street2'),
            (self.EE_CITY, 'emp_city'),
            (self.EE_EMAIL, 'emp_email'),
            (self.EE_PHONE, 'emp_phone')
        ]

        self.enter_text_data_if_provided(text_fields, kwargs)

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
            (self.SP_SSN, 'sp_ssn'),
        ]
        self.enter_text_data_if_provided(text_fields, kwargs)

        if kwargs.get('sp_gender'):
            if kwargs['sp_gender'].lower().startswith('m'):
                gender_val = 'male'
            else:
                gender_val = 'female'
            self.enter_spouse_val_js("gender", gender_val)

    def fill_out_step_5_data(self, **kwargs):

        ee_bene_other_text_fields = [
            (self.EE_BENE_NAME, "ee_bene_name"),
            (self.EE_BENE_REL, "ee_bene_relationship"),
        ]
        sp_bene_other_text_fields = [
            (self.SP_BENE_NAME, "sp_bene_name"),
            (self.SP_BENE_REL, "sp_bene_relationship"),
        ]

        ee_cont_bene_other_text_fields = [
            (self.EE_CONT_BENE_NAME, "ee_cont_bene_name"),
            (self.EE_CONT_BENE_REL, "ee_cont_bene_relationship"),
        ]

        sp_cont_bene_other_text_fields = [
            (self.SP_CONT_BENE_NAME, "sp_cont_bene_name"),
            (self.SP_CONT_BENE_REL, "sp_cont_bene_relationship"),
        ]

        # If any of these are provided, select other
        if filter(lambda t: kwargs.get(t[1]), ee_bene_other_text_fields):
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.employee_beneficiary_type('other');
            });
            """)
        if filter(lambda t: kwargs.get(t[1]), sp_bene_other_text_fields):
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.spouse_beneficiary_type('other');
            });
            """)

        if filter(lambda t: kwargs.get(t[1]), ee_cont_bene_other_text_fields):
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.employee_contingent_beneficiary_type('other');
            });
            """)
        elif 'ee_cont_bene_type' in kwargs:
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.employee_contingent_beneficiary_type('{}');
            });
            """.format(kwargs['ee_cont_bene_type']))

        if filter(lambda t: kwargs.get(t[1]), sp_cont_bene_other_text_fields):
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.spouse_contingent_beneficiary_type('other');
            });
            """)
        elif 'sp_cont_bene_type' in kwargs:
            self.browser.execute_script("""
            _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
                product.spouse_contingent_beneficiary_type('{}');
            });
            """.format(kwargs['sp_cont_bene_type']))

        self.enter_text_data_if_provided(ee_bene_other_text_fields, kwargs)
        self.enter_text_data_if_provided(sp_bene_other_text_fields, kwargs)
        self.enter_text_data_if_provided(ee_cont_bene_other_text_fields, kwargs)
        self.enter_text_data_if_provided(sp_cont_bene_other_text_fields, kwargs)

        self.browser.execute_script("""
        _.forEach(window.vm.coverage_vm.selected_product_coverages(), function (product) {
            // Grab data for the employee's beneficiary data and put it into the input values
            $('.ee-bene-ssn').val('333333333');
            $('.ee-bene-dob').val('10/10/1990');
            $('.ee-bene-cont-ssn').val('444444444');
            $('.ee-bene-cont-dob').val('10/10/1990');

            // Grab data for the spouse's beneficiary data and put it into the input values
            $('.sp-bene-ssn').val('333333333');
            $('.sp-bene-dob').val('10/10/1990');
            $('.sp-bene-cont-ssn').val('444444444');
            $('.sp-bene-cont-dob').val('10/10/1990');
        });
        """)

    def fill_out_step_6_data(self, **kwargs):
        if 'date_of_hire' in kwargs:
            self.browser.execute_script("""
            window.vm.identityToken("{}")
            """.format(kwargs['date_of_hire']))

        if kwargs.get('ack_benefit_disclosure', '').upper() == 'Y':
            self.lookup(self.ACK_DISCLOSURE).click()

        if kwargs.get('enroll_city'):
            self.lookup(self.ENROLL_CITY).send_keys(kwargs['enroll_city'])

        if kwargs.get('authorize_payroll_deduction'):
            el = self.lookup(self.ACK_PAYROLL_DEDUCTION, none_if_missing=True)
            if el and el.is_displayed():
                el.click()

    def select_coverage_options(self, product_coverage_pairs):
        self.wait_until_recommended_coverage_visible()
        time.sleep(0.25)
        for product_code, coverage_type in product_coverage_pairs:
            coverage_header_id = '#coverage-header-{0}'.format(product_code.upper())
            recommended_coverage_div = 'div.pricing-span.{0}'.format(coverage_type.lower())
            self.lookup(coverage_header_id).click()
            self.lookup(recommended_coverage_div).click()
            time.sleep(0.25)
        self.click_next()

    def wait_until_recommended_coverage_visible(self):
        return self.wait_until_condition(
            EC.visibility_of_element_located((By.CSS_SELECTOR, '.recommended-coverage-table')))
