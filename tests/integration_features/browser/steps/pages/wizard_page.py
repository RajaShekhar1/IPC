
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

    EE_GENDER_MALE = 'input[type=radio][name=gender][value=male]'
    EE_GENDER_FEMALE = 'input[type=radio][name=gender][value=female]'

    SHOW_RATES_BTN = 'button.show-rates'

    NEXT_BTN = 'button.btn-next'

    EE_COVERAGE_SELECT = 'select.emp-coverage'
    SP_COVERAGE_SELECT = 'select.sp-coverage'
    CH_COVERAGE_SELECT = 'select.ch-coverage'

    REC_COVERAGE_GOOD = '.pricing-span.good .widget-main'
    REC_COVERAGE_BETTER = '.pricing-span.better .widget-main'
    REC_COVERAGE_BEST = '.pricing-span.best .widget-main'

    def test_navigation_succeeded(self):
        return EC.element_to_be_clickable((By.CSS_SELECTOR, self.EE_FIRST))

    def wait_until_loaded(self):
        try:
            WebDriverWait(self.browser, 10).until(self.test_navigation_succeeded())
        except TimeoutException:
            return False

        return True

    def fill_out_step1_data(self, **data):
        emp_text_fields = [
            (self.EE_FIRST, data.get('emp_first')),
            (self.EE_LAST, data.get('emp_last')),
        ]

        for field, value in emp_text_fields:
            if value:
                self.lookup(field).send_keys(value)

        # Handle birthdate specially due to mask
        if data.get('emp_birthdate'):
            val = data['emp_birthdate']
            script = """
            window.ui.employee().birthdate("{}");
            """.format(val)
            self.browser.execute_script(script)

        if data.get('is_married'):
            self.lookup(self.EE_MARRIED_CHECK).click()

        if data.get('include_children'):
            self.lookup(self.EE_INCLUDE_CHILDREN_CHECK).click()

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

    def get_all_no_buttons(self):
        return self.lookup_multiple("button.flagBtn.val_No")

    def select_no_for_all_questions(self):
        btns = self.get_all_no_buttons()
        for btn in btns:
            if btn.is_displayed():
                btn.click()

    def fill_out_step3_data(self, **kwargs):
        text_fields = [
            (self.EE_STREET1, kwargs.get('emp_street1')),
            (self.EE_STREET2, kwargs.get('emp_street2')),
            (self.EE_CITY, kwargs.get('emp_city')),
            (self.EE_ZIP, kwargs.get('emp_zip')),
        ]

        if kwargs.get('emp_gender'):
            if kwargs['emp_gender'].startswith('m'):
                self.lookup(self.EE_GENDER_MALE).click()
            else:
                self.lookup(self.EE_GENDER_FEMALE).click()

        "emp_state"



