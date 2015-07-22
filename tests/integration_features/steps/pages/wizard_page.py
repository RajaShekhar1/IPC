
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

    SHOW_RATES_BTN = 'button.show-rates'

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
            (self.EE_FIRST, data.get('employee_first')),
            (self.EE_LAST, data.get('employee_last')),
            (self.EE_BIRTHDATE, data.get('employee_birthdate')),
            #(self.EE_SSN, data.get('employee_ssn')),
        ]

        for field, value in emp_text_fields:
            if value:
                print("Filling out {} with {}".format(field, value))
                self.lookup(field).send_keys(value)

        if data.get('is_married'):
            self.lookup(self.EE_MARRIED_CHECK).click()

        if data.get('include_children'):
            self.lookup(self.EE_INCLUDE_CHILDREN_CHECK).click()

    def fill_out_fpp_wizard_with_basic_data(self):
        self.fill_out_step1_data(**dict(
            employee_first='Joe',
            employee_last='Smith',
            employee_birthdate='01311980',
            is_married=False,
            include_children=False,
        ))

