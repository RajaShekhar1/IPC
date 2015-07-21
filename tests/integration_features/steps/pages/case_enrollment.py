import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class CaseEnrollmentPage(PageBase):
    URL = lambda s, case_id: '/enrollment-case/{case_id}#enrollment'.format(case_id=case_id)

    ADD_ENROLLMENT_BUTTON = '#add-to-census-btn'
    NEW_ENROLLMENT_SSN_INPUT = '#add-census-ssn-input'
    ENROLLMENT_NEXT_BTN = '.success-buttons .btn-primary'


    #def __init__(self, context, http_scheme='http', hostname='localhost:5000'):
    #    super(LoginPage, self).__init__(context, http_scheme=http_scheme, hostname=hostname)

    def test_navigation_succeeded(self):
        return EC.title_contains("Agent Manage Case")
        
    def get_base_url(self):
        return "{}://{}".format(self.http_scheme, self.hostname)
        
    def get_url(self):
        return "{}{}".format(self.get_base_url(), self.URL(self.context.case.id))
    
    def add_enrollment(self, ssn):
        add_enrollment_button = self.lookup(self.ADD_ENROLLMENT_BUTTON)
        add_enrollment_button.click()

        WebDriverWait(self.browser, 2).until(EC.visibility_of(self.lookup(self.NEW_ENROLLMENT_SSN_INPUT)))

        time.sleep(.25)
        self.lookup(self.NEW_ENROLLMENT_SSN_INPUT).send_keys('123-12-1234')

        next_btn = self.lookup(self.ENROLLMENT_NEXT_BTN)
        next_btn.click()

        time.sleep(.25)
