import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class CaseEnrollmentPage(PageBase):

    ADD_ENROLLMENT_BUTTON = '#add-to-census-btn'
    NEW_ENROLLMENT_SSN_INPUT = '#add-census-ssn-input'
    ENROLLMENT_NEXT_BTN = '.success-buttons .btn-primary'
    BEGIN_ENROLLMENT_BTN = '.success-buttons .btn-success'

    def test_navigation_succeeded(self):
        return EC.element_to_be_clickable([By.ID, self.ADD_ENROLLMENT_BUTTON])

    def get_base_url(self):
        return "{}://{}".format(self.http_scheme, self.hostname)

    def get_url_path(self):
        return '/enrollment-case/{0}#enrollment'.format(self.context.case.id)

    def get_url(self):
        return "{}{}".format(self.get_base_url(), self.get_url_path())

    def add_enrollment(self, ssn):
        add_enrollment_button = self.lookup(self.ADD_ENROLLMENT_BUTTON)
        add_enrollment_button.click()

        ssn_input = self.lookup(self.NEW_ENROLLMENT_SSN_INPUT)

        WebDriverWait(self.browser, 5).until(EC.visibility_of(ssn_input))

        ssn_input.click()
        ssn_input.send_keys(ssn)

        next_btn = self.lookup(self.ENROLLMENT_NEXT_BTN)
        next_btn.click()

        begin_enrollment_btn = self.lookup(self.BEGIN_ENROLLMENT_BTN)
        WebDriverWait(self.browser, 5).until(EC.visibility_of(begin_enrollment_btn))

        begin_enrollment_btn.click()
