import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class SelfEnrollmentPage(PageBase):

    PROCEED_TO_ENROLLMENT_BTN = "button.btn.btn-lg.btn-primary"

    def test_navigation_succeeded(self):
        if self.browser.execute_script("""return jQuery('strong:contains(Proceed to enrollment)')"""):
            return True
        else:
            return False

    def get_url(self):
        return self.browser.getCurrentUrl()

    def click_self_enroll_btn(self):
        self.lookup(self.PROCEED_TO_ENROLLMENT_BTN).click()
