import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class CasePage(PageBase):
    CANCELED_PROMPT = """return jQuery('h3:contains(canceled / declined)')"""
    SELF_ENROLL_LINK = "a[href*='/self-enroll/']"

    def test_navigation_succeeded(self):
        return EC.visibility_of_element_located([By.CSS_SELECTOR, self.SELF_ENROLL_LINK])

    def get_base_url(self):
        return "{}://{}".format(self.http_scheme, self.hostname)

    def get_url_path(self):
        return '/enrollment-case/{0}#setup'.format(self.context.case.id)

    def get_url(self):
        return "{}{}".format(self.get_base_url(), self.get_url_path())

    def click_self_enroll_url(self):
        self.lookup(self.SELF_ENROLL_LINK).click()

    def switch_to_popup_window(self):
        main_window = None
        while not main_window:
            main_window = self.browser.current_window_handle
        self.click_self_enroll_url()
        popup_window_handle = None
        while not popup_window_handle:
            for handle in self.browser.window_handles:
                if handle != main_window:
                    popup_window_handle = handle
                    break
        self.browser.switch_to.window(popup_window_handle)
        self.browser.maximize_window()

    def wait_until_redirected_declined(self):
        if self.browser.execute_script(self.CANCELED_PROMPT):
            return True
        else:
            return False
