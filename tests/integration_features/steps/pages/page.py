
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait

class PageError(Exception): pass

class PageBase(object):
    def __init__(self, context, http_scheme='http', hostname='localhost:5000'):
        self.browser = context.browser
        self.context = context
        self.http_scheme = http_scheme
        self.hostname = hostname

    def lookup(self, css_selector, root_element=None):
        if not root_element:
            root_element = self.browser
        return root_element.find_element(by=By.CSS_SELECTOR, value=css_selector)

    def navigate(self):
        self.browser.get(self.get_url())
        try:
            self.test_navigation_succeeded()
        except NoSuchElementException as e:
            return False

        try:
            WebDriverWait(self.browser, 2).until(self.test_navigation_succeeded())
        except TimeoutException as e:
            return False
        except NoSuchElementException as e:
            return False

        return True

    def test_navigation_succeeded(self):
        raise NotImplementedError("Override this method")

    def get_url(self):
        raise NotImplementedError("Override this method")