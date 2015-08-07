import os

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

class PageError(Exception): pass

class PageBase(object):
    def __init__(self, context, http_scheme='http', hostname='localhost:{}'.format(os.environ.get('SERVER_PORT', 5000))):
        self.browser = context.browser
        self.context = context
        self.http_scheme = http_scheme
        self.hostname = hostname

    def lookup(self, css_selector, root_element=None, wait=True):
        if wait:
            WebDriverWait(self.browser, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, css_selector)))

        if not root_element:
            root_element = self.browser
        return root_element.find_element(by=By.CSS_SELECTOR, value=css_selector)

    def lookup_multiple(self, css_selector, root_element=None, wait=True):
        if wait:
            WebDriverWait(self.browser, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, css_selector)))

        if not root_element:
            root_element = self.browser
        return root_element.find_elements_by_css_selector(css_selector)

    def navigate(self):
        self.browser.get(self.get_url())

        try:
            WebDriverWait(self.browser, 10).until(self.test_navigation_succeeded())
        except TimeoutException as e:
            return False
        except NoSuchElementException as e:
            return False

        return True

    def test_navigation_succeeded(self):
        raise NotImplementedError("Override this method")

    def get_url(self):
        raise NotImplementedError("Override this method")