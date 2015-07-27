import time

from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC

from page import PageBase, PageError


class LoginPage(PageBase):
    URL = lambda s: '/login'

    USERNAME_INPUT = '#login'
    PASSWORD_INPUT = '#password'
    LOGIN_BUTTON = 'button[type=submit]'

    #def __init__(self, context, http_scheme='http', hostname='localhost:5000'):
    #    super(LoginPage, self).__init__(context, http_scheme=http_scheme, hostname=hostname)

    def test_navigation_succeeded(self):
        return EC.title_contains("Login")
        
    def get_base_url(self):
        return "{}://{}".format(self.http_scheme, self.hostname)
        
    def get_url(self):
        return "{}{}".format(self.get_base_url(), self.URL())

    def login_as_agent(self, agent):
        username_element = self.lookup(self.USERNAME_INPUT)
        username_element.send_keys(agent.email)

        password_element = self.lookup(self.PASSWORD_INPUT)
        password_element.send_keys("12121212")

        submit_button = self.lookup(self.LOGIN_BUTTON)
        submit_button.click()

        time.sleep(.25)

    def get_visible_entries(self):
        entries = []
        for entry_element in self.browser.find_elements_by_css_selector(self.ENTRY_WRAPPER):
            desc = self.lookup(self.ENTRY_DESCRIPTION)
            dept = self.lookup(self.ENTRY_DEPT)
            date = self.lookup(self.ENTRY_DATE)
            start = self.lookup(self.ENTRY_START)
            end = self.lookup(self.ENTRY_END)
            hours = self.lookup(self.ENTRY_HOURS)
            
            entries.append(dict(
                description=desc.text.strip(), 
                client_department=dept.text.strip(),
                date=date.text.strip(), 
                start=start.text.strip(),
                end=end.text.strip(),
                hours=hours.text.strip(),
            ))
            
        return entries
