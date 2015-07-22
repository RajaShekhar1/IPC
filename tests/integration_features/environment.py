
from selenium import webdriver

def before_all(context):
    desired_capabilities = webdriver.DesiredCapabilities.FIREFOX
    #desired_capabilities['version'] = '12'
    #desired_capabilities['platform'] = 'WINDOWS'
    desired_capabilities['name'] = 'TAA Testing Selenium 2 with Behave'
    #desired_capabilities['client_key'] = 'key'
    #desired_capabilities['client_secret'] = 'secret'

    context.browser = webdriver.Firefox(
        #desired_capabilities=desired_capabilities,
        #command_executor="http://hub.testingbot.com:4444/wd/hub"
    )

def after_all(context):
    context.browser.quit()