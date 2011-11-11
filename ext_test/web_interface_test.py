from selenium import webdriver
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait

import time
import os, base64

address="http://localhost:8000"
waittime=20

### Generalized test functions
def registerUser(driver, path, data, response):
    driver.get(path)

    driver.find_element_by_partial_link_text("Register").click()

    for element, value in data.iteritems():
        driver.find_element_by_id(element).send_keys(value)

    registerButton = "//form[@id='register_form']/div[@class='actions']/input[@class='btn primary']"
    driver.find_element_by_xpath(registerButton).click()

    # Loop until page has loaded completly (we'll get exceptions about stale
    # objects while it is not fully loaded).
    while True:
        try:
            ret = driver.find_element_by_xpath("//p[text()='"+response+"\n']").text
            break
        except StaleElementReferenceException:
            continue
    assert response == ret

def login(driver, path, data, response):
    driver.get(path)

    for element, value in data.iteritems():
        driver.find_element_by_id(element).send_keys(value)

    loginButton = "//form[@id='login_form']/button[@class='btn']"
    driver.find_element_by_xpath(loginButton).click()

    # Loop until page has loaded completly (we'll get exceptions about stale
    # objects while it is not fully loaded).
    while True:
        try:
            ret = driver.find_element_by_partial_link_text(response).text
            break
        except StaleElementReferenceException:
            continue
    assert response == ret


### Tests
def registerUserTest(driver):
    random_string = base64.urlsafe_b64encode(os.urandom(20))
    data = {"email": "a@a.com",
            "name": "Aa Aa",
            "nick": random_string,
            "password": "1234",
            "confirm_password": "1234"}
    response = "Registration was successful."
    registerUser(driver, address, data, response)
    return random_string

def loginTest(driver, nick):
    data = {"login_nick": nick,
            "login_password": "1234"}
    response = "Logout"
    login(driver, address, data, response)

# Create a new (shared among tests) browser instance
driver = webdriver.Chrome()
# Set the maximum time the script will wait to find an element before throwing
# an exception
driver.implicitly_wait(waittime)

# Keep the try/except blocks around the tests so that we can close the browser
# properly even when tests fail :-)
try:
    # Go to start page to load javascript before tests. Sleep afterwards since
    # we cannot know when all javascript files are fully loaded...
    driver.get(address)
    time.sleep(2)

    nick = registerUserTest(driver)
    loginTest(driver, nick)
except Exception, e:
    # Terminate the browser and re-throw the exception
    driver.quit()
    raise

driver.quit()
