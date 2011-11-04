from selenium import webdriver
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.support.ui import WebDriverWait

address="http://localhost:8000"
waittime=20

### Generalized test functions
def registerUser(driver, path, data, response):
    driver.get(path)

    driver.find_element_by_partial_link_text("Register!").click()

    for element, value in data.iteritems():
        driver.find_element_by_id(element).send_keys(value)

    driver.find_element_by_xpath("//form[@id='register']/p[7]/button").click()

    # Loop until page has loaded completly (we'll get exceptions about stale
    # objects while it is not fully loaded).
    while True:
        try:
            ret = driver.find_element_by_class_name("success").text
            break
        except StaleElementReferenceException:
            continue
    assert response == ret

def login(driver, path, data, response):
    driver.get(path)

    for element, value in data.iteritems():
        driver.find_element_by_id(element).send_keys(value)

    driver.find_element_by_xpath("//form[@id='login']/p[4]/button").click()

    # Loop until page has loaded completly (we'll get exceptions about stale
    # objects while it is not fully loaded).
    while True:
        try:
            ret = driver.find_element_by_xpath("//div[@id='page']/h2").text
            break
        except StaleElementReferenceException:
            continue
    assert response == ret


### Tests
def registerUserTest(driver):
    data = {"reg_email": "a@a.com",
            "reg_fullname": "Aa Aa",
            "reg_nick": "aa",
            "reg_password": "1234",
            "reg_confirm_password": "1234"}
    response = "Registration successful. Please login to continue."
    registerUser(driver, address, data, response)

def loginTest(driver):
    data = {"login_nick": "aa",
            "login_password": "1234"}
    response = "Welcome to the game"
    login(driver, address, data, response)

# Create a new (shared among tests) browser instance
driver = webdriver.Chrome()
# Set the maximum time the script will wait to find an element before throwing
# an exception
driver.implicitly_wait(waittime)

# Keep the try/except blocks around the tests so that we can close the browser
# properly even when tests fail :-)
try:
    registerUserTest(driver)
    loginTest(driver)
except Exception, e:
    # Terminate the browser and re-throw the exception
    driver.quit()
    raise

driver.quit()
