from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait

import time

def basicWebsocket(driver, path):
    testString = "Hello World from test!"
    driver.get(path)
    
    # Find the 'Connect' button and click it to connect
    connectButton = driver.find_element_by_id("cA")
    connectButton.click()
    
    # Chat
    inputField = driver.find_element_by_id("phrase")
    sendButton = driver.find_element_by_id("sendB")
    inputField.send_keys(testString)
    sendButton.click()
    
    try:
        # Check that we have received an echo
        xpath = "//div[@id='msgs']/span[1]"
        assert testString == driver.find_element_by_xpath(xpath).text
        
    finally:
        driver.quit()


# Create a new (shared among tests) browser instance
driver = webdriver.Chrome()

# Keep the try/except blocks around the tests so that we can close the browser
# properly even when tests fail :-)
try:
    # Run the test for websocket example
    basicWebsocket(driver, "http://localhost:8000/websockets_example.yaws")
except Exception, e:
    # Terminate the browser and re-throw the exception
    driver.quit()
    raise
