from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait

import time

def basicWebsocket(path): 
    testString = "Hello World from test!"
    # Create a new instance of the browser driver
    driver = webdriver.Chrome()
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

# Run the test for websocket example
basicWebsocket("http://localhost:8000/websockets_example.yaws")
