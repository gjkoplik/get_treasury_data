
# Gary Koplik
# Winter, 2018
# download_securities.py

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
import time
import os
# import sys # for running just the python script with the security name
import pandas as pd

# function that downloads relevant Securities.csv file from Treasury Direct
# input: security of interest (str)
# with notation "4-week", "2-year", "10-year", etc
# saves file Securities.csv in same directory as this script
def download_dates(security = "2-year"):
    # get current directory
    orig_dir = os.getcwd()
    # get file to save in same directory as python script
    options = webdriver.ChromeOptions()
    prefs = {'download.default_directory' : orig_dir}
    options.add_experimental_option('prefs', prefs)

    # open chrome (note this is using driver 2.35)
    driver = webdriver.Chrome('./chromedriver/chromedriver.exe', chrome_options = options)
    # Navigate to URL
    driver.get("https://www.treasurydirect.gov/instit/annceresult/annceresult_query.htm")
    # pause to let load
    time.sleep(5)

    # find where to write security name
    # found the xpath of the object holding the text entry
    #   and then selected the only thing inside it "//*"
    security_name = driver.find_element(By.XPATH, '//*[@id="row00jqxgrid"]/div[3]//*')
    # click on it
    security_name.click()
    # pause a second
    time.sleep(1)
    # enter the security name of interest
    security_name.send_keys(security)
    # pause to let thing reload
    time.sleep(5)

    # scroll down to make number of row selection visible
    #   (doesn't run when not visible)
    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")

    # change the number of rows to 1000 to make sure we get all dates
    # (goes more than far enough to reach April, 2008 even for weekly T-bill auctions)
    # open the drop-down menu
    rows = driver.find_element(By.XPATH, '//*[@id="dropdownlistArrowgridpagerlistjqxgrid"]/div')
    # pause for a second
    time.sleep(1)
    rows.click()
    # pause for a second
    time.sleep(1)
    # click on 1000 rows
    thousand = driver.find_element(By.XPATH,'//*[@id="listitem3innerListBoxgridpagerlistjqxgrid"]/span')
    thousand.click()
    # pause to let things reload
    time.sleep(5)

    # save the file as a csv in same file as this file
    save_button = driver.find_element_by_id("csvExport")
    save_button.click()
    # pause to let save
    time.sleep(2)
    # exit the window
    driver.quit()
    return

if __name__ == "__main__":
    # for
    # download_dates(sys.argv[1])
    # grab the user input saved while running shell script
    user_input = pd.read_csv("user_input.csv", header = None)
    # only need the security name for the python script
    #   which is hard coded as element [0, 0]
    download_dates(user_input.iloc[:, 0].values[0])
