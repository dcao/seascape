from dotenv import load_dotenv
import os
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait as wait
from selenium.webdriver.support import expected_conditions as EC
import scrapy

load_dotenv()

class CapesSpider(scrapy.Spider):
    name = "capes"
    cookies = {}

    def start_requests(self):
        # CAPEs requires us to be logged in before scraping.
        # Thus, using Selenium, we log in, wait for the 2FA authentication,
        # collect the cookies, then use those cookies to let our
        # spider do the work.
        options = webdriver.FirefoxOptions()
        # options.set_headless()
        driver = webdriver.Firefox(firefox_options=options)

        driver.get("https://cape.ucsd.edu/responses/Results.aspx?Name=%2C")
        # driver.get("https://cape.ucsd.edu/responses/Results.aspx?Name=Gillespie")
        wait(driver, 10).until(EC.title_contains("SINGLE SIGN-ON"))
        username = driver.find_element_by_xpath('//*[@id="ssousername"]')
        password = driver.find_element_by_xpath('//*[@id="ssopassword"]')

        username.send_keys(os.environ['CAPE_USER'])
        password.send_keys(os.environ['CAPE_PASS'])

        driver.find_element_by_xpath("/html/body/main/div/section/div[2]/div/div[1]/div/div[2]/form/button").click()

        wait(driver, 300).until(EC.title_contains("CAPE"))
        wait(driver, 20).until(
            EC.presence_of_element_located((By.ID, "ContentPlaceHolder1_gvCAPEs_hlViewReport_0"))
        )

        links = driver.find_elements_by_xpath('//td/a[@href]')

        for v in driver.get_cookies():
            self.cookies[v['name']] = v['value']

        for link in links:
            url = link.get_attribute("href")
            yield scrapy.Request(url=url, callback=self.parse, cookies=self.cookies)

    def parse(self, response):
        pass
