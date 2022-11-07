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

    custom_settings = {
        'ITEM_PIPELINES': {
            'seascrape.pipelines.CapesPipeline': 400
        }
    }

    def start_requests(self):
        # CAPEs requires us to be logged in before scraping.
        # Thus, using Selenium, we log in, wait for the 2FA authentication,
        # collect the cookies, then use those cookies to let our
        # spider do the work.
        options = webdriver.FirefoxOptions()
        # options.set_headless()
        driver = webdriver.Firefox(firefox_options=options)

        driver.get("https://cape.ucsd.edu/responses/Results.aspx?Name=asdasd")
        # driver.get("https://cape.ucsd.edu/responses/Results.aspx?Name=Gillespie")
        wait(driver, 10).until(EC.title_contains("SINGLE SIGN-ON"))
        username = driver.find_element_by_xpath('//*[@id="ssousername"]')
        password = driver.find_element_by_xpath('//*[@id="ssopassword"]')

        username.send_keys(os.environ['CAPE_USER'])
        password.send_keys(os.environ['CAPE_PASS'])

        driver.find_element_by_xpath("//button[contains(@class, 'btn-primary')]").click()

        wait(driver, 300).until(EC.title_contains("CAPE"))

        for v in driver.get_cookies():
            self.cookies[v['name']] = v['value']

        yield scrapy.Request(url="https://cape.ucsd.edu/responses/Results.aspx?Name=%2C", callback=self.parse, cookies=self.cookies)

        driver.quit()

    def parse(self, response):
        if response.request.url == "https://cape.ucsd.edu/responses/Results.aspx?Name=%2C":
            # uncomment for non-sparse scraping
            # for url in response.xpath("//td/a/@href").extract():
            #     yield scrapy.Request(url=f"https://cape.ucsd.edu/responses/{url}", callback=self.parse, cookies=self.cookies)

            for row in response.css("tbody tr"):
                instr = row.css("td::text")[0].get().split(',', 1)
                first = instr[1].strip()
                last = instr[0].strip()
                course, rest = row.css("td a::text").get().split(' - ', 1)
                section = rest[-2]
                term = row.css("td::text")[3].get()
                title = rest[:-4]
                enrolled = row.css("td::text")[4].get()
                
                others = row.css("td span::text").getall()

                evals = int(others[0])
                rec_class = int(evals * float(others[1][:-1]) / 100)
                rec_instr = int(evals * float(others[2][:-1]) / 100)
                hours = [float(others[3])] if others[3] != 'N/A' else []
                grades_exp = [float(others[4][others[4].find('(')+1:others[4].find(')')])] if others[4] != 'N/A' else []
                grades_rcv = [float(others[5][others[5].find('(')+1:others[5].find(')')])] if others[5] != 'N/A' else []
                
                yield {
                    'first': first,
                    'last': last,

                    # course info - used to get a ref to a course id
                    'course': course,
                    'title': title,

                    # course info attached to this entry
                    'section': section,
                    'term': term,
                    'enrolled': enrolled,

                    # eval info attached to this entry
                    'evals': evals,
                    'rec_class': rec_class,
                    'rec_instr': rec_instr,
                    'grades_exp': grades_exp,
                    'grades_rcv': grades_rcv,
                    'hours': hours,
                }
        elif "CAPEReport" in response.request.url:
            # new-style cape report
            instr, course, title = response.css("#ContentPlaceHolder1_lblReportTitle::text").get().split(' - ')
            first = instr.split(', ', 1)[0]
            last = instr.split(', ', 1)[1]

            s = response.css("#ContentPlaceHolder1_lblCourseDescription::text").get()
            section = s[s.find("(")+1:s.find(")")]
            term = response.css("#ContentPlaceHolder1_lblTermCode::text").get()
            enrolled = response.css("#ContentPlaceHolder1_lblEnrollment::text").get()
            evals = response.css("#ContentPlaceHolder1_lblEvaluationsSubmitted::text").get()

            rec_class = response.css("#ContentPlaceHolder1_lblRecommendCourse::text").get()
            rec_instr = response.css("#ContentPlaceHolder1_lblRecommendInstructor::text").get()

            grades_exp = response.css("table#ContentPlaceHolder1_tblExpectedGrades td::text").getall()[0:7]
            grades_rcv = response.css("table#ContentPlaceHolder1_tblGradesReceived td::text").getall()[0:7]
            hours = [response.css(f"#ContentPlaceHolder1_dlQuestionnaire_rptChoices_6_rbSelect_{x}::text").get() for x in range(0, 11)]

            yield {
                'first': first,
                'last': last,

                # course info - used to get a ref to a course id
                'course': course,
                'title': title,

                # course info attached to this entry
                'section': section,
                'term': term,
                'enrolled': enrolled,

                # eval info attached to this entry
                'evals': evals,
                'rec_class': rec_class,
                'rec_instr': rec_instr,
                'grades_exp': grades_exp,
                'grades_rcv': grades_rcv,
                'hours': hours,
            }
        else:
            # old-style cape report
            # for now, ignore
            pass
