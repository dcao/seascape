import csv
import psycopg2
import scrapy
import re

## CONFIG
QTR       = "SP21"

def class_names():
    hostname = 'localhost'
    username = 'postgres'
    password = '' # your password
    database = 'seascape_dev'

    connection = psycopg2.connect(host=hostname, user=username, password=password, dbname=database)
    cursor = self.connection.cursor()

    cursor.execute("select code from courses")
    codes = cursor.fetchall()

    cursor.close()
    connection.close()
    print("PostgreSQL connection is closed")

    return [c[0].replace(' ', '') for c in codes]

class PrereqsSpider(scrapy.Spider):
    name = "prereqs"
    start_urls = [f"https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesPreReq.htm?termCode={QTR}&courseId={c}" for c in class_names()]

    def parse(self, response):
        raw_course = response.css("h1::text").get().split(",")[0].split()[-1]
        course = re.sub(r"([0-9]+(\.[0-9]+)?)", r" \1", raw_course)

        cs = []

        for x in response.css("table")[1].css("tr")[1:]:
            raw_ors = x.css("span::text").getall()[::2]
            raw_descs = [n.strip()[1:-2].strip() for n in x.xpath(".//span/following-sibling::text()")[::3].getall()]

            ors = [{"course": re.sub(r"([0-9]+(\.[0-9]+)?)", r" \1", c.strip()), "desc": n} for (c, n) in zip(raw_ors, raw_descs)]

            cs.append(ors)

        yield {
            "course": course,
            "prereqs": cs,
        }
