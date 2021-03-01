import csv
import scrapy
import re

## CONFIG
CLASS_CSV = "../../data/data.csv"
QTR       = "SP21"

# importing courses
class_names = []

with open(CLASS_CSV, 'r') as cf:
    reader = csv.DictReader(cf)
    for row in reader:
        cn = row["course"].replace(" ", "")
        if cn not in class_names:
            class_names.append(cn)

class PrereqsSpider(scrapy.Spider):
    name = "prereqs"
    start_urls = [f"https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesPreReq.htm?termCode={QTR}&courseId={c}" for c in class_names]

    def parse(self, response):
        raw_course = response.css("h1::text").get().split(",")[0].split()[-1]
        course = re.sub(r"([0-9]+(\.[0-9]+)?)", r" \1", raw_course)

        cs = []

        for x in response.css("table")[1].css("tr")[1:]:
            raw_ors = x.css("span::text").getall()[::2]

            ors = [re.sub(r"([0-9]+(\.[0-9]+)?)", r" \1", c.strip()) for c in raw_ors]

            cs.append(ors)

        yield {
            "course": course,
            "prereqs": cs,
        }
