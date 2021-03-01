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
            raw_descs = [n.strip()[1:-2].strip() for n in x.xpath(".//span/following-sibling::text()")[::3].getall()]

            ors = [{"course": re.sub(r"([0-9]+(\.[0-9]+)?)", r" \1", c.strip()), "desc": n} for (c, n) in zip(raw_ors, raw_descs)]

            cs.append(ors)

        yield {
            "course": course,
            "prereqs": cs,
        }
