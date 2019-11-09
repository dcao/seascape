# scrape_clist.py - scraping the UCSD course list.
from bs4 import BeautifulSoup
from collections import namedtuple, defaultdict
import itertools as it
import requests

NO_RESULT = "No Result Found. Try another search."

Time = namedtuple("Time", ["h", "m"])
TimeRange = namedtuple("TimeRange", ["start", "end"])
When = namedtuple("When", ["days", "time"])

def parse_time(s):
    hm = s.split(":")
    h = 12 + int(hm[0]) if hm[1][-1] == 'p' else int(hm[0])
    m = int(hm[1][:-1])

    return Time(h, m)

def parse_timerange(s):
    ss = s.split("-")
    start = parse_time(ss[0])
    end = parse_time(ss[1])

    return TimeRange(start, end)

def retrieve_clist(course_config):
    data = {
        "selectedTerm": "WI20",
        "xsoc_term": "",
        "loggedIn": "false",
        "tabNum": "tabs-crs",
        "_selectedSubjects": "1",
        "schedOption1": "true",
        "_schedOption1": "on",
        "_schedOption11": "on",
        "_schedOption12": "on",
        "schedOption2": "true",
        "_schedOption2": "on",
        "_schedOption4": "on",
        "_schedOption5": "on",
        "_schedOption3": "on",
        "_schedOption7": "on",
        "_schedOption8": "on",
        "_schedOption13": "on",
        "_schedOption10": "on",
        "_schedOption9": "on",
        "schDay": "M",
        "_schDay": "on",
        "schDay": "T",
        "_schDay": "on",
        "schDay": "W",
        "_schDay": "on",
        "schDay": "R",
        "_schDay": "on",
        "schDay": "F",
        "_schDay": "on",
        "schDay": "S",
        "_schDay": "on",
        "schStartTime": "12%3A00",
        "schStartAmPm": "0",
        "schEndTime": "12%3A00",
        "schEndAmPm": "0",
        "_selectedDepartments": "1",
        "schedOption1Dept": "true",
        "_schedOption1Dept": "on",
        "_schedOption11Dept": "on",
        "_schedOption12Dept": "on",
        "schedOption2Dept": "true",
        "_schedOption2Dept": "on",
        "_schedOption4Dept": "on",
        "_schedOption5Dept": "on",
        "_schedOption3Dept": "on",
        "_schedOption7Dept": "on",
        "_schedOption8Dept": "on",
        "_schedOption13Dept": "on",
        "_schedOption10Dept": "on",
        "_schedOption9Dept": "on",
        "schDayDept": "M",
        "_schDayDept": "on",
        "schDayDept": "T",
        "_schDayDept": "on",
        "schDayDept": "W",
        "_schDayDept": "on",
        "schDayDept": "R",
        "_schDayDept": "on",
        "schDayDept": "F",
        "_schDayDept": "on",
        "schDayDept": "S",
        "_schDayDept": "on",
        "schStartTimeDept": "12:00",
        "schStartAmPmDept": "0",
        "schEndTimeDept": "12:00",
        "schEndAmPmDept": "0",
        "courses": "\n".join([x["name"] for x in course_config]),
        "sections": "",
        "instructorType": "begin",
        "instructor": "",
        "titleType": "contain",
        "title": "",
        "_hideFullSec": "on",
        "_showPopup": "on",
    }

    s = requests.Session()
    r1 = s.post("https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesStudentResult.htm", data=data)
    soup = BeautifulSoup(r1.text)

    # First, check if we didn't match any classes
    alert = soup.find("div", class_="msg alert")
    if alert and alert.contents[0].strip() == NO_RESULT:
        return []

    # Next, we check to see if we need to get the "print-friendly" version; we only do this if there are multiple pages
    page_str = soup.find_all("table")[-1].find("td", align="right").contents[0].strip()[12:]
    multipage = page_str[:page_str.find(')')] == 1
    soup = soup if not multipage else BeautifulSoup(s.get("https://act.ucsd.edu/scheduleOfClasses/scheduleOfClassesStudentResultPrint.htm"))

    res = defaultdict(dict)

    courses = soup.find_all("td", class_="crsheader", colspan="5")
    for course in courses:
        num = course.find_previous("td").text
        cat = course.find_all_previous("span", class_="centeralign")[1].text
        cat = cat[cat.find("(")+1:cat.find(")")].strip()
        code = f"{cat} {num}"

        res[code]["units"] = int(course.contents[2].strip().split()[1])

        sec_required = []
        sec_choice = []
        nonenrs = []

        cur = course.find_parent("tr").next_sibling.next_sibling
        instructor = None
        while cur and cur.get("class") and "sectxt" in cur["class"]:
            meeting_dict = dict()
            meeting_dict["type"] = cur.contents[7].span["title"]
            meeting_dict["section"] = cur.contents[9].text.strip()
            meeting_dict["when"] = When(cur.contents[11].text.strip(), parse_timerange(cur.contents[13].text.strip()))
            meeting_dict["building"] = cur.contents[15].text.strip()
            meeting_dict["room"] = cur.contents[17].text.strip()
            instructor = instructor if instructor else cur.contents[19].text.strip()
            meeting_dict["instructor"] = instructor
            available = cur.contents[21].text.strip()
            meeting_dict["available"] = 0 if "FULL" in available else int(available) if available else None

            if meeting_dict["available"] is None:
                sec_required.append(meeting_dict)
            else:
                sec_choice.append(meeting_dict)
            
            cur = cur.next_sibling.next_sibling

        while cur and cur.get("class") and "nonenrtxt" in cur["class"]:
            meeting_dict = dict()
            meeting_dict["type"] = cur.contents[5].span["title"]
            meeting_dict["date"] = cur.contents[7].text.strip()
            meeting_dict["time"] = parse_timerange(cur.contents[11].text.strip())
            meeting_dict["building"] = cur.contents[13].text.strip()
            meeting_dict["room"] = cur.contents[15].text.strip()

            nonenrs.append(meeting_dict)
            
            cur = cur.next_sibling.next_sibling

        reqd = sec_required + nonenrs
        combos = []
        for opt in sec_choice:
            combo = []
            cf = next(x for x in course_config if x["name"] == code)
            if cf["lecture"]:
                combo += reqd
            if cf["other"]:
                combo += [opt]
            if combo:
                combos.append(combo)
            
        res[code].setdefault("sections", [])
        res[code]["sections"].extend(combos)

    return res

def ordered_insert(xs, elem):
    def go(xs, elem, start, end):
        if len(xs) == 0:
            xs.insert(0, elem)
            return 0
        
        if end >= start:
            xs.insert(start, elem)
            return start

        mid = xs[start + (end - start) / 2]
        if elem == mid:
            xs.insert(start, elem)
            return start + (end - start) / 2
        elif elem > mid:
            return go(xs, elem, mid + 1, end)
        else:
            return go(xs, elem, start, mid - 1)

    return go(xs, elem, 0, xs.len())
    

def is_overlapping(whens):
    ds = ["M", "Tu", "W", "Th", "F", "S"]
    times = {d: [] for d in ds}
    bools = {d: [] for d in ds}
    for when in whens:
        for d in ds:
            if d in when["days"]:
                six = ordered_insert(times[d], when["time"]["start"])
                eix = ordered_insert(times[d], when["time"]["end"])

                if six >= len(bools) or (bools[six] == False and bools[eix] == True):
                    bools[d].insert(six, False)
                    bools[d].insert(eix, True)
                else:
                    return True

    return False

def combinations(clist, min_units, max_units):
    # This is where we build all the possible combinations of courses.
    # To build combinations, we first get all combinations of course codes
    # which combine to min_units <= units <= max_units.
    # With each of those combinations, we generate all combinations of
    # courses, rejecting ones with overlapping times.
    valid = []
    for i in range(1, len(clist) + 1):
        combs = it.combinations(clist.keys(), i)
        for c in combs:
            units = sum([clist[x]["units"] for x in c])
            if units >= min_units and units <= max_units:
                valid.append(c)

    res = []
    for valid_combo in valid:
        res.append(it.starmap(it.chain, it.product(*[clist[x]["sections"] for x in valid_combo])))

    res = it.chain(*res)

    print(list([list(x) for x in res]))
    
    return [is_overlapping([y["when"] for y in x]) for x in res]

def schedule(sconf, cset, n=5):
    pass
