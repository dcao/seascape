# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://docs.scrapy.org/en/latest/topics/item-pipeline.html


# useful for handling different item types with a single interface
from datetime import datetime
from dotenv import load_dotenv
from itemadapter import ItemAdapter
import logging
import psycopg2
from psycopg2.extras import execute_values

load_dotenv()

class CapesPipeline:
    def __init__(self):
        super(CapesPipeline, self).__init__()
        self.items = []
        self.item_cnt = 0

    def open_spider(self, spider):
        hostname = 'localhost'
        username = 'postgres'
        password = '' # your password
        database = 'seascape_dev'
        self.connection = psycopg2.connect(host=hostname, user=username, password=password, dbname=database)
        self.cur = self.connection.cursor()

    def close_spider(self, spider):
        self.insert_current_items()
        self.cur.close()
        self.connection.close()

    def process_item(self, item, spider):
        n = datetime.now()

        self.items.append((item['first'], item['last'], item['course'], item['section'], item['term'], item['title'], item['enrolled'], item['evals'], item['rec_class'], item['rec_instr'], item['grades_exp'], item['grades_rcv'], item['hours'], n, n))
        self.item_cnt += 1
        
        if self.item_cnt >= 1000:
            self.insert_current_items()
        return item

    def insert_current_items(self):
        items = self.items
        self.items = []
        self.item_cnt = 0
        self.insert_to_database(items)

    def insert_to_database(self, items):
        # logging.info("started 1000 item push")
        # first, we have to insert the instructor and the course, if needed.
        execute_values(self.cur, """
insert into courses (code, title, inserted_at, updated_at)
values %s
on conflict (code) do nothing
""", [(i[2], i[5], i[13], i[13]) for i in items])
        execute_values(self.cur, """
insert into instructors (first, last, inserted_at, updated_at)
values %s
on conflict (first, last) do nothing
""", [(i[0], i[1], i[13], i[13]) for i in items])
        # todo: on conflict overwrite - first scrape sparse, then overwrite with detailed scrape
        execute_values(self.cur, """
insert into cape_entries (instr_id, course_code, section, term, enrolled, evals, rec_class, rec_instr, grades_exp, grades_rcv, hours, inserted_at, updated_at)
values %s
on conflict (instr_id, course_code, section, term) do nothing
""", [(i[0], i[1], i[2], i[3], i[4], i[6], i[7], i[8], i[9], i[10], i[11], i[12], i[13], i[14]) for i in items], template="""
((select id from instructors where first=%s and last=%s), (select code from courses where code=%s), %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
""")
        self.connection.commit()
        # logging.info("finished 1000 item push")

class PrereqsPipeline:
    def __init__(self):
        super(PrereqsPipeline, self).__init__()
        self.items = []
        self.item_cnt = 0

    def open_spider(self, spider):
        hostname = 'localhost'
        username = 'postgres'
        password = '' # your password
        database = 'seascape_dev'
        self.connection = psycopg2.connect(host=hostname, user=username, password=password, dbname=database)
        self.cur = self.connection.cursor()

    def close_spider(self, spider):
        self.insert_current_items()
        self.cur.close()
        self.connection.close()

    def process_item(self, item, spider):
        n = datetime.now()

        self.items.append((item['first'], item['last'], item['course'], item['section'], item['term'], item['title'], item['enrolled'], item['evals'], item['rec_class'], item['rec_instr'], item['grades_exp'], item['grades_rcv'], item['hours'], n, n))
        self.item_cnt += 1
        
        if self.item_cnt >= 1000:
            self.insert_current_items()
        return item

    def insert_current_items(self):
        items = self.items
        self.items = []
        self.item_cnt = 0
        self.insert_to_database(items)

    def insert_to_database(self, items):
        # logging.info("started 1000 item push")
        # first, we have to insert the instructor and the course, if needed.
        execute_values(self.cur, """
insert into courses (code, title, inserted_at, updated_at)
values %s
on conflict (code) do nothing
""", [(i[2], i[5], i[13], i[13]) for i in items])
        execute_values(self.cur, """
insert into instructors (first, last, inserted_at, updated_at)
values %s
on conflict (first, last) do nothing
""", [(i[0], i[1], i[13], i[13]) for i in items])
        execute_values(self.cur, """
insert into cape_entries (instr_id, course_code, section, term, title, enrolled, evals, rec_class, rec_instr, grades_exp, grades_rcv, hours, inserted_at, updated_at)
values %s
on conflict (instr_id, course_code, section, term) do nothing
""", items, template="""
((select id from instructors where first=%s and last=%s), (select code from courses where code=%s), %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
""")
        self.connection.commit()
        # logging.info("finished 1000 item push")
