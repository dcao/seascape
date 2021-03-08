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

        self.items.append((item['instr'], item['course'], item['section'], item['term'], item['title'], item['enrolled'], item['evals'], item['rec_class'], item['rec_instr'], item['grades_exp'], item['grades_rcv'], item['hours'], n, n))
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
        execute_values(self.cur, """
insert into cape_entries (instr, course, section, term, title, enrolled, evals, rec_class, rec_instr, grades_exp, grades_rcv, hours, inserted_at, updated_at)
values %s
on conflict (instr, course, section, term) do nothing
""", items)
        self.connection.commit()
        # logging.info("finished 1000 item push")
