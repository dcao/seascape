"""scrape_cape.py: CAPE scraping tools.

In order to get all of the data we want, we need to scrape through existing CAPE
data. CAPE doesn't provide a public-facing API, so we have to get our data
this way.

Much of this code is heavily adapted from Andrey Portnoy's "Smarter CAPEs"
project. The original URL of the file this code is based on is:
https://github.com/andportnoy/smartercapes.com/blob/master/tools.py
"""

import pandas as pd
from natsort import natsorted
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from statsmodels.stats.proportion import proportion_confint as ci

def generate_sparse_df():
    """Generates a "sparse" dataset - we only use the data from the CAPE table
    to generate our own table. Generation of detailed data is substantially
    slower, so we have this function to generate the minimum needed to render
    a table.
    """

    # launch browser using Selenium, need to have Firefox installed
    print('Opening a browser window...')
    driver = webdriver.Firefox()
    print('Browser window open, loading the page...')

    # We make a dummy request so that we can log in with our credentials
    driver.get('https://cape.ucsd.edu/responses/Results.aspx?Name=whatever')
    print('Please enter credentials...')

    # wait until credentials are entered
    wait = WebDriverWait(driver, 60)
    element = wait.until(expected_conditions.title_contains('CAPE Results'))

    # Now that we are signed in, actually get all the data
    # We search for the comma character ("%2C") since that matches all prof
    # names
    driver.get('https://cape.ucsd.edu/responses/Results.aspx?Name=%2C')
    print('Page loaded, parsing dataset with pandas...')

    # Read in the dataset from the html file
    df = pd.read_html(driver.page_source)[0]
    print('Dataset parsed, closing browser window.')

    # destroy driver instance
    driver.quit()

    return df

def get_clean_cape_dataframe(raw_cape_dataframe, terms=None):
    df = raw_cape_dataframe

    # only looking at evaluations from 15/16 and 16/17
    if terms:
        df = df[df.Term.isin(terms)]

    # subset the columns we need
    df = df[['Instructor', 'Course', 'Term', 'Evals Made', 'Rcmnd Class',
             'Rcmnd Instr', 'Study Hrs/wk', 'Avg Grade Expected',
             'Avg Grade Received']]

    # rename the columns for convenience
    df = df.rename(columns={
        'Instructor': 'instr', 'Course': 'course', 'Term': 'term',
        'Evals Made': 'evals', 'Rcmnd Class': 'rcmnd_class',
        'Rcmnd Instr': 'rcmnd_instr', 'Study Hrs/wk': 'time',
        'Avg Grade Expected': 'grade_expected',
        'Avg Grade Received': 'grade_actual'
    })

    # drop rows that have data missing
    df = df.dropna()

    # only need the courses which hade at least one evaluation made
    df = df[df['evals'] != 0]

    # split to get the dept + course code
    df.loc[:, 'course'] = df.course.str.split(' - ').apply(lambda x: x[0])

    # convert the recommendation percentages to float values
    # and resize to be in the interval [0, 1]:
    df.loc[:, 'rcmnd_instr'] = (df.rcmnd_instr
                                  .str.rstrip(' %')
                                  .astype('float')) / 100

    df.loc[:, 'rcmnd_class'] = (df.rcmnd_class
                                  .str.rstrip(' %')
                                  .astype('float')) / 100

    """We create a "weighted evals" column which contains the recommendation
    percentage multiplied by the number of evals, yielding the approximate
    number of positive recommendations. We round them to obtain integer values.
    The exact numbers are available for every course, but it would require
    scraping a lot of pages.  Maybe in the next iteration."""

    df['class_weighted_evals'] = ((df.evals * df.rcmnd_class).round()
                                                             .astype('int'))
    df['instr_weighted_evals'] = ((df.evals * df.rcmnd_instr).round()
                                                             .astype('int'))

    df['letter_expected'] = (df.grade_expected.str.split('(')
                                              .apply(lambda x: x[0]))
    df['gpa_expected'] = (df.grade_expected.str.split('(')
                                           .apply(lambda x: x[-1])
                                           .str.rstrip(')')
                                           .astype('float'))

    df['letter_actual'] = (df.grade_actual.str.split('(')
                                          .apply(lambda x: x[0]))
    df['gpa_actual'] = (df.grade_actual.str.split('(')
                                       .apply(lambda x: x[-1])
                                       .str.rstrip(')')
                                       .astype('float'))

    df = df.drop(['grade_expected', 'grade_actual'], axis=1)

    # set and reset index to build an incremental index that starts at 0
    df = df.set_index('instr').reset_index()

    return df

def generate_and_save_sparse():
    """This combines the two steps above and saves the output df in
    "data/sparse.h5."
    """

    df = generate_sparse_df()
    df = get_clean_cape_dataframe(df)
    df.to_hdf("./data/sparse.h5", key="df", mode="w")
