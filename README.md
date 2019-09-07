# seascape

UCSD offers a tool called [CAPE](cape.ucsd.edu), which allows students to report
feedback on their course at the end of each term. However, the site has some
issues which make the data harder to understand. Most notably, there's no
context for any of the numbers, so it's pretty hard to compare classes and
professors.

Seascape (Super Extra Awesome Spicy CAPE) is a solution to this, providing
visualizations and extra statistics to make CAPE data easier to understand
and compare.

## Scraping data

Seascape relies on scraping data from the CAPE website for its function; this
can be accomplished with `seascape/scrape_cape.py`. Keep in mind that the script
requires you to log in with your UCSD account.
