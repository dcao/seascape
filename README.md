# seascape

UCSD offers a tool called [CAPE](http://cape.ucsd.edu), which allows students to report
feedback on their course at the end of each term. However, the site has some
issues which make the data harder to understand. Most notably, there's no
context for any of the numbers, so it's pretty hard to compare classes and
professors.

Seascape (Super Extra Awesome Spicier CAPE) is a solution to this, providing
visualizations and extra statistics to make CAPE data easier to understand
and compare.

For more info, see [the blogpost](https://cao.st/posts/seascape).

## Scraping data

Seascape relies on scraping data from the CAPE website for its function; this
can be accomplished with the `scrape-sparse` executable. There are two ways of
running it:

- Without an argument, during which it will use Selenium to navigate to the
  correct page and scrape the data (you will need to log into your UCSD
  account during the process).
  
- With a file argument pointing to the downloaded CAPE Results page, where
  the data can be scraped without Selenium.

## Acknowledgments

- CAPE for the original data

- [Smarter CAPEs](http://smartercapes.com) for the concept & original scraping code

This project was originally created for [SPIS 2019](https://sites.google.com/a/eng.ucsd.edu/spis/home).

## License

MIT.
