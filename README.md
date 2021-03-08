# seascape

UCSD offers multiple tools for finding information about courses. Among these
includes a tool for evaluating courses and instructors
([CAPE](http://cape.ucsd.edu)), and other tools which report course
prerequisites and registration info. This information is spread out across
multiple disparate sources and is hard to use for enrollment purposes.

Additionally, UCSD has internal APIs for accessing this information. However,
despite the fact that all this info is publicly available for all UCSD students,
getting programmatic access to this info is nontrivial at best.

**Seascape** (Super Extra Awesome Spicier CAPE) is a solution to this, providing
visualizations and extra statistics to make CAPE and course data easier to
understand and compare. Seascape also aims to provide an API for public UCSD
course info to allow other developers programmatic access to this information.

For more info, see [the original blogpost](https://cao.sh/posts/seascape).

## Scraping data

Seascape scrapes data from multiple sources scattered around UCSD's websites.
These scrapers are contained within the `seascrape` Scrapy project, and there
exists two spiders for scraping CAPE data and course prerequisite info.

## Acknowledgments

- CAPE for the original data

- [Smarter CAPEs](http://smartercapes.com) for the concept & original scraping code

This project was originally created for [SPIS 2019](https://sites.google.com/a/eng.ucsd.edu/spis/home).

## License

MIT.
