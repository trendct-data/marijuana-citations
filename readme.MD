
# Analyzing marijuana citations in Connecticut

This repository contains source data and analysis code for the Trend CT story:

*  [Who in CT is being cited for marijuana possession and by whom?](http://trendct.org/2016/09/26/where-connecticut-residents-have-been-arrested-the-most-for-marijuana-poessession/).

# Walkthroughs

* Marijuana citations in Connecticut analysis- [[HTML](http://trendct-data.github.io/marijuana-citations/)][[RMarkdown](https://github.com/trendct-data/marijuana-citations/blob/master/index.Rmd)] 

### The Data

The data used in this analysis (marijuana citations between 2011 and 2014) is from the Connecticut State Police via a FOIA request made by [Evan Anderson](https://www.muckrock.com/news/archives/2015/apr/29/new-york-still-throes-reefer-madness/) via [Muckrock.com](https://www.muckrock.com/foi/connecticut-53/connecticut-marijuana-citation-data-2011-2014-16701/).

* Data on citations reported to the National Incident-Based Reporting System (includes details such as age, race, and gender)
* Data on citations reported to the FBI's Uniform Crime Reporting system.

### What's in this repo

* `data` [folder]((https://github.com/trendct-data/marijuana-citations/data)) with raw and cleaned/transformed data.
* `analysis.R` is an R script analyzing the datasets and exploratory visualizations, as well as scripts to port the graphics to Trend CT's interactives generator (which only works with the login credentials).
* `.png and svg` files of various chart outputs for use on TrendCT.org.

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct-data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.
0Looking
0
