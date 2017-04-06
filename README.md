# roadoi - Use oaDOI.org with R




[![Build Status](https://travis-ci.org/njahn82/roadoi.svg?branch=master)](https://travis-ci.org/njahn82/roadoi)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/njahn82/roadoi?branch=master&svg=true)](https://ci.appveyor.com/project/njahn82/roadoi)
[![codecov.io](https://codecov.io/github/njahn82/roadoi/coverage.svg?branch=master)](https://codecov.io/github/njahn82/roadoi?branch=master)
[![cran version](http://www.r-pkg.org/badges/version/roadoi)](https://cran.r-project.org/package=europepmc)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/roadoi)](https://github.com/metacran/cranlogs.app)

roadoi interacts with the [oaDOI API](http://oadoi.org/), a simple interface which links DOIs 
and open access versions of scholarly works. oaDOI powers [unpaywall](http://unpaywall.org/).

API Documentation: <http://oadoi.org/api>

## How do I use it? 

Use the `oadoi_fetch()` function in this package to get open access status
information and full-text links from oaDOI.


```r
roadoi::oadoi_fetch(dois = c("10.1038/ng.3260", "10.1093/nar/gkr1047"))
#> # A tibble: 2 × 16
#>                                      `_best_open_url` `_closed_base_ids`
#>                                                 <chr>             <list>
#> 1 http://nrs.harvard.edu/urn-3:HUL.InstRepos:25290367          <chr [1]>
#> 2                  http://doi.org/10.1093/nar/gkr1047         <list [0]>
#> # ... with 14 more variables: `_closed_urls` <list>,
#> #   `_open_base_ids` <list>, `_open_urls` <list>, doi <chr>,
#> #   doi_resolver <chr>, evidence <chr>, free_fulltext_url <chr>,
#> #   is_boai_license <lgl>, is_free_to_read <lgl>,
#> #   is_subscription_journal <lgl>, license <chr>, oa_color <chr>,
#> #   url <chr>, year <int>
```

There are no API restrictions. However, a rate limit of 100k is implemented. If you need to access more data, use the data dump <https://oadoi.org/api#dataset> instead.

## How do I get it? 

To install the development version, use the [devtools package](https://cran.r-project.org/package=devtools)


```r
devtools::install_github("njahn82/roadoi")
library(roadoi)
```

## Long-Form Documentation including use-case



Open access copies of scholarly publications are sometimes hard to find. Some are published in open access journals. Others are made freely available as preprints before publication, and others are deposited in institutional repositories, digital archives maintained by universities and research institutions. This document guides you to roadoi, a R client that makes it easy to search for these open access copies by interfacing the [oaDOI.org](https://oadoi.org/) service where DOIs are matched with full-text links in open access journals and archives.

### About oaDOI.org

[oaDOI.org](https://oadoi.org/), developed and maintained by the [team of Impactstory](https://oadoi.org/team), is a non-profit service that finds open access copies of scholarly literature simply by looking up a DOI (Digital Object Identifier). It not only returns open access full-text links, but also helpful metadata about the open access status of a publication such as licensing or provenance information.

oaDOI uses different data sources to find open access full-texts including:

- [Crossref](http://www.crossref.org/): a DOI registration agency serving major scholarly publishers.
- [Datacite](https://www.datacite.org/): another DOI registration agency with main focus on research data
- [Directory of Open Access Journals (DOAJ)](https://doaj.org/): a registry of open access journals
- [Bielefeld Academic Search Engine (BASE)](https://www.base-search.net/): an aggregator of various OAI-PMH metadata sources. OAI-PMH is a protocol often used by open access journals and repositories.

### Basic usage

There is one major function to talk with oaDOI.org, `oadoi_fetch()`.


```r
library(roadoi)
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9", "10.1016/j.cognition.2014.07.007"))
#> # A tibble: 2 × 16
#>                                       `_best_open_url` `_closed_base_ids`
#>                                                  <chr>             <list>
#> 1             http://doi.org/10.1186/s12864-016-2566-9         <list [0]>
#> 2 http://hdl.handle.net/11858/00-001M-0000-0024-2A9E-8          <chr [1]>
#> # ... with 14 more variables: `_closed_urls` <list>,
#> #   `_open_base_ids` <list>, `_open_urls` <list>, doi <chr>,
#> #   doi_resolver <chr>, evidence <chr>, free_fulltext_url <chr>,
#> #   is_boai_license <lgl>, is_free_to_read <lgl>,
#> #   is_subscription_journal <lgl>, license <chr>, oa_color <chr>,
#> #   url <chr>, year <int>
```

According to the [oaDOI.org API specification](https://oadoi.org/api), the following variables with the following definitions are returned:

* `_best_open_url`: Link to free full-text
* `_closed_base_ids`: Ids of oai metadata records with closed access full-text links collected by the [Bielefeld Academic Search Engine (BASE)](https://www.base-search.net/) 
* `_open_base_ids`: Ids of oai metadata records with open access full-text links collected by the [Bielefeld Academic Search Engine (BASE)](https://www.base-search.net/)
* `_open_urls`: full-text url
* `doi`: the requested DOI
* `doi_resolver`: Possible values:
    + crossref
    + datacite
* `evidence`: A phrase summarizing the step of the open access detection process where the `free_fulltext_url` was found.
* `free_fulltext_url`: The URL where we found a free-to-read version of the DOI. None when no free-to-read version was found.
* `is_boai_license`: TRUE whenever the license indications Creative Commons - Attribution (CC BY), Creative Commons  CC - Universal(CC 0)) or Public Domain were found. These permissive licenses comply with the highly-regarded [BOAI definition of Open access](http://www.budapestopenaccessinitiative.org/)
* `is_free_to_read`: TRUE whenever the free_fulltext_url is not None.
* `is_subscription_journal`: TRUE whenever the journal is not in the Directory of Open Access Journals or DataCite. Please note that there might be a time-lag between the first publication of an open access journal and its registration in the DOAJ.
* `license`: Contains the name of the [Creative Commons license](https://creativecommons.org/) associated with the `free_fulltext_url`, whenever one was found. Example: "cc-by".
* `oa_color`: Possible values:
    + green
    + gold
* `url`: the canonical DOI URL
* `year`: year of publication

Providing your email address when using this client is highly appreciated by oaDOI.org. It not only helps the maintainer of oaDOI.org, the non-profit [Impactstory](https://impactstory.org/), to inform you when something breaks, but also to demonstrate API usage to its funders. Simply use the `email` parameter for this purpose:


```r
roadoi::oadoi_fetch("10.1186/s12864-016-2566-9", email = "name@example.com")
#> # A tibble: 1 × 16
#>                           `_best_open_url` `_closed_base_ids`
#>                                      <chr>             <list>
#> 1 http://doi.org/10.1186/s12864-016-2566-9         <list [0]>
#> # ... with 14 more variables: `_closed_urls` <list>,
#> #   `_open_base_ids` <list>, `_open_urls` <list>, doi <chr>,
#> #   doi_resolver <chr>, evidence <chr>, free_fulltext_url <chr>,
#> #   is_boai_license <lgl>, is_free_to_read <lgl>,
#> #   is_subscription_journal <lgl>, license <chr>, oa_color <chr>,
#> #   url <chr>, year <int>
```

To follow your API call, and to estimate the time until completion, use the `.progress` parameter inherited from plyr to display a progress bar.


```r
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9", "10.1016/j.cognition.2014.07.007"), .progress = "text")
#>   |                                                                         |                                                                 |   0%  |                                                                         |================================                                 |  50%  |                                                                         |=================================================================| 100%
#> # A tibble: 2 × 16
#>                                       `_best_open_url` `_closed_base_ids`
#>                                                  <chr>             <list>
#> 1             http://doi.org/10.1186/s12864-016-2566-9         <list [0]>
#> 2 http://hdl.handle.net/11858/00-001M-0000-0024-2A9E-8          <chr [1]>
#> # ... with 14 more variables: `_closed_urls` <list>,
#> #   `_open_base_ids` <list>, `_open_urls` <list>, doi <chr>,
#> #   doi_resolver <chr>, evidence <chr>, free_fulltext_url <chr>,
#> #   is_boai_license <lgl>, is_free_to_read <lgl>,
#> #   is_subscription_journal <lgl>, license <chr>, oa_color <chr>,
#> #   url <chr>, year <int>
```

### Use Case: Studying the compliance with open access policies

An increasing number of universities, research organisations and funders have launched open access policies in recent years. Using roadoi together with other R-packages makes it easy to examine how and to what extent researchers comply with these policies in a reproducible and transparent manner. In particular, the [rcrossref package](https://github.com/ropensci/rcrossref), maintained by rOpenSci, provides many helpful functions for this task.

#### Gathering DOIs representing scholarly publications

DOIs have become essential for referencing scholarly publications, and thus many digital libraries and institutional databases keep track of these persistent identifiers. For the sake of this vignette, instead of starting with a pre-defined set of publications originating from these sources, we simply generate a random sample of 100 DOIs registered with Crossref by using the [rcrossref package](https://github.com/ropensci/rcrossref).


```r
library(dplyr)
library(rcrossref)
# get a random sample of DOIs and metadata describing these works
random_dois <- rcrossref::cr_r(sample = 100) %>%
  rcrossref::cr_works() %>%
  .$data
random_dois
#> # A tibble: 100 × 34
#>       alternative.id
#>                <chr>
#> 1                   
#> 2                   
#> 3  S1385894705004109
#> 4       BFjibs199240
#> 5                   
#> 6                   
#> 7                   
#> 8                   
#> 9                   
#> 10                  
#> # ... with 90 more rows, and 33 more variables: container.title <chr>,
#> #   created <chr>, deposited <chr>, DOI <chr>, funder <list>,
#> #   indexed <chr>, ISBN <chr>, ISSN <chr>, issued <chr>, link <list>,
#> #   member <chr>, page <chr>, prefix <chr>, publisher <chr>,
#> #   reference.count <chr>, score <chr>, source <chr>, subject <chr>,
#> #   title <chr>, type <chr>, URL <chr>, assertion <list>, author <list>,
#> #   `clinical-trial-number` <list>, issue <chr>, volume <chr>,
#> #   license_date <chr>, license_URL <chr>, license_delay.in.days <chr>,
#> #   license_content.version <chr>, archive <chr>, subtitle <chr>,
#> #   update.policy <chr>
```

Let's see when these random publications were published


```r
random_dois %>%
  # convert to years
  mutate(issued, issued = lubridate::parse_date_time(issued, c('y', 'ymd', 'ym'))) %>%
  mutate(issued, issued = lubridate::year(issued)) %>%
  group_by(issued) %>%
  summarize(pubs = n()) %>%
  arrange(desc(pubs))
#> # A tibble: 45 × 2
#>    issued  pubs
#>     <dbl> <int>
#> 1      NA    15
#> 2    2013     7
#> 3    2006     5
#> 4    2009     5
#> 5    2012     5
#> 6    2014     5
#> 7    1984     4
#> 8    2011     4
#> 9    2016     4
#> 10   1988     3
#> # ... with 35 more rows
```

and of what type they are


```r
random_dois %>%
  group_by(type) %>%
  summarize(pubs = n()) %>%
  arrange(desc(pubs))
#> # A tibble: 8 × 2
#>                  type  pubs
#>                 <chr> <int>
#> 1     journal-article    67
#> 2        book-chapter    12
#> 3 proceedings-article     8
#> 4           component     6
#> 5             dataset     4
#> 6       journal-issue     1
#> 7     reference-entry     1
#> 8            standard     1
```

#### Calling oaDOI.org

Now let's call oaDOI.org


```r
oa_df <- roadoi::oadoi_fetch(dois = random_dois$DOI)
```

and merge the resulting information about open access full-text links with our Crossref metadata-set


```r
my_df <- dplyr::left_join(oa_df, random_dois, by = c("doi" = "DOI"))
my_df
#> # A tibble: 100 × 49
#>                                                               `_best_open_url`
#>                                                                          <chr>
#> 1                                                                         <NA>
#> 2                                                                         <NA>
#> 3                                                                         <NA>
#> 4  http://digitalcommons.macalester.edu/cgi/viewcontent.cgi?article=1045&conte
#> 5                                                                         <NA>
#> 6                             http://doi.org/10.1371/journal.pone.0059758.g002
#> 7                                                                         <NA>
#> 8             http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.678.7907
#> 9                                                                         <NA>
#> 10                                                                        <NA>
#> # ... with 90 more rows, and 48 more variables: `_closed_base_ids` <list>,
#> #   `_closed_urls` <list>, `_open_base_ids` <list>, `_open_urls` <list>,
#> #   doi <chr>, doi_resolver <chr>, evidence <chr>,
#> #   free_fulltext_url <chr>, is_boai_license <lgl>, is_free_to_read <lgl>,
#> #   is_subscription_journal <lgl>, license <chr>, oa_color <chr>,
#> #   url <chr>, year <int>, alternative.id <chr>, container.title <chr>,
#> #   created <chr>, deposited <chr>, funder <list>, indexed <chr>,
#> #   ISBN <chr>, ISSN <chr>, issued <chr>, link <list>, member <chr>,
#> #   page <chr>, prefix <chr>, publisher <chr>, reference.count <chr>,
#> #   score <chr>, source <chr>, subject <chr>, title <chr>, type <chr>,
#> #   URL <chr>, assertion <list>, author <list>,
#> #   `clinical-trial-number` <list>, issue <chr>, volume <chr>,
#> #   license_date <chr>, license_URL <chr>, license_delay.in.days <chr>,
#> #   license_content.version <chr>, archive <chr>, subtitle <chr>,
#> #   update.policy <chr>
```

#### Reporting

After gathering the data, reporting with R is very straightforward. You can even generate dynamic reports using [R Markdown](http://rmarkdown.rstudio.com/) and related packages, thus making your study reproducible and transparent for others.

To display how many full-text links were found and which sources were used in a nicely formatted markdown-table using the [`knitr`](https://yihui.name/knitr/)-package:


```r
my_df %>%
  group_by(evidence) %>%
  summarise(Articles = n()) %>%
  mutate(Proportion = Articles / sum(Articles)) %>%
  arrange(desc(Articles)) %>%
  knitr::kable()
```



|evidence                                              | Articles| Proportion|
|:-----------------------------------------------------|--------:|----------:|
|closed                                                |       84|       0.84|
|oa journal (via publisher name)                       |        6|       0.06|
|oa journal (via journal title in doaj)                |        3|       0.03|
|oa repository (via BASE doi match)                    |        2|       0.02|
|oa repository (via BASE title and first author match) |        2|       0.02|
|oa repository (via pmcid lookup)                      |        2|       0.02|
|oa repository (via BASE title match)                  |        1|       0.01|

How many of them are provided as green or gold open access?


```r
my_df %>%
  group_by(oa_color) %>%
  summarise(Articles = n()) %>%
  mutate(Proportion = Articles / sum(Articles)) %>%
  arrange(desc(Articles)) %>%
  knitr::kable()
```



|oa_color | Articles| Proportion|
|:--------|--------:|----------:|
|NA       |       84|       0.84|
|gold     |        9|       0.09|
|green    |        7|       0.07|

Let's take a closer look and assess how green and gold is distributed over publication types?


```r
my_df %>%
  filter(!evidence == "closed") %>% 
  count(oa_color, type, sort = TRUE) %>% 
  knitr::kable()
```



|oa_color |type                |  n|
|:--------|:-------------------|--:|
|gold     |component           |  6|
|green    |journal-article     |  6|
|gold     |journal-article     |  3|
|green    |proceedings-article |  1|


## Meta

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

License: MIT

Please use the [issue tracker](https://github.com/njahn82/roadoi/issues) for bug reporting and feature requests.

