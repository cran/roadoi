## ------------------------------------------------------------------------
library(roadoi)
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9",
                             "10.1016/j.cognition.2014.07.007"), 
                    email = "name@example.com")

## ------------------------------------------------------------------------
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9",
                             "10.1016/j.cognition.2014.07.007"), 
                    email = "name@example.com", 
                    .progress = "text")

## ---- message=FALSE------------------------------------------------------
library(dplyr)
library(rcrossref)
# get a random sample of DOIs and metadata describing these works
random_dois <- rcrossref::cr_r(sample = 100) %>%
  rcrossref::cr_works() %>%
  .$data
random_dois

## ------------------------------------------------------------------------
random_dois %>%
  # convert to years
  mutate(issued, issued = lubridate::parse_date_time(issued, c('y', 'ymd', 'ym'))) %>%
  mutate(issued, issued = lubridate::year(issued)) %>%
  group_by(issued) %>%
  summarize(pubs = n()) %>%
  arrange(desc(pubs))

## ------------------------------------------------------------------------
random_dois %>%
  group_by(type) %>%
  summarize(pubs = n()) %>%
  arrange(desc(pubs))

## ------------------------------------------------------------------------
oa_df <- roadoi::oadoi_fetch(dois = random_dois$DOI, email = "name@example.com")

## ------------------------------------------------------------------------
my_df <- dplyr::left_join(oa_df, random_dois, by = c("doi" = "DOI"))
my_df

## ---- results='asis'-----------------------------------------------------
my_df %>%
  group_by(evidence) %>%
  summarise(Articles = n()) %>%
  mutate(Proportion = Articles / sum(Articles)) %>%
  arrange(desc(Articles)) %>%
  knitr::kable()

## ---- results='asis'-----------------------------------------------------
my_df %>%
  group_by(oa_color) %>%
  summarise(Articles = n()) %>%
  mutate(Proportion = Articles / sum(Articles)) %>%
  arrange(desc(Articles)) %>%
  knitr::kable()

## ------------------------------------------------------------------------
my_df %>%
  filter(!evidence == "closed") %>% 
  count(oa_color, type, sort = TRUE) %>% 
  knitr::kable()

