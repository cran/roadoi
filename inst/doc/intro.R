## -----------------------------------------------------------------------------
library(roadoi)
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9",
                             "10.1103/physreve.88.012814"), 
                    email = "najko.jahn@gmail.com")

## -----------------------------------------------------------------------------
library(dplyr)
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9",
                             "10.1103/physreve.88.012814",
                             "10.1093/reseval/rvaa038",
                             "10.1101/2020.05.22.111294",
                             "10.1093/bioinformatics/btw541"), 
                    email = "najko.jahn@gmail.com", .flatten = TRUE) %>%
  dplyr::count(is_oa, evidence, is_best) 

## -----------------------------------------------------------------------------
roadoi::oadoi_fetch(dois = c("10.1186/s12864-016-2566-9",
                             "10.1103/physreve.88.012814"), 
                    email = "najko.jahn@gmail.com", 
                    .progress = "text")

## ---- message=FALSE-----------------------------------------------------------
library(dplyr)
library(rcrossref)
# get a random sample of DOIs and metadata describing these works
random_dois <- rcrossref::cr_r(filter = list(
  issn = "2330-1643", type = "journal-article"
  ), sample = 50)

## -----------------------------------------------------------------------------
oa_df <- roadoi::oadoi_fetch(random_dois, 
                             email = "najko.jahn@gmail.com")

## -----------------------------------------------------------------------------
oa_df

## -----------------------------------------------------------------------------
oa_df %>%
  group_by(is_oa) %>%
  summarise(Articles = n()) %>%
  mutate(Proportion = Articles / sum(Articles)) %>%
  arrange(desc(Articles))

## -----------------------------------------------------------------------------
oa_df %>%
  filter(is_oa == TRUE) %>%
  tidyr::unnest(oa_locations) %>% 
  group_by(oa_status, evidence, is_best) %>%
  summarise(Articles = n()) %>%
  arrange(desc(Articles))

