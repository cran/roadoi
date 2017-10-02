#' Fetch open access status information and full-text links from oaDOI
#'
#' This is the main function to retrieve comprehensive open access status
#' information from the oaDOI service. Please play nice with the API. At the
#' moment only 100k request are suggested per user and day.
#' For more info see \url{https://oadoi.org/api}.
#'
#' @param dois character vector, search by a single DOI or many DOIs.
#'   A rate limit of 100k requests per day is suggested. If you need to access
#'   more data, request the data dump \url{https://oadoi.org/api} instead.
#' @param email character vector, mandatory! oaDOI requires your email address,
#'   so that they can track usage and notify you when something breaks.
#'   Set email address in your `.Rprofile` file with
#'   the option `roadoi_email` \code{options(roadoi_email = "name@example.com")}.
#' @param .progress Shows the \code{plyr}-style progress bar.
#'   Options are "none", "text", "tk", "win", and "time".
#'   See \code{\link[plyr]{create_progress_bar}} for details
#'   of each. By default, no progress bar is displayed.
#'
#' @return The result is a tibble with each row representing a publication.
#'   Here are the returned columns and descriptions according to the API docu:
#'
#'
#' \tabular{ll}{
#'  \code{doi}              \tab DOI (always in lowercase). \cr
#'  \code{best_oa_location} \tab list-column describing the best OA location.
#'  Algorithm prioritizes publisher hosted content (eg Hybrid or Gold),
#'  then prioritizes versions closer to the  version of record (PublishedVersion
#'  over AcceptedVersion), then more authoritative  repositories (PubMed Central
#'  over CiteSeerX). \cr
#'  \code{oa_locations}     \tab list-column of all the OA locations. \cr
#'  \code{data_standard}    \tab Indicates the data collection approaches used
#'  for this resource. \code{1} mostly uses Crossref for hybrid detection. \code{2}
#'  uses more comprehensive hybrid detection methods. \cr
#'  \code{is_oa}            \tab Is there an OA copy (logical)? \cr
#'  \code{journal_is_oa}    \tab Is the article published in a fully OA journal?
#'  Uses the Directory of Open Access Journals (DOAJ) as source. \cr
#'  \code{journal_issns}    \tab ISSNs, i.e. unique numbers to identify
#'  journals. \cr
#'  \code{journal_name}     \tab Journal title, not normalized. \cr
#'  \code{publisher}        \tab Publisher, not normalized. \cr
#'  \code{title}            \tab Publication title. \cr
#'  \code{year}             \tab Year published. \cr
#'  \code{updated}          \tab Time when the data for this resource was last updated. \cr
#'  \code{non_compliant}    \tab Lists other full-text resources that are not
#'  hosted by either publishers or repositories. \cr
#' }
#'
#' The columns  \code{best_oa_location} and  \code{oa_locations} are list-columns
#' that contain useful metadata about the OA sources found by oaDOI. These are
#'
#' \tabular{ll}{
#'  \code{evidence}        \tab How the OA location was found and is characterized
#'   by oaDOI? \cr
#'  \code{host_type}       \tab OA full-text provided by \code{publisher} or
#'   \code{repository}. \cr
#'  \code{license}         \tab The license under which this copy is published,
#'   e.g. Creative Commons license. \cr
#'  \code{url}             \tab The URL where you can find this OA copy. \cr
#'  \code{versions}        \tab The content version accessible at this location
#'   following the DRIVER 2.0 Guidelines
#'  (\url{https://wiki.surfnet.nl/display/DRIVERguidelines/DRIVER-VERSION+Mappings}\cr
#' }
#'
#'
#' To unnest list-columns, you want to use tidyr's unnest function
#' \code{\link[tidyr]{unnest}}.

#' @examples \dontrun{
#' oadoi_fetch("10.1038/nature12373", email = "name@example.com")
#' oadoi_fetch(dois = c("10.1016/j.jbiotec.2010.07.030",
#' "10.1186/1471-2164-11-245", email = "name@example.com"))
#' }
#'
#' @export
oadoi_fetch <-
  function(dois = NULL,
           email = getOption("roadoi_email"),
           .progress = "none") {
    # input validation
    stopifnot(!is.null(dois))
    email <- val_email(email)
    if (length(dois) > api_limit)
      stop(
        "A rate limit of 100k requests per day is suggested.
        If you need to access more data, request the data dump
        https://oadoi.org/api instead",
        .call = FALSE
      )
    # Call API for every DOI, and return results as tbl_df
    plyr::llply(dois, oadoi_fetch_, email, .progress = .progress) %>%
      dplyr::bind_rows()
  }

#' Get open access status information.
#'
#' In general, use \code{\link{oadoi_fetch}} instead. It calls this
#' method, returning open access status information from all your requests.
#'
#' @param doi character vector,a DOI
#' @param email character vector, required! It is strongly encourage to tell
#'   oaDOI your email adress, so that they can track usage and notify you
#'   when something breaks. Set email address in your `.Rprofile` file with
#'   the option `roadoi_email` \code{options(roadoi_email = "name@example.com")}.
#' @return A tibble
#' @examples \dontrun{
#' oadoi_fetch_(doi = c("10.1016/j.jbiotec.2010.07.030"))
#' }
#' @export
oadoi_fetch_ <- function(doi = NULL, email) {
  u <- httr::modify_url(
    oadoi_baseurl(),
    query = list(email = email),
    path = c(oadoi_api_version(), doi)
  )
  # Call oaDOI API
  resp <- httr::GET(u, ua)

  # test for valid json
  if (httr::http_type(resp) != "application/json") {
    # test needed because oaDOI throws 505 when non-encoded whitespace
    # is provided by this client
    stop(
      sprintf(
        "Oops, API did not return json after calling '%s':
        check your query - or api.oadoi.org may experience problems",
        doi
      ),
      call. = FALSE
    )
  }

  # warn if nothing could be found and return meaningful message
  if (httr::status_code(resp) != 200) {
    warning(
      sprintf(
        "oaDOI request failed [%s]\n%s",
        httr::status_code(resp),
        httr::content(resp)$message
      ),
      call. = FALSE
    )
    NULL
  } else {
    httr::content(resp, "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      purrr::map(purrr::compact) %>% # remove empty list elements
      parse_oadoi()
  }
}

#' Parser for OADOI JSON
#'
#' @param req unparsed JSON
#'
#' @noRd
parse_oadoi <- function(req) {
  # be aware of empty list elements
  req <- purrr::map_if(req, is.null, ~ NA_character_)
  tibble::tibble(
    doi = req$doi,
    best_oa_location = list(as_data_frame(req$best_oa_location)),
    oa_locations = list(as_data_frame(req$oa_location)),
    data_standard = req$data_standard,
    is_oa = req$is_oa,
    journal_is_oa = as.logical(ifelse(is.null(req$journal_is_oa),
                           FALSE, req$journal_is_oa)),
    journal_issns = req$journal_issns,
    journal_name = req$journal_name,
    publisher = req$publisher,
    title = req$title,
    year = as.character(req$year),
    updated = req$updated,
    non_compliant = list(req$x_reported_noncompliant_copies)
  )
}
