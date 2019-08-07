#' Get all lego themes from brickset.com
#'
#' @param url URL of shop theme website (should also work for interests)
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom stringr str_extract str_remove
#' @importFrom readr parse_number
#' @examples
#' library(Lego)
#' themes <- brickset_get_themes()
brickset_get_themes <- function(url) {
  doc <- xml2::read_html(url)
  
  tibble::tibble(
    name = rvest::html_nodes(doc, ".navrow li a") %>% rvest::html_text(),
    link = rvest::html_nodes(doc, ".navrow li a") %>% rvest::html_attr("href") %>%
      paste0("https://brickset.com", .), 
    year = stringr::str_extract(link, "year-\\d{1,}") %>% 
      stringr::str_remove("year-") %>% 
      readr::parse_number()
  )
}

brickset_get_sets <- function(url) {
  doc <- xml2::read_html(url)

  # tibble::tibble(
  #   # set_number = rvest::
  # )
}