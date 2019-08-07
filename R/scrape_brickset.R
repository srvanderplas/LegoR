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
brickset_get_themes <- function(url = "https://brickset.com/browse/sets") {
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
  doc_orig <- xml2::read_html(url)
  ct <- V8::v8() # create a new V8 context

  # js to set csvview class
  set_csvview <- "var element = document.getElementsByTagName('body')[0];
                  element.classList.replace('listview', 'csvview');"


  doc <- xml2::read_html(ct$eval(url)

  sets <- rvest::html_nodes(doc, ".meta")
  tibble::tibble(
    # set_number = rvest::
  )
}

#Loading both the required libraries
library(rvest)
library(V8)

httr::POST(url = "https://brickset.com/api/v2.asmx/getSets",
           body = "apiKey=xxx&theme=ArchitecturepageSize=500")

#URL with js-rendered content to be scraped
link <- "https://brickset.com/sets/theme-Architecture/year-2019"
#Read the html page content and extract all javascript codes that are inside a list
emailjs <- read_html(link) %>% html_nodes('li') %>% html_nodes('script') %>% html_text()
# Create a new v8 context
ct <- V8::v8()
#parse the html content from the js output and print it as text
read_html(ct$eval(gsub('document.write','',emailjs))$source(link)) %>%
  html_text()
info@brewhemia.co.uk
