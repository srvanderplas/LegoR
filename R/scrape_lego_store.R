#' Get all lego themes from shop.lego.com
#'
#' @param url URL of shop theme website (should also work for interests)
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @export
#' @examples
#' library(LegoR)
#' themes <- lego_get_themes()
#' interests <- lego_get_themes("https://shop.lego.com/en-US/category/interests")
lego_get_themes <- function(url = "https://shop.lego.com/en-US/category/themes") {
  . <- NULL
  doc <- xml2::read_html(url)
  tibble::tibble(
    name = rvest::html_nodes(doc, "span[class*=CategoryLeafstyles__Title]") %>%
      rvest::html_text(),
    link = rvest::html_nodes(doc, "a[class*=DetailsLink]") %>%
      rvest::html_attr("href") %>%
      paste0("https://shop.lego.com", .),
    description = rvest::html_nodes(doc, "div[class*=CategoryLeafstyles__Description]") %>%
      rvest::html_text(),
    age_range = rvest::html_nodes(doc, "span[class*=CategoryLeafstyles__AgeRange]") %>%
      rvest::html_text()
  )
}



node_or_NA <- function(x, fun, ...) {
  res <- try(fun(x, ...))
  if (length(res) > 0) return(res)

  return(NA)
}

#' Get all lego sets on the page
#'
#' @param url url of page with lego sets
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom purrr map_chr
#' @importFrom readr parse_number
lego_get_sets_on_page <- function(url) {
  . <- NULL
  doc <- xml2::read_html(url)

  sets <- rvest::html_nodes(doc, "li[class*=ProductGridstyles__Item]")

  node_text <- function(...) html_text(html_nodes(...))
  node_attr <- function(attr, ...) html_attr(html_nodes(...), name = attr)

  tibble::tibble(
    flag = sets %>% purrr::map_chr(.f = node_or_NA,
                                   fun = node_text,
                                   css = "span[data-test*=product-flag]"),
    Item = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_text,
                                   css = "span[class*=ProductLeafSharedstyles__Code]"),
    price = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_text,
                                    css = "span[class*=ProductPrice__StyledText]") %>%
      readr::parse_number(),
    title = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_text,
                                    css = "h2[class*=ProductLeafSharedstyles__Title] span"),
    link = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_attr,
                                   css = "a[data-test*=product-leaf-title-link]",
                                   attr = "href") %>%
      paste0("https://shop.lego.com", .)
  )
}

#' Get all lego sets in a category
#' 
#' @param url url of the base page (additional pages will be retrieved automatically)
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_node
#' @importFrom stringr str_extract_all str_remove
#' @importFrom readr parse_number
#' @importFrom purrr map_df
#' @export
#' @examples 
#' library(LegoR)
#' lego_get_sets("https://shop.lego.com/en-US/category/creator-3-in-1")
lego_get_sets <- function(url) {
  . <- NULL
  doc <- xml2::read_html(url)
  
  n_pages <- rvest::html_node(doc, "span[class*=PaginationBadgerstyles__Text]") %>%
    rvest::html_text() %>%
    stringr::str_extract_all("\\d{1,}") %>%
    unlist() %>%
    readr::parse_number() %>%
    max()
  
  urls <- stringr::str_remove(url, "\\?.*$") %>%
    paste0(., "?page=", 1:n_pages)
  
  purrr::map_df(urls, lego_get_sets_on_page) %>%
    unique()
}

#' Get data from a lego set page
#' 
#' @param url page url
#' @importFrom purrr map_dfc set_names
#' @importFrom tibble tibble
#' @importFrom rvest html_nodes html_text html_children
#' @importFrom stringr str_extract str_remove
#' @importFrom readr parse_number
#' @importFrom xml2 read_html
#' @export
#' @examples 
#' library(LegoR)
#' sets <- lego_get_sets("https://shop.lego.com/en-US/category/creator-3-in-1")
#' lego_get_set_data(sets$link[1]) # get one set
#' sets <- sets %>%
#'   dplyr::mutate(set_info = purrr::map(link, lego_get_set_data)) %>%
#'   tidyr::unnest()
lego_get_set_data <- function(url) {
  . <- NULL
  doc <- xml2::read_html(url)
  
  rvest::html_nodes(doc, "dl[class*=ProductDetails__ProductAttribute]") %>%
    purrr::map_dfc(function(x) {
      tibble::tibble(a = rvest::html_node(x, "dd") %>% rvest::html_text()) %>% 
        purrr::set_names(rvest::html_node(x, "dt") %>% rvest::html_text())
    }) %>%
    purrr::set_names(make.names(names(.))) %>%
    dplyr::mutate(
      minifigs = node_or_NA(
        doc, 
        function(.) rvest::html_nodes(., "p:contains(minifig)") %>% 
          rvest::html_text() %>%
          stringr::str_extract("\\d{1,} minifig") %>%
          stringr::str_remove_all("\\D") %>%
          readr::parse_number()
      ),
      availability = node_or_NA(
        doc, 
        function(.) rvest::html_nodes(., "p[class*=ProductOverviewstyles__AvailabilityStatus]") %>%
          rvest::html_text()
      ),
      review_count = node_or_NA(
        doc, 
        function(.) rvest::html_nodes(., "span[itemprop*=reviewCount]") %>%
        rvest::html_text() %>% 
        readr::parse_number()
      ),
      rating_value = node_or_NA(
        doc, 
        function(.) rvest::html_nodes(., "span[itemprop*=ratingValue]") %>%
        rvest::html_text() %>% 
        readr::parse_number()
      ),
      best_rating = node_or_NA(
        doc, 
        function(.) rvest::html_nodes(., "span[itemprop*=bestRating]") %>%
        rvest::html_text() %>% 
        readr::parse_number()
      )
    )
}