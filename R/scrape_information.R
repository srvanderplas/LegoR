#' Get all lego themes from shop.lego.com
#'
#' @param url URL of shop theme website (should also work for interests)
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @examples
#' library(Lego)
#' themes <- lego_get_themes()
#' interests <- lego_get_themes("https://shop.lego.com/en-US/category/interests")
lego_get_themes <- function(url = "https://shop.lego.com/en-US/category/themes") {
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
#' @param url
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom purrr map_chr
#' @importFrom readr parse_number
#' @examples
#' library(Lego)
#' star_wars_pg1 <- lego_get_sets_on_page("https://shop.lego.com/en-US/category/star-wars")
lego_get_sets_on_page <- function(url) {
  doc <- xml2::read_html(url)

  sets <- rvest::html_nodes(doc, "li[class*=ProductGridstyles__Item]")

  node_text <- function(...) html_text(html_nodes(...))
  node_attr <- function(attr, ...) html_attr(html_nodes(...), name = attr)

  tibble::tibble(
    flag = sets %>% purrr::map_chr(.f = node_or_NA,
                                   fun = node_text,
                                   css = "span[data-test*=product-flag]"),
    id = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_text,
                                 css = "span[class*=ProductLeafSharedstyles__Code]"),
    rating = sets %>% purrr::map_chr(.f = node_or_NA, fun = node_attr,
                                     css = "div[class*=RatingBar__RatingContainer]",
                                     attr = "title") %>%
      readr::parse_number(),
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