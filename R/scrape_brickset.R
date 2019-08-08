brickset_username <- function() {
  if (exists(".brickset_username")) {
    return(.brickset_username)
  }
  return(NULL)
}

brickset_password <- function() {
  if (exists(".brickset_password")) {
    return(.brickset_password)
  }
  return(NULL)
}

brickset_key <- function() {
  if (exists(".brickset_key")) {
    return(.brickset_key)
  }
  return(NULL)
}

brickset_hash <- function() {
  if (exists(".brickset_hash")) {
    return(.brickset_hash)
  }
  return(NULL)
}

#' Authenticate with the brickset.com api
#'
#' Passes username, password, and API key to brickset API, receives a userHash
#' in response. The userHash is stored in a global .brickset_hash variable if it is
#' retrieved successfully; the function returns true for success and false for
#' failure.
#' @param key API key. If NULL, the function looks for .brickset_key in the environment
#' @param username username. If NULL, the function looks for .brickset_username in the environment
#' @param password password. If NULL, the function looks for .brickset_password in the environment
#' @param cache cache key, username, password for later (default TRUE)
#' @return TRUE if authentication succeeds, FALSE otherwise.
#' @examples
#' \dontrun{
#' brickset_auth()
#' brickset_auth(key = "your-key", username = "your_username", password = "hunter2")
#' }
brickset_auth <- function(key = brickset_key(), username = brickset_username(),
                          password = brickset_password(), cache = T) {

  assertthat::assert_that(!is.null(key),
                          !is.null(username),
                          !is.null(password))

  if (cache) {
    assign(".brickset_key", key, pos = .GlobalEnv)
    assign(".brickset_username", username, pos = .GlobalEnv)
    assign(".brickset_password", password, pos = .GlobalEnv)
  }

  url_pattern <- "https://brickset.com/api/v2.asmx/login?apiKey=%s&username=%s&password=%s"
  res <- httr::GET(sprintf(url_pattern, key, username, password),
                   content = "text/xml; charset=utf-8")
  if (res$status_code == "200") {
    userHash <- res$content %>% rawToChar() %>% xml2::read_html() %>% rvest::html_text()
    # print(userHash)
    assign(".brickset_hash", userHash, pos = .GlobalEnv, inherits = F)
    return(TRUE)
  } else {
    return(FALSE)
  }
}

brickset_valid_key <- function(key = brickset_key()) {
  value <- brickset_api(where = "checkKey", apiKey = key) %>%
    xml2::read_html() %>%
    rvest::html_text()

  return(value == "OK")
}

brickset_check_user_hash <- function() {

  value <- brickset_api(where = "checkUserHash", userHash = brickset_hash()) %>%
    xml2::read_html() %>%
    rvest::html_text()

  if (value == "INVALID") warning("User hash is invalid")

  return(tolower(value) == tolower(brickset_username()))
}

#' Get API options
#'
#' This is a general helper for handling the brickset API.
#' @inheritParams brickset_get_themes
#' @param where API endpoint (e.g. getTheme, getSets)
#' @param default_args default arguments for the generici api. Should be supplied by calling function
#'
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
brickset_api <- function(where, auth_args = list(key = brickset_key(), username = brickset_username(),
                                                 password = brickset_password(), userHash = brickset_hash()),
                         default_args = list(),
                         ...) {
  key <- username <- password <- userHash <- . <- NULL


  assertthat::assert_that(!is.null(auth_args$key),
                          !is.null(auth_args$username),
                          !is.null(auth_args$password))

  if (is.null(auth_args$userHash)) {
    auth_res <- do.call(brickset_auth, auth_args[names(auth_args) %in% c("key", "username", "password")])
    assertthat::assert_that(auth_res, msg = "Authentication was not successful")
    userHash <- auth_args$userHash <- .brickset_hash
    auth_args <- list(key = brickset_key(), username = brickset_username(),
                      password = brickset_password(), userHash = brickset_hash())
  }

  assertthat::assert_that(!is.null(auth_args$userHash))

  auth_args <- list(
    apiKey = auth_args$key,
    userHash = auth_args$userHash
  )

  arglist <- list(...)

  arglist <- c(arglist, default_args[!names(default_args) %in% names(arglist)]) %>%
  # URL-encode values
    purrr::map(utils::URLencode)

  arglist <- c(auth_args, arglist)


  res <- httr::GET(paste0("https://brickset.com/api/v2.asmx/", where, "?",
                          paste(paste0(names(arglist), "=", arglist), collapse = "&")))

  if (res$status_code != 200) {
    warning("Query failed")
  }

  return(res)
}

#' Get all lego themes from brickset.com
#'
#' @param auth_args list containing key, username, password. If list is full of
#'          NULL values (default), they will be substituted from the environment.
#'          Environment variables should be named .brickset_key, .brickset_username, .brickset_password.
#'          Auth variables are only necessary to get the user hash; if
#'          .brickset_hash exists, this argument can be ignored entirely.
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @export
#' @examples
#' \dontrun{
#' themes <- brickset_get_themes()
#' }
brickset_get_themes <- function(auth_args = list(key = brickset_key(), username = brickset_username(),
                                                 password = brickset_password(), userHash = brickset_hash())) {

  res <- brickset_api(where = "getThemes",
                      auth_args = auth_args,
                      default_args = list())

  assertthat::assert_that(res$status_code == 200)

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("themes") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(.at = -1, readr::parse_number)
}


#' Function to return many sets' data using brickset.com search API
#'
#' @inheritParams brickset_get_themes
#' @param ... Variables as defined in https://brickset.com/api/v2.asmx?op=getSets
#'          Any variables not provided will be substituted with default values
#'          from the documentation with the exception of pageSize, which has
#'          been increased.
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' brickset_get_sets(theme = "Architecture", pageSize = "10")
#' }
#' if (exists(".brickset_key") & exists(".brickset_hash")) {
#'   brickset_get_sets(theme = "Architecture", pageSize = "10")
#' }
#'
brickset_get_sets <- function(auth_args = list(key = brickset_key(), username = brickset_username(),
                                               password = brickset_password(), userHash = brickset_hash()),
                              ...) {

  default_args <- list(
    query = "",
    theme = "",
    subtheme = "",
    setNumber = "",
    year = "",
    owned = "",
    wanted = "",
    orderBy = "",
    pageSize = "200",
    pageNumber = "",
    userName = ""
  )

  res <- brickset_api(where = "getSets",
                      auth_args = auth_args,
                      default_args = default_args,
                      ...)

  assertthat::assert_that(res$status_code == 200)

  numeric_vars <- c("setid", "year", "pieces", "minifigs", "qtyowned", "acmdatacount", "ownedbytotal", "wantedbytotal", "ukretailprice", "usretailprice", "caretailprice", "euretailprice", "rating", "reviewcount", "instructionscount", "agemin", "userrating")

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("sets") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(numeric_vars, as.numeric)
}



#' Get information about a single lego set from brickset.com
#'
#' @param setID Set ID, obtained from sets retrieved by brickset_get_sets().
#'          This is NOT the set number, it's the internal unique database ID
#'          that remains constant, whereas set numbers may not.
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @export
#' @examples
#' \dontrun{
#' brickset_get_set(setID = 1)
#' }
#'
brickset_get_set <- function(setID = NULL,
                             auth_args = list(key = brickset_key(), username = brickset_username(),
                                              password = brickset_password(), userHash = brickset_hash())) {

  assertthat::assert_that(!is.null(setID))

  res <- brickset_api(where = "getSet",
                      auth_args = auth_args,
                      default_args = list(setID = setID))

  assertthat::assert_that(res$status_code == 200)
  numeric_vars <- c("setid", "year", "pieces", "minifigs", "qtyowned", "acmdatacount", "ownedbytotal", "wantedbytotal", "ukretailprice", "usretailprice", "caretailprice", "euretailprice", "rating", "reviewcount", "instructionscount", "agemin", "userrating")

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("sets") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(numeric_vars, as.numeric)
}

#' Get recently updated sets from brickset.com
#'
#' @param minutesAgo integer get sets updated within the past __ minutes.
#'          Defaults to one day.
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom purrr map map_dfr map_dfc modify_at
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' brickset_get_recently_updated_sets(minutesAgo = 24*60*30)
#' }
brickset_get_recently_updated_sets <- function(
  minutesAgo = 24*60,
  auth_args = list(key = brickset_key(), username = brickset_username(),
                   password = brickset_password(), userHash = brickset_hash())) {

  assertthat::assert_that(!is.null(minutesAgo))

  res <- brickset_api(where = "getRecentlyUpdatedSets",
                      auth_args = auth_args,
                      default_args = list(minutesAgo = minutesAgo))

  assertthat::assert_that(res$status_code == 200)
  numeric_vars <- c("setid", "year", "pieces", "minifigs", "qtyowned", "acmdatacount", "ownedbytotal", "wantedbytotal", "ukretailprice", "usretailprice", "caretailprice", "euretailprice", "rating", "reviewcount", "instructionscount", "agemin", "userrating")

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("sets") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(numeric_vars, as.numeric)
}

#' Get reviews of a single lego set from brickset.com
#'
#' @param setID Set ID, obtained from sets retrieved by brickset_get_sets().
#'          This is NOT the set number, it's the internal unique database ID
#'          that remains constant, whereas set numbers may not.
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' brickset_get_reviews(setID = 22941)
#' }
brickset_get_reviews <- function(
  setID,
  auth_args = list(key = brickset_key(), username = brickset_username(),
                   password = brickset_password(), userHash = brickset_hash())) {
  assertthat::assert_that(!is.null(setID))

  res <- brickset_api(where = "getReviews",
                      auth_args = auth_args,
                      default_args = list(setID = setID))

  assertthat::assert_that(res$status_code == 200)
  numeric_vars <- c("setid", "overallrating", "parts", "buildingexperience", "playability", "valueformoney")
  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("reviews") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    dplyr::mutate(setid = setID) %>%
    purrr::modify_at(numeric_vars, as.numeric)
}


#' Get link to instructions for a single lego set from brickset.com
#'
#' @param setID Set ID, obtained from sets retrieved by brickset_get_sets().
#'          This is NOT the set number, it's the internal unique database ID
#'          that remains constant, whereas set numbers may not.
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @examples
#' \dontrun{
#' brickset_get_instructions(setID = 22941)
#' }
brickset_get_instructions <- function(
  setID,
  auth_args = list(key = brickset_key(), username = brickset_username(),
                   password = brickset_password(), userHash = brickset_hash())) {
  assertthat::assert_that(!is.null(setID))

  res <- brickset_api(where = "getInstructions",
                      auth_args = auth_args,
                      default_args = list(setID = setID))

  assertthat::assert_that(res$status_code == 200)

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("instructions") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    dplyr::mutate(setid = setID)

  if (requireNamespace("qpdf", quietly = TRUE)) {
    res2 <- res2 %>%
      dplyr::mutate(pages = purrr::map_int(url, qpdf::pdf_length))
  }

  res2
}

#' Get subthemes released under a theme
#'
#' @param theme Theme name
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @examples
#' \dontrun{
#' brickset_auth()
#' themes <- brickset_get_themes()
#' subthemes <- dplyr::filter(themes, subthemecount > 0) %>%
#' magrittr::extract2("theme") %>%
#' purrr::map_df(brickset_get_subthemes)
#' }
brickset_get_subthemes <- function(
  theme,
  auth_args = list(key = brickset_key(), username = brickset_username(),
                   password = brickset_password(), userHash = brickset_hash())) {
  assertthat::assert_that(!is.null(theme))

  res <- brickset_api(where = "getSubthemes",
                      auth_args = auth_args,
                      default_args = list(Theme = theme))

  assertthat::assert_that(res$status_code == 200)

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("subthemes") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(-c(1, 2), as.numeric)
}

#' Get years and number of sets released under a theme
#'
#' @param theme Theme name
#' @inheritParams brickset_get_themes
#' @importFrom assertthat assert_that
#' @importFrom xml2 read_html
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names modify_at
#' @importFrom tibble tibble
#' @importFrom readr parse_number
#' @examples
#' \dontrun{
#' brickset_get_years(theme = "Pirates")
#' }
brickset_get_years <- function(
  theme,
  auth_args = list(key = brickset_key(), username = brickset_username(),
                   password = brickset_password(), userHash = brickset_hash())) {
  assertthat::assert_that(!is.null(theme))

  res <- brickset_api(where = "getYears",
                      auth_args = auth_args,
                      default_args = list(Theme = theme))

  assertthat::assert_that(res$status_code == 200)

  res2 <- res %>% xml2::read_html() %>%
    rvest::html_nodes("years") %>%
    purrr::map(rvest::html_children) %>%
    purrr::map_dfr(function(x) {
      x %>%
        purrr::map_dfc(function(xx) {
          tibble::tibble(rvest::html_text(xx)) %>%
            purrr::set_names(rvest::html_name(xx))
        })
    }) %>%
    purrr::modify_at(-1, as.numeric)
}