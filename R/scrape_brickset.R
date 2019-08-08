#' Get all lego themes from brickset.com
#'
#' @param url URL of shop theme website (should also work for interests)
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom stringr str_extract str_remove
#' @importFrom readr parse_number
#' @export
#' @examples
#' library(Lego)
#' themes <- brickset_get_themes()
brickset_get_themes <- function(url = "https://brickset.com/browse/sets") {
  doc <- xml2::read_html(url)
  . <- link <- NULL

  tibble::tibble(
    name = rvest::html_nodes(doc, ".navrow li a") %>% rvest::html_text(),
    link = rvest::html_nodes(doc, ".navrow li a") %>% rvest::html_attr("href") %>%
      paste0("https://brickset.com", .),
    year = stringr::str_extract(link, "year-\\d{1,}") %>%
      stringr::str_remove("year-") %>%
      readr::parse_number()
  )
}

#' Function to get set-specific data from brickset.com
#' 
#' @param auth_args list containing key, username, password. If list is full of 
#'          NULL values, they will be substituted from the environment. 
#'          Auth variables are only necessary to get the user hash/token; if 
#'          .token exists, this argument can be ignored entirely.
#' @param ... Variables as defined in https://brickset.com/api/v2.asmx?op=getSets
#'          Any variables not provided will be substituted with default values
#'          from the documentation with the exception of pageSize, which has 
#'          been increased.
#' @importFrom httr GET
#' @importFrom xml2 read_html 
#' @importFrom rvest html_text html_children html_name
#' @importFrom purrr map map_dfr map_dfc set_names
#' @importFrom tibble tibble
#' @export
#' @examples 
#' \dontrun{
#' brickset_get_sets(theme = "Architecture", pageSize = "10")
#' }
#' if (exists(".api_key") & exists(".token")) {
#'   brickset_get_sets(theme = "Architecture", pageSize = "10")
#' }
#' 
brickset_get_sets <- function(auth_args = list(key = NULL, username = NULL, password = NULL), ...) {
  key <- username <- password <- . <- NULL
  if (exists(".api_key")) {
    if (is.null(auth_args$key)) {
      key <- .api_key
    } else {
      key <- auth_args$key
    }
  } else {
    .api_key <- NULL
  }

  if (!exists(".token")) {
    .token <- NULL
    auth_res <- do.call(brickset_auth, auth_args)
    if (auth_res) {
      token <- .token 
    } else {
      stop("Authentication could not be completed")
    }
  } else {
    token <- .token 
  }
  stopifnot(exists("token"))
  stopifnot(exists("key"))
  
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
  
  auth_args <- list(
    apiKey = key, 
    userHash = token
  )
  
  arglist <- list(...)
  
  arglist <- c(auth_args, arglist, default_args[!names(default_args) %in% names(arglist)])

  
  res <- httr::GET(paste0("https://brickset.com/api/v2.asmx/getSets?", 
                          paste(paste0(names(arglist), "=", arglist), collapse = "&")))
  
  if (res$status_code == 200) {
    res2 <- res %>% xml2::read_html() %>% 
      rvest::html_nodes("sets") %>%
      purrr::map(rvest::html_children) %>% 
      purrr::map_dfr(function(x) {
        x %>%
          purrr::map_dfc(function(xx) {
            tibble::tibble(rvest::html_text(xx)) %>%
              purrr::set_names(rvest::html_name(xx))
          })
      })
  } else {
    warning("Query failed")
    return(res)
  }

}

#' Authenticate with the brickset.com api
#' 
#' @param key API key. If NULL, the function looks for .api_key in the environment
#' @param username username. If NULL, the function looks for .username in the environment
#' @param password password. If NULL, the function looks for .password in the environment
#' @examples 
#' \dontrun{
#' brickset_auth()
#' brickset_auth(key = "your-key", username = "your_username", password = "hunter2")
#' }
brickset_auth <- function(key = NULL, username = NULL, password = NULL) {
  if (exists(".api_key")) {
    if (is.null(key)) {
      key <- .api_key
    }
  } else {
    .api_key <- NULL
  }
  if (exists(".username")) {
    if (is.null(username)) {
      username <- .username
    }
  } else {
    .username <- NULL
  }
  if (exists(".password")) {
    if (is.null(password)) {
      password <- .password
    } 
  } else {
    .password <- NULL
  }
  
  url_pattern <- "https://brickset.com/api/v2.asmx/login?apiKey=%s&username=%s&password=%s"
  res <- httr::GET(sprintf(url_pattern, key, username, password),
                   content="text/xml; charset=utf-8")
  if (res$status_code == "200") {
    token <- res$content %>% rawToChar() %>% xml2::read_html() %>% rvest::html_text()
    # print(token)
    assign(".token", token, pos = .GlobalEnv, inherits = F)
    return(TRUE)
  } else {
    return(FALSE)
  }
}




