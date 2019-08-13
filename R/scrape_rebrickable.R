if(getRversion() >= "2.15.1")  utils::globalVariables(".rebrickable_key")
#' Set up R to work with rebrickable.com
#'
#' @importFrom usethis ui_yeah
#' @importFrom utils browseURL
#' @export
#' @examples
#' \dontrun{
#' rebrickable_setup()
#' }
rebrickable_setup <- function() {
  request <- "Welcome to the LegoR package. \n
  To use the rebrickable functionality, you will need a user account and an API key. \n"

  if (!interactive()) {
    message(request)
    message("As this session is not interactive, you'll need to do this part manually. \n
            Go to https://rebrickable.com and register, saving your username, password, and api key to input into rebrickable_save_credentials().")
    return(invisible(NULL))
  }

  if (usethis::ui_yeah(request)) {
    utils::browseURL("https://rebrickable.com/register/")
    message("If a browser did not open automatically, go to https://rebrickable.com/register/ and sign up for an account")
  }

  validate_email <- "Next, you need to validate your email so you can get an API key. Go to account -> settings and verify your email address. "
  message(validate_email)

  api_key <- "Let's get an API key!"
  if (usethis::ui_yeah(api_key)) {
    message <- "Please enter your username and press enter to submit."
    username <- readline()
    utils::browseURL(sprintf("https://rebrickable.com/users/%s/settings/#api", username))
    message("Press the 'Generate new API Key' button")
  }

  message("Call rebrickable_save_credentials(api_key) to save your key to your Rprofile.")
}

#' Save rebrickable credentials to Rprofile file
#'
#' Opens the user's Rprofile file and pastes the values to the clipboard, then
#' sources the file.
#'
#' @param api_key API key.
#' @param profile_save Save information to R profile? One of "user", "project", or NULL (to not save information)
#' @importFrom usethis edit_r_profile ui_yeah
#' @importFrom clipr write_clip
#' @export
#' @examples
#' \dontrun{
#' # rebrickable_save_credentials("your_api_key")
#' }
rebrickable_save_credentials <- function(api_key, profile_save = "user") {

  if (!is.null(profile_save)) {
    clipr::write_clip(sprintf(".rebrickable_key = '%s'", api_key))

    if (interactive()) {
      if (usethis::ui_yeah(sprintf("Are you ok with adding these to your %s Rprofile?", profile_save))) {
        usethis::edit_r_profile(profile_save)
        message("The values have been copied to your clipboard. Paste them into the R profile file and save.")
      } else {
        message("The values have been copied to your clipboard. Save them in a safe place of your choosing.")
      }
    } else {
      message("Session is not interactive. Values have been copied to the clipboard.")
    }

  }
  
  if (is.null(rebrickable_key())) {
    assign(".rebrickable_key", api_key, pos = .GlobalEnv)
  }
  
  Sys.setenv(rebrickable_key = .rebrickable_key)
  source("~/.Rprofile")
}


rebrickable_key <- function() {
  if (exists(".rebrickable_key")) {
    return(.rebrickable_key)
  }
  return(NULL)
}


#' Generic API call
#'
#' General helper for handling the brickset API.
#' @param where API endpoint
#' @param api_key API key (pulled from .rebrickable_key if it exists)
#' @param default_args default API args
#' @param ... Additional arguments to the API
#' @param follow_next If response contains a 'next' argument, should it be followed?
#' @importFrom httr GET
#' @importFrom utils URLencode
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that
#' @importFrom jsonlite fromJSON
#'
rebrickable_api <- function(where, api_key = rebrickable_key(), default_args = list(), ..., follow_next = TRUE) {
  assertthat::assert_that(!is.null(api_key))

  arglist <- list(...)
  arglist <- c(arglist, default_args[!names(default_args) %in% names(arglist)])
  arglist <- c(key = api_key, arglist)

  arg_str <- paste(paste0(names(arglist), "=", as.character(arglist)), collapse = "&") %>%
    # URL-encode values
    utils::URLencode()

  url <- sprintf("https://rebrickable.com/api/v3/lego/%s/?%s", where, arg_str)

  res <- rebrickable_api_call(url)

  ret_res <- rebrickable_parse_api_res(res)

  if (follow_next) {
    rebrickable_api_all(ret_res)
  } else {
    ret_res
  }
}

#' Do-while loop for GET to handle rcurl being flaky
#' @param url url to GET
#' @return results in the form of a list from GET
rebrickable_api_call <- function(url, retry = 5) {
  # I hate myself for having to do it this way...
  n <- 0
  repeat {
    res <- try(httr::GET(url))
    if (n >= retry | !("try-error" %in% class(res))) {
      break;
    }
    n <- n + 1
  }
  return(res)
}

#' Function to format API response
#'
#' @param res response from API
#' @return tibble with response status, content (list col), headers (list col), url, date, times (list col), and request (list col)
rebrickable_parse_api_res <- function(res) {

  if (res$status_code == 429) {
    warning("Request throttled")
  } else if (!res$status_code %in% c(200, 201, 204)) {
    warning("Something went wrong")
  }

  res_tbl <- tibble::tibble(
    status = res$status_code,
    content = list(res$content %>% xml2::read_html() %>% rvest::html_text()),
    headers = list(res$headers),
    url = res$url,
    date = res$date,
    times = list(res$times),
    request = list(res$request)
  )

  class(res_tbl) <- c(class(res_tbl), "rebrickable_api")

  res_tbl
}

#' Get all pages from the rebrickable api
#' 
#' Follows 'next' links returned by API calls, appending the results into
#' a tibble so that each tibble row corresponds to a single call.
#' @param api_res rebrickable_api return object
#' @return tibble of rebrickable_api return objects
#' @importFrom dplyr bind_rows
#' @importFrom httr GET
rebrickable_api_all <- function(api_res) {
  # message("in recursion")
  stopifnot("rebrickable_api" %in% class(api_res))

  next_link <- stringr::str_extract(api_res$content, "\\\"next\\\":\\\"https[:/a-z.0-9?=&]{1,}\\\"") %>%
    stringr::str_remove_all("\\\"") %>%
    stringr::str_remove_all("next:")

  if (!is.na(next_link)) {
    nextpg <- rebrickable_api_call(next_link)
    
    nextpg_res <- rebrickable_parse_api_res(nextpg)
    
    return(dplyr::bind_rows(api_res, rebrickable_api_all(nextpg_res)))
  }
  return(api_res)
}


rebrickable_unnest_colors <- function(xid, xext_df) {

  tibble(id = xid,
         dfr = purrr::map2(xext_df$ext_ids, xext_df$ext_descrs, function(a, b){
           tibble(ext_ids = a, ext_descrs = purrr::map(b, as.character)) %>%
             tidyr::unnest()
         })) %>%
    tidyr::unnest(dfr)

}


#' Get a data frame of all brick colors
#'
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
rebrickable_colors <- function(key = rebrickable_key(), ..., parse = T) {

  color_res <- rebrickable_api("colors", api_key = key)

  if (parse) {
    content_list <- jsonlite::parse_json(color_res$content[[1]])

    main_df <- content_list$results %>%
      purrr::map_if(purrr::is_list, tibble::as_tibble) %>%
      purrr::map_df(~tidyr::nest(., external_ids)) %>%
      dplyr::rename(external_color_id = data)
    # external_color_mapping <- dplyr::select(main_df, id, external_color_id) %>%
    #   tidyr::unnest() %>%
    #   purrr::modify_at("external_ids", function(x) x %>% dplyr::bind_cols())
    # # dplyr::bind_cols() %>% tidyr::unnest(ext_ids) %>% tidyr::unnest() %>% tidyr::unnest()
    #
    #   purrr::modify_at("external_ids", purrr::map_if(., purrr::is_list, function(x) x %>% tibble::as_tibble(.name_repair = "unique")))
    # %>%
    #   purrr::map2(.x = ., .y = main_df$id, ~mutate(.x, var = c("ext_id", "ext_value"), id = .y)) %>%
    #   purrr::map(~tidyr::gather(., key = site, value = value, -var)) %>%
    #   purrr::map_df(~tidyr::spread(., key = var, value = value)) %>%
    #
    #   tidyr::unnest()

    color_res %>%
      rename(original_content = content) %>%
      mutate(content = list(main_df))

  }
}

#
# rebrickable_api("colors")
#
# tmp <- httr::GET("https://rebrickable.com/api/v3/lego/colors/?key=2387cd1ec974d99374795f15087e3b32")
# tmp$content %>% xml2::read_html() %>% rvest::html_text() %>% jsonlite::fromJSON()
#
# tmp$url
# tmp$status_code
# tmp$headers
# tmp$all_headers
# tmp$cookies
# tmp$content
# tmp$date
# tmp$times
# tmp$request
# tmp$handle
