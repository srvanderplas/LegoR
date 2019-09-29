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
#' @param sys_env_var Variable name to use if saving system environment variable. Do not change - parameter for testing purposes only.
#' @param global_var Variable name to use if saving global variable. Do not change - parameter for testing purposes only.
#' @importFrom usethis edit_r_profile ui_yeah
#' @importFrom clipr write_clip
#' @export
#' @examples
#' \dontrun{
#' # rebrickable_save_credentials("your_api_key")
#' }
rebrickable_save_credentials <- function(api_key, profile_save = "user",
                                         sys_env_var = "rebrickable_key",
                                         global_var = ".rebrickable_key") {

  if (!is.null(profile_save)) {
    clipr::write_clip(sprintf("%s = '%s'", global_var, api_key),
                      allow_non_interactive = TRUE)

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

  if (is.null(rebrickable_key(global_var))) {
    assign(global_var, api_key, pos = .GlobalEnv)
  }

  arglist <- list(api_key)
  names(arglist) <- sys_env_var
  do.call(Sys.setenv, arglist)
  if (file.exists(here::here(".Rprofile"))) source(here::here(".Rprofile"))
  if (file.exists("~/.Rprofile")) source("~/.Rprofile")
}


rebrickable_key <- function(nm = ".rebrickable_key") {
  if (exists(nm)) {
    return(eval(as.name(nm)))
  }
  return(NULL)
}


#' Generic API call
#'
#' General helper for handling the brickset API. Only call this directly if you 
#' know what you're doing with APIs - it is exposed because all of the API 
#' functions are not wrapped in this package.
#' 
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
#' @export
rebrickable_api <- function(where, api_key = rebrickable_key(), default_args = list(), ..., follow_next = TRUE) {
  assertthat::assert_that(!is.null(api_key))

  arglist <- list(...)
  arglist <- c(arglist, default_args[!names(default_args) %in% names(arglist)])
  arglist <- c(key = api_key, arglist)

  arg_str <- paste(paste0(names(arglist), "=", as.character(arglist)), collapse = "&") %>%
    # URL-encode values
    utils::URLencode()

  if(arg_str != "") arg_str <- paste0("?", arg_str)
  url <- sprintf("http://rebrickable.com/api/v3/lego/%s/%s", where, arg_str)

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
#' @param retry number of times to retry curl call
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

  next_link <- stringr::str_extract(api_res$content, "next(.*)previous") %>%
    stringr::str_remove_all("next|previous") %>%
    stringr::str_remove_all("\\\"") %>%
    stringr::str_remove_all("^:") %>%
    stringr::str_remove_all(",$")

  if (!is.na(next_link) & next_link != "null") {
    nextpg <- rebrickable_api_call(next_link)

    nextpg_res <- rebrickable_parse_api_res(nextpg)

    return(dplyr::bind_rows(api_res, rebrickable_api_all(nextpg_res)))
  }
  return(api_res)
}

#
# rebrickable_unnest_colors <- function(xid, xext_df) {
#
#   tibble(id = xid,
#          dfr = purrr::map2(xext_df$ext_ids, xext_df$ext_descrs, function(a, b){
#            tibble(ext_ids = a, ext_descrs = purrr::map(b, as.character)) %>%
#              tidyr::unnest()
#          })) %>%
#     tidyr::unnest(dfr)
#
# }


#' Get a data frame of all brick colors
#'
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_colors()
#' }
rebrickable_colors <- function(key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL

  color_res <- rebrickable_api("colors", ..., api_key = key)
  content_list <- color_res$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        purrr::map_df(function(x) dplyr::select(x, -external_ids) %>% unique())
    })
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = color_res))
  }
}

#' Get a data frame of information about a specific brick color
#'
#' @param color_id a single numerical color ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @importFrom assertthat assert_that
#' @importFrom jsonlite parse_json
#' @importFrom stringr str_remove
#' @importFrom purrr map_df map_if is_list
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_color_info(color_id = 1)
#' }
rebrickable_color_info <- function(color_id = 1, key = rebrickable_key(), ..., parse = T) {

  assertthat::assert_that(length(color_id) == 1)
  
  color_res <- rebrickable_api(sprintf("colors/%s/", color_id), ..., api_key = key)
  content_list <- color_res$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        fix_color_mapping
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = color_res))
  }
}

fix_color_mapping <- function(lst) {
  . <- NULL
  reg_cols <- lst[!grepl("external", names(lst))] %>% tibble::as_tibble()

  problem_cols <- lst[grepl("external", names(lst))] %>%
    purrr::map2(., names(.),
                              function(x, y) x %>%
                                dplyr::mutate(type = gsub("external_ids\\.", "", y))) %>%
  purrr::map(~tidyr::unnest(., cols = ext_ids)) %>%
  purrr::map(~tidyr::unnest(., cols = ext_descrs)) %>%
  purrr::map_df(~dplyr::mutate(., ext_descrs = unlist(ext_descrs)))

  reg_cols$external_mapping <- list(problem_cols)

  reg_cols
}


#' Get a data frame of all part categories
#'
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_part_categories()
#' }
rebrickable_part_categories <- function(key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  part_categories <- rebrickable_api("part_categories", ..., api_key = key)
  content_list <- part_categories$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows()
    })
  
  attr(content_list, "full_call") <- part_categories
  
  return(content_list)
}


#' Get a data frame of information about a specific part category ID
#'
#' @param cat_id part category id
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_part_category_info(cat_id = 1)
#' }
rebrickable_part_category_info <- function(cat_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(cat_id) == 1)
  part_categories <- rebrickable_api(sprintf("part_categories/%s/", cat_id), ..., api_key = key)
  content_list <- part_categories$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows()
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_categories))
  }
}


#' Get a data frame of information about parts
#'
#' The parts API endpoint takes parameters page, page_size, part_num, part_nums,
#' part_cat_id, color_id, bricklink_id, brickowl_id, legoid, ldraw_id, ordering, 
#' and search (a search term).
#' 
#' Note that currently external IDs are not returned. 
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_parts(page = 1, follow_next = F)
#' }
rebrickable_parts <- function(key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  parts <- rebrickable_api("parts/", ..., api_key = key)
  
  content_list <- parts$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json(simplifyVector = T) %>%
        `[[`("results") %>%
        as_tibble()  %>%
        dplyr::select(-dplyr::matches("external_ids"))
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = parts))
  }
}


#' Get a data frame of information about a specific part
#'
#' @param part_id part ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_part_info(part_id = 31110)
#' }
rebrickable_part_info <- function(part_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(part_id) == 1)
  part_categories <- rebrickable_api(sprintf("parts/%s/", part_id), ..., api_key = key)
  content_list <- part_categories$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = T) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows()
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_categories))
  }
}



#' Get a list of all colors a part has appeared in
#'
#' @param part_id part ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_part_colors(part_id = 31110)
#' }
rebrickable_part_colors <- function(part_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(part_id) == 1)
  part_colors <- rebrickable_api(sprintf("parts/%s/colors/", part_id), ..., api_key = key)
  content_list <- part_colors$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows() %>%
        tidyr::unnest(elements) %>%
        tidyr::nest(elements=c(elements))
    })
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_colors))
  }
}



#' Get a list of all sets a part/color combination has appeared in
#'
#' @param part_id part ID
#' @param color_id color ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_part_color_sets(part_id = 31110, color_id = 1)
#' }
rebrickable_part_color_sets <- function(part_id = 1, color_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(part_id) == 1)
  assertthat::assert_that(length(color_id) == 1)
  part_colors <- rebrickable_api(sprintf("parts/%s/colors/%s/sets/", part_id, color_id), ..., api_key = key)
  content_list <- part_colors$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, rbind) %>%
        purrr::map_df(as.data.frame) %>%
        dplyr::mutate_each(~ifelse(. == "NULL", NA, .))
    })
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_colors))
  }
}

#' Get a data frame of information about sets
#'
#' The parts API endpoint takes parameters page, page_size, theme_id, min_year,
#' max_year, min_parts, max_parts, ordering, and search.
#' 
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_sets(min_year = 2018, max_year = 2019, min_parts = 20, page_size = 1000, follow_next = T)
#' }
rebrickable_sets <- function(key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  parts <- rebrickable_api("sets/", ..., api_key = key)
  
  content_list <- parts$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json(simplifyVector = T) %>%
        `[[`("results") %>%
        as_tibble() 
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = parts))
  }
}

#' Get a data frame of information about a specific set
#'
#' @param set_id set ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_set_info(set_id = "40349-1")
#' }
rebrickable_set_info <- function(set_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(set_id) == 1)
  part_categories <- rebrickable_api(sprintf("sets/%s/", set_id), ..., api_key = key)
  content_list <- part_categories$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = T) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows()
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_categories))
  }
}


#' Get a data frame of information about alternate builds using only parts from a specific set
#'
#' @param set_id set ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_set_moc(set_id = "10266-1")
#' }
rebrickable_set_moc <- function(set_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(set_id) == 1)
  part_categories <- rebrickable_api(sprintf("sets/%s/alternates/", set_id), ..., api_key = key)
  content_list <- part_categories$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = T) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") %>%
        bind_rows()
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = part_categories))
  }
}


#' Get a data frame of information about pieces in a specific set
#'
#' Note that external IDs are not preserved in parsed results.
#' 
#' @param set_id set ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_set_parts(set_id = "40349-1")
#' }
rebrickable_set_parts <- function(set_id = 1, key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  assertthat::assert_that(length(set_id) == 1)
  set_parts <- rebrickable_api(sprintf("sets/%s/parts/", set_id), ..., api_key = key)
  content_list <- set_parts$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      y <- x %>%
        jsonlite::parse_json() %>%
        `[[`("results") %>%
        purrr::map(rbind)
      
      parts <- purrr::map(y, 3)
      colors <- purrr::map(y, 4)
      y2 <- y %>% purrr::modify_depth(~ifelse(is.null(.), NA, .), .depth = 2)
      
      purrr::map_df(y2, . %>% as_tibble() %>% dplyr::select(-one_of(c("part", "color"))) %>%
        tidyr::unnest(cols = dplyr::everything(), keep_empty = T)) %>%
        dplyr::mutate(parts = purrr::map(parts, . %>% rbind() %>% 
                                           tibble::as_tibble() %>% 
                                           dplyr::select(-external_ids, -print_of) %>%
                                           tidyr::unnest(dplyr::everything()) %>%
                                           dplyr::rename(part_name = name))) %>%
        dplyr::mutate(colors = purrr::map(colors, .%>% rbind() %>%
                                            tibble::as_tibble() %>%
                                            dplyr::select(-external_ids) %>%
                                            tidyr::unnest(dplyr::everything()) %>%
                                            dplyr::rename(color_name = name, color_id = id))) %>%
        dplyr::mutate(color_parts = purrr::map2(parts, colors, dplyr::bind_cols)) %>%
        tidyr::unnest_wider(color_parts) %>%
        select(-colors,-parts)
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = set_parts))
  }
}



#' Get a data frame of all themes
#'
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_themes()
#' }
rebrickable_themes <- function(key = rebrickable_key(), ..., parse = T) {
  . <- external_ids <- NULL
  
  theme_res <- rebrickable_api("themes", ..., api_key = key)
  content_list <- theme_res$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json(simplifyVector = T) %>%
        `[[`("results") %>%
        as_tibble() 
    })
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = theme_res))
  }
}

#' Get a data frame of information about a specific brick color
#'
#' @param theme_id a single numerical theme ID
#' @param key API key (pulled from environment if saved via rebrickable_save_credentials)
#' @param ... other arguments (page, page_size, ordering)
#' @param parse Return results as a formatted tbl without the response information?
#' @importFrom assertthat assert_that
#' @importFrom jsonlite parse_json
#' @importFrom stringr str_remove
#' @importFrom purrr map_df map_if is_list
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @export
#' @examples
#' if (exists(".rebrickable_key")) {
#'   rebrickable_theme_info(theme_id = 1)
#' }
rebrickable_theme_info <- function(theme_id = 1, key = rebrickable_key(), ..., parse = T) {
  
  assertthat::assert_that(length(theme_id) == 1)
  
  theme_res <- rebrickable_api(sprintf("themes/%s/", theme_id), ..., api_key = key)
  content_list <- theme_res$content %>%
    stringr::str_remove("\\\"count\\\":\\d{1,},") %>%
    stringr::str_remove("\\\"next\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    stringr::str_remove("\\\"previous\\\":(\\\")?[A-z\\.:/0-9?=&]{1,}(\\\")?,") %>%
    purrr::map_df(function(x) {
      x %>%
        jsonlite::parse_json() %>%
        unlist(recursive = F) %>%
        purrr::map_if(purrr::is_list, tibble::as_tibble, .name_repair = "minimal") 
    })
  
  
  if (parse) {
    return(content_list)
  } else {
    return(list(parsed_content = content_list, full_res = theme_res))
  }
}

