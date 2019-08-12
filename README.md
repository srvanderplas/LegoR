
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LegoR

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/LegoR)](https://cran.r-project.org/package=LegoR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--08--12-yellowgreen.svg)](/commits/master)
[![Travis build
status](https://travis-ci.org/srvanderplas/LegoR.svg?branch=master)](https://travis-ci.org/srvanderplas/LegoR)
<!-- badges: end -->

The goal of LegoR is to make it easy to get Lego-centric data into R.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("srvanderplas/LegoR")
```

``` r
# Set up and load packages
library(tidyverse)
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.0          ✔ purrr   0.3.2     
#> ✔ tibble  2.1.3          ✔ dplyr   0.8.3     
#> ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
#> ✔ readr   1.3.1          ✔ forcats 0.4.0
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(LegoR)
```

## Lego.com

The first set of functions provide a convenient way to scrape data from
<https://shop.lego.com/>. These functions are based on the `rvest`
package and depend on the structure of the site; site updates may break
the functionality. All functions for lego.com start with the `lego_`
prefix.

The natural approach to gather data on all currently available lego sets
is to get all sets by theme.

``` r
(themes <- lego_get_themes())
#> # A tibble: 40 x 4
#>    theme_name  theme_link         theme_description         theme_age_range
#>    <chr>       <chr>              <chr>                     <chr>          
#>  1 Architectu… https://shop.lego… LEGO® Architecture prese… ""             
#>  2 BOOST       https://shop.lego… LEGO® BOOST lets childre… ""             
#>  3 BrickHeadz  https://shop.lego… Collect, build and displ… ""             
#>  4 City        https://shop.lego… LEGO® City is a realisti… ""             
#>  5 Classic     https://shop.lego… Develop children’s creat… ""             
#>  6 Creator 3-… https://shop.lego… The LEGO® Creator series… ""             
#>  7 Creator Ex… https://shop.lego… Are you ready for the ul… ""             
#>  8 DC Super H… https://shop.lego… LEGO® DC Universe™ Super… ""             
#>  9 Disney™     https://shop.lego… LEGO® Disney characters … ""             
#> 10 DUPLO®      https://shop.lego… For 50 years, we have be… ""             
#> # … with 30 more rows
```

Each theme link leads to a page with one or more sets.

``` r
(architecture_sets <- lego_get_sets(themes$theme_link[1]))
#> # A tibble: 11 x 5
#>    set_flag set_id set_price set_title        set_link                     
#>    <chr>    <chr>      <dbl> <chr>            <chr>                        
#>  1 New      21046      130.  Empire State Bu… https://shop.lego.com/en-US/…
#>  2 New      21045       80.0 Trafalgar Square https://shop.lego.com/en-US/…
#>  3 <NA>     21042      120.  Statue of Liber… https://shop.lego.com/en-US/…
#>  4 <NA>     21030      100.0 United States C… https://shop.lego.com/en-US/…
#>  5 <NA>     21028       60.0 New York City    https://shop.lego.com/en-US/…
#>  6 <NA>     21039       60.0 Shanghai         https://shop.lego.com/en-US/…
#>  7 <NA>     21041       50.0 Great Wall of C… https://shop.lego.com/en-US/…
#>  8 <NA>     21044       50.0 Paris            https://shop.lego.com/en-US/…
#>  9 <NA>     21043       50.0 San Francisco    https://shop.lego.com/en-US/…
#> 10 <NA>     21047       40.0 Las Vegas        https://shop.lego.com/en-US/…
#> 11 <NA>     21034       40.0 London           https://shop.lego.com/en-US/…
```

If the goal is to get the price and titles, we could stop here, but more
set data is available on the set-specific page.

``` r
lego_get_set_data(architecture_sets$set_link[1])
#> # A tibble: 1 x 9
#>   set_Item set_VIP.Points set_Ages set_Pieces set_minifigs set_availability
#>   <chr>    <chr>          <chr>    <chr>      <lgl>        <chr>           
#> 1 21046    845            16+      1767       NA           Available now   
#> # … with 3 more variables: set_review_count <dbl>, set_rating_value <dbl>,
#> #   set_best_rating <dbl>
```

These sets are structured in order to provide easy pipe functionality:

``` r
set_data <- lego_get_themes() %>%
  filter(row_number() == 1) %>% # Don't get everything in the demo
  mutate(set_summary = purrr::map(theme_link, lego_get_sets)) %>%
  unnest(set_summary) %>%
  mutate(set_data = purrr::map(set_link, lego_get_set_data)) %>%
  unnest(set_data) %>%
  select(-set_Item) # Some variables are repeated

set_data
#> # A tibble: 11 x 17
#>    theme_name theme_link theme_descripti… theme_age_range set_flag set_id
#>    <chr>      <chr>      <chr>            <chr>           <chr>    <chr> 
#>  1 Architect… https://s… LEGO® Architect… ""              New      21046 
#>  2 Architect… https://s… LEGO® Architect… ""              New      21045 
#>  3 Architect… https://s… LEGO® Architect… ""              <NA>     21042 
#>  4 Architect… https://s… LEGO® Architect… ""              <NA>     21030 
#>  5 Architect… https://s… LEGO® Architect… ""              <NA>     21028 
#>  6 Architect… https://s… LEGO® Architect… ""              <NA>     21039 
#>  7 Architect… https://s… LEGO® Architect… ""              <NA>     21041 
#>  8 Architect… https://s… LEGO® Architect… ""              <NA>     21044 
#>  9 Architect… https://s… LEGO® Architect… ""              <NA>     21043 
#> 10 Architect… https://s… LEGO® Architect… ""              <NA>     21047 
#> 11 Architect… https://s… LEGO® Architect… ""              <NA>     21034 
#> # … with 11 more variables: set_price <dbl>, set_title <chr>,
#> #   set_link <chr>, set_VIP.Points <chr>, set_Ages <chr>,
#> #   set_Pieces <chr>, set_minifigs <lgl>, set_availability <chr>,
#> #   set_review_count <dbl>, set_rating_value <dbl>, set_best_rating <dbl>
```

## Brickset

<https://brickset.com/> contains data on historical lego sets as well as
current sets. Unlike Lego.com, we can access Brickset data using an API
(application programming interface). This does require registering for a
brickset account and requesting an API key. All functions for the
brickset.com data start with the `brickset_` prefix.

``` r
brickset_setup() # guides you through the account setup process
```

Once you have your credentials, you can save them to your Rprofile using
`brickset_save_credentials()`. This will also save the credentials as
global
variables.

``` r
brickset_save_credentials("your_username", "your_password", "your_api_key")
```

Then, you can access brickset’s data by authenticating. You may have to
periodically reauthenticate depending on your internet configuration,
but most functions should refresh the authentication automatically.

``` r
brickset_auth()
#> [1] TRUE
```

As with the Lego store, sets on brickset are organized by theme.

``` r
themes <- brickset_get_themes()
```

We can see what themes existed at the beginning…

``` r
# Oldest themes
arrange(themes, yearfrom)
#> # A tibble: 140 x 5
#>    theme                  setcount subthemecount yearfrom yearto
#>    <chr>                     <dbl>         <dbl>    <dbl>  <dbl>
#>  1 System                      433            10     1949   1970
#>  2 Promotional                 441            16     1958   2019
#>  3 Dacta                       165            10     1960   2003
#>  4 Samsonite                   269             7     1961   1979
#>  5 Books                       295            36     1966   2020
#>  6 Trains                      206             7     1966   2006
#>  7 Duplo                      1198            44     1969   2019
#>  8 LEGOLAND                    162             6     1969   1978
#>  9 Universal Building Set       43             3     1969   1987
#> 10 Minitalia                    21             0     1970   1977
#> # … with 130 more rows
```

Or the themes that have been around the longest…

``` r
# Longest running themes
arrange(themes, desc(yearto - yearfrom)) %>%
  head(10)
#> # A tibble: 10 x 5
#>    theme       setcount subthemecount yearfrom yearto
#>    <chr>          <dbl>         <dbl>    <dbl>  <dbl>
#>  1 Promotional      441            16     1958   2019
#>  2 Books            295            36     1966   2020
#>  3 Duplo           1198            44     1969   2019
#>  4 Gear            2195           134     1971   2019
#>  5 Dacta            165            10     1960   2003
#>  6 Technic          419            11     1977   2019
#>  7 Trains           206             7     1966   2006
#>  8 Castle           281            23     1978   2016
#>  9 Space            322            21     1978   2015
#> 10 Basic            410             5     1972   2003
```

Most of the functions described in the [API
documentation](https://brickset.com/tools/webservices/v2) have been
wrapped; the exception is functions which concern a user’s personal
collection.
