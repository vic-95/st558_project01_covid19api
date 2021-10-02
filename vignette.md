covid19api\_vignette
================
Victoria Seng
9/21/2021

-   [Required Packages (will update if additional are
    needed)](#required-packages-will-update-if-additional-are-needed)
-   [Functions](#functions)
    -   [`getSummary()`](#getsummary)
    -   [`getCountries()`](#getcountries)
    -   [`returnSlug()`](#returnslug)
    -   [`handleTZ()`](#handletz)
    -   [`handleDT()`](#handledt)
    -   [`getDayOneLive()`](#getdayonelive)
    -   [`getByCountryLive()`](#getbycountrylive)
    -   [`getLiveByCountryStatus()`](#getlivebycountrystatus)
    -   [`getLiveByCountryAllStatus()`](#getlivebycountryallstatus)
    -   [`getLiveCsAfterDT()`](#getlivecsafterdt)
-   [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
-   [Wrap Up](#wrap-up)

# Required Packages (will update if additional are needed)

-   `tidyverse`
-   `httr`
-   `jsonlite`
-   `stringr`
-   `lubridate`

# Functions

-   `getSummary()`
-   `getCountries()`
-   `returnSlug()`
-   `handleTZ()`
-   `handleDT()`
-   `getDayOneLive()`

## `getSummary()`

Grabs the daily summary, which shows total and new cases by country and
case type

``` r
getSummary <- function(){
  resp <- GET("https://api.covid19api.com/summary")
  summary <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(summary)
}
```

## `getCountries()`

Grabs all country names, slugs, and abbreviations. It is used inside all
other functions where the user inputs `country` as an argument.

``` r
getCountries <- function(){
  resp <- GET("https://api.covid19api.com/countries")
  countries <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(countries)
}
```

## `returnSlug()`

Takes a quoted string and returns the country slug (if available) as a
character string. Argument can be the name of the country, the slug, or
the ISO2 abbreviation. Throws a specific error if no match is found.

``` r
returnSlug <- function(cntry){
  countries <- getCountries() ## grab all the countries as a tibble
  
  slug <- countries %>%
    filter(
        case_when(
          cntry %in% Country ~ cntry == Country,
          cntry %in% ISO2    ~ cntry == ISO2,
          cntry %in% Slug    ~ cntry == Slug
      ) ## figure out which column the argument is referencing, then filter to the correct row 
    ) %>% 
      select(Slug) ## grab the slug
  
  if(nrow(slug) == 0) stop(
    paste0(
      cntry, " is not a country name, slug, or ISO2 value supported by the api"
    ) ## if there is no match, the resulting tibble will have 0 rows. Throw an error if this happens
  )
  
  return(unname(unlist(slug))) ## turn the slug into a character string and return it
}
```

## `handleTZ()`

This function takes a character string and tries to find a match in
`OlsonNames()`. If an exact match cannot be found, a partial match is
searched for. If one is found, it is used, but a warning is thrown. If
that cannot be found, the function returns a TZ of “UTC” and throws a
warning.

``` r
handleTZ <- function(z = "UTC"){
  #handle the time zone
  olson <- OlsonNames()
  zone <- if(z %in% olson) {z} else {NULL} # if the zone is an exact match, use it
  
  found <- if(is_null(zone)) {
    unlist(
      lapply(
        olson, function(x){
          if(grepl(z, x)) {x} else {NULL}
        }
      )
    )
  } # if not, search for a matching string within olson.
  
  if(is_null(zone) & (length(found) > 1 | is_null(found))) {
    zone <- "UTC"
    warning(
      paste0(z, " is not a recognized time zone. Defaulting to UTC.")
    ) # If no match or too many matches are found, just use UTC. But warn the user.
  } else if(is_null(zone) & length(found) == 1) {
    zone <- found
    warning(
      paste0(z, " is not a recognized time zone. Using closest match, ", found, ".")
    ) # if a single match is found, use it. But warn the user.
  }
  return(zone)
}
```

## `handleDT()`

``` r
handleDT <- function(date, z){
  zone <- if(!is_null(z)){handleTZ(z)} else {"UTC"} # handle the time zone
  dt <- str_replace_all(date, "/", "-") #preliminary data cleaning
  
  cleanDT <- if(
    str_detect(
      dt, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}([:space:]|T)[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"
    )
  ) {ymd_hms(dt, tz = zone)} else if(
    str_detect(
      dt, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}([:space:]|T)[:digit:]{2}:[:digit:]{2}"
    )
  ) {ymd_hm(dt, tz = zone)} else if(
    str_detect(
      dt, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}([:space:]|T)[:digit:]{2}"
    )
  ) {ymd_h(dt, tz = zone)} else if(
    str_detect(
      dt, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}"
    )
  ) {ymd(dt, tz = zone)} else {stop(paste0("Could not parse ", date, " into a datetime object."))}
  
  cleanUTC <- toString(if(zone != "UTC") {with_tz(cleanDT, "UTC")} else {cleanDT}) # translate to UTC if needed
  dtStr <- paste0(substr(cleanUTC,1,10),"T", substr(cleanUTC,12,19),"Z")
  return(dtStr)
}
```

## `getDayOneLive()`

This endpoint returns all cases of the specified case type for the
specified country from its first recorded case to present (updated every
10 minutes.)

Its arguments are `country` and `caseType`.

TO DO: figure out if there is an all option to this endpoint. If there
is, I’ll need to have these be unnamed args and add logic to
accommodate.

``` r
getDayOneLive <- function(country, caseType){
  resp <- GET(
    paste0(
      "https://api.covid19api.com/dayone/country/",returnSlug(country),"/status/",caseType,"/live"
    )
  )
  df <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(df)
}
```

## `getByCountryLive()`

Takes a country, from datetime string, and optionally a to and time zone
string

``` r
getCountryLive <- function(country, from, to = NULL, z = NULL){
  params <- paste0(
    returnSlug(country),"/status/confirmed/live?from=",handleDT(from, z)
  )
  if(!is_null(to)){params <- paste0(params, "&to=", handleDT(to, z))}
  
  resp <- GET(
    paste0(
      "https://api.covid19api.com/country/", params
    )
  )
  df <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(df)
}
```

## `getLiveByCountryStatus()`

takes country, case type

``` r
getLiveByCountryStatus <- function(country, caseType){
  params <- paste0(
    returnSlug(country),"/status/",caseType)
  
  resp <- GET(
    paste0(
      "https://api.covid19api.com/country/", params
    )
  )
  df <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(df)
}
```

## `getLiveByCountryAllStatus()`

takes country name

``` r
getLiveByCountryStatus <- function(country){
  params <- paste0(
    returnSlug(country))
  
  resp <- GET(
    paste0(
      "https://api.covid19api.com/live/country/", params
    )
  )
  df <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(df)
}
```

## `getLiveCsAfterDT()`

``` r
getLiveCsAfterDT <- function(country, from, z = NULL){
  params <- paste0(
    returnSlug(country),"/status/confirmed/live?from=",handleDT(from, z)
  )
  
  resp <- GET(
    paste0(
      "https://api.covid19api.com/country/", params
    )
  )
  df <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(df)
}
```

# Exploratory Data Analysis (EDA)

A few requirements about your EDA are below: \* You should pull data
from at least two endpoints (possibly combining them into one) \* You
should create at least two new variables that are functions of the
variables from a data set you use \* You should create some contingency
tables \* You should create numerical summaries for some quantitative
variables at each setting of some of your categorical variables \* You
should create at least five plots utilizing coloring, grouping, etc. All
plots should have nice labels and titles. ∗ You should have at least one
bar plot, one histogram, one box plot, and one scatter plot

# Wrap Up
