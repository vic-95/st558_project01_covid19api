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
    -   [`handleDates()`](#handledates)
    -   [`getDayOneLive()`](#getdayonelive)
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
-   `handleDates()`
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

TO DO: I might try to make this not case sensitive in the future

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
  
  found <- if(is.null(zone)) {
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
  
  cleanUTC <- if(zone != "UTC") {with_tz(cleanDT, "UTC")} else {cleanDT} # translate to UTC if needed
  return(cleanUTC)
}
```

## `handleDates()`

Takes a vector of date/datetime strings and standardizes their format to
`yyyy-mm-ddThh:mm:ssZ`

TO DO: I think its fine to ask a user to input dates as yyyy/mm/dd and
datetimes as yyyy/mm/ddThh:mm:ss. I’d also like them to be able to
specify their time zone and translate that into UTC for them. So this
function should accept a date OR a datetime and if it is not in UTC,
convert it to UTC.

## `getDayOneLive()`

This endpoint returns all cases of the specified case type for the
specified country from its first recorded case to present (updated every
10 minutes.)

Its arguments are `country` and `caseType`.

TO DO: figure out if there is an all option to this endpoint. If there
is, I’ll need to have these be unnamed args and add logic to
accommodate.

``` r
getDayOneLive <- function(cntry, caseType){
  resp <- GET(
    paste0(
      "https://api.covid19api.com/dayone/country/",returnSlug(cntry),"/status/",caseType,"/live"
    )
  )
  dayOneLive <- as_tibble(fromJSON(rawToChar(resp$content)))
  return(dayOneLive)
}
```

-   By Country Live params = country, case type, from
-   Live by Country and Status params = country, case type, from
-   Live by Country All Status
-   Live by Country And Status After Date
-   Will need to allow for country name, slug, or abbrev to work in args
    (maybe try to find partial matches too?)
-   Will need to allow for various date and datetime formats to work as
    arguments
-   Decide if I want to do a function per endpoint or have the endpoints
    passed to the function as arguments and have logic inside the
    function.
-   Looking at the examples, I think I’ll try 1+ functions per endpoint
    and a wrapper function around them.

# Exploratory Data Analysis (EDA)

# Wrap Up
