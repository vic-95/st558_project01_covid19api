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
    -   [`getDayOneLive()`](#getdayonelive)
-   [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
-   [Wrap Up](#wrap-up)

# Required Packages (will update if additional are needed)

-   `tidyverse`
-   `httr`
-   `jsonlite`

# Functions

-   `getSummary()`
-   `getCountries()`
-   `returnSlug()`
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
      cntry, " is not a country name, slug, or ISO2 valus supported by the api"
    ) ## if there is no match, the resulting tibble will have 0 rows. Throw an error if this happens
  )
  
  return(unname(unlist(slug))) ## turn the slug into a character string and return it
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
getDayOneLive <- function(cntry, caseType){
  params <- vector()
  
  params[1] <- returnSlug(cntry)
  params[2] <- caseType
  
  resp <- GET(
    paste0(
      "https://api.covid19api.com/dayone/country/",params[1],"/status/",params[2],"/live"
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
