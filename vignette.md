covid19api\_vignette
================
Victoria Seng
9/21/2021

-   [Required Packages (will update if additional are
    needed)](#required-packages-will-update-if-additional-are-needed)
-   [Functions](#functions)
-   [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)

# Required Packages (will update if additional are needed)

-   `tidyverse`
-   `httr`
-   `jsonlite`

# Functions

right now, this is just the spot where I’m testing out calling the api.
Will replace this with a function later.

``` r
library(tidyverse)
library(httr)
library(jsonlite)

resp <- GET("https://api.covid19api.com/countries")
str(resp) #shows that the data is in content as a raw vector like in hw4
```

    ## List of 10
    ##  $ url        : chr "https://api.covid19api.com/countries"
    ##  $ status_code: int 200
    ##  $ headers    :List of 22
    ##   ..$ date                            : chr "Wed, 22 Sep 2021 01:08:54 GMT"
    ##   ..$ content-type                    : chr "application/json"
    ##   ..$ transfer-encoding               : chr "chunked"
    ##   ..$ connection                      : chr "keep-alive"
    ##   ..$ accept-ranges                   : chr "bytes"
    ##   ..$ access-control-allow-credentials: chr "true"
    ##   ..$ access-control-allow-origin     : chr ""
    ##   ..$ access-control-expose-headers   : chr "Content-Length"
    ##   ..$ content-encoding                : chr "gzip"
    ##   ..$ last-modified                   : chr "Tue, 21 Sep 2021 16:25:07 GMT"
    ##   ..$ strict-transport-security       : chr "max-age=15724800; includeSubDomains"
    ##   ..$ vary                            : chr "Origin"
    ##   ..$ vary                            : chr "Accept-Encoding"
    ##   ..$ x-content-type-options          : chr "nosniff"
    ##   ..$ x-dns-prefetch-control          : chr "off"
    ##   ..$ x-download-options              : chr "noopen"
    ##   ..$ x-frame-options                 : chr "DENY"
    ##   ..$ x-ratelimit-limit               : chr "10"
    ##   ..$ x-ratelimit-remaining           : chr "9"
    ##   ..$ x-ratelimit-reset               : chr "1632272939"
    ##   ..$ x-request-id                    : chr "15cd2c361f1bcf7823e1874ef0fc58dc"
    ##   ..$ x-xss-protection                : chr "1; mode=block"
    ##   ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ all_headers:List of 1
    ##   ..$ :List of 3
    ##   .. ..$ status : int 200
    ##   .. ..$ version: chr "HTTP/1.1"
    ##   .. ..$ headers:List of 22
    ##   .. .. ..$ date                            : chr "Wed, 22 Sep 2021 01:08:54 GMT"
    ##   .. .. ..$ content-type                    : chr "application/json"
    ##   .. .. ..$ transfer-encoding               : chr "chunked"
    ##   .. .. ..$ connection                      : chr "keep-alive"
    ##   .. .. ..$ accept-ranges                   : chr "bytes"
    ##   .. .. ..$ access-control-allow-credentials: chr "true"
    ##   .. .. ..$ access-control-allow-origin     : chr ""
    ##   .. .. ..$ access-control-expose-headers   : chr "Content-Length"
    ##   .. .. ..$ content-encoding                : chr "gzip"
    ##   .. .. ..$ last-modified                   : chr "Tue, 21 Sep 2021 16:25:07 GMT"
    ##   .. .. ..$ strict-transport-security       : chr "max-age=15724800; includeSubDomains"
    ##   .. .. ..$ vary                            : chr "Origin"
    ##   .. .. ..$ vary                            : chr "Accept-Encoding"
    ##   .. .. ..$ x-content-type-options          : chr "nosniff"
    ##   .. .. ..$ x-dns-prefetch-control          : chr "off"
    ##   .. .. ..$ x-download-options              : chr "noopen"
    ##   .. .. ..$ x-frame-options                 : chr "DENY"
    ##   .. .. ..$ x-ratelimit-limit               : chr "10"
    ##   .. .. ..$ x-ratelimit-remaining           : chr "9"
    ##   .. .. ..$ x-ratelimit-reset               : chr "1632272939"
    ##   .. .. ..$ x-request-id                    : chr "15cd2c361f1bcf7823e1874ef0fc58dc"
    ##   .. .. ..$ x-xss-protection                : chr "1; mode=block"
    ##   .. .. ..- attr(*, "class")= chr [1:2] "insensitive" "list"
    ##  $ cookies    :'data.frame': 0 obs. of  7 variables:
    ##   ..$ domain    : logi(0) 
    ##   ..$ flag      : logi(0) 
    ##   ..$ path      : logi(0) 
    ##   ..$ secure    : logi(0) 
    ##   ..$ expiration: 'POSIXct' num(0) 
    ##   ..$ name      : logi(0) 
    ##   ..$ value     : logi(0) 
    ##  $ content    : raw [1:24258] 5b 0a 20 20 ...
    ##  $ date       : POSIXct[1:1], format: "2021-09-22 01:08:54"
    ##  $ times      : Named num [1:6] 0 0.0388 0.151 0.372 0.4787 ...
    ##   ..- attr(*, "names")= chr [1:6] "redirect" "namelookup" "connect" "pretransfer" ...
    ##  $ request    :List of 7
    ##   ..$ method    : chr "GET"
    ##   ..$ url       : chr "https://api.covid19api.com/countries"
    ##   ..$ headers   : Named chr "application/json, text/xml, application/xml, */*"
    ##   .. ..- attr(*, "names")= chr "Accept"
    ##   ..$ fields    : NULL
    ##   ..$ options   :List of 2
    ##   .. ..$ useragent: chr "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.2"
    ##   .. ..$ httpget  : logi TRUE
    ##   ..$ auth_token: NULL
    ##   ..$ output    : list()
    ##   .. ..- attr(*, "class")= chr [1:2] "write_memory" "write_function"
    ##   ..- attr(*, "class")= chr "request"
    ##  $ handle     :Class 'curl_handle' <externalptr> 
    ##  - attr(*, "class")= chr "response"

``` r
resPretty <- as_tibble(fromJSON(rawToChar(resp$content)))
head(resPretty, 10)
```

    ## # A tibble: 10 x 3
    ##    Country                       Slug                        ISO2 
    ##    <chr>                         <chr>                       <chr>
    ##  1 Cambodia                      cambodia                    KH   
    ##  2 Congo (Brazzaville)           congo-brazzaville           CG   
    ##  3 Grenada                       grenada                     GD   
    ##  4 Lebanon                       lebanon                     LB   
    ##  5 Oman                          oman                        OM   
    ##  6 Ukraine                       ukraine                     UA   
    ##  7 Nigeria                       nigeria                     NG   
    ##  8 Swaziland                     swaziland                   SZ   
    ##  9 Australia                     australia                   AU   
    ## 10 Holy See (Vatican City State) holy-see-vatican-city-state VA

# Exploratory Data Analysis (EDA)
