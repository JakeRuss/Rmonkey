<!-- README.md is generated from README.Rmd. Please edit that file -->
Rmonkey provides access to Survey Monkey, for the complete integration of survey data collection and analysis into a single, easily reproducible workflow. It used to be on CRAN. SurveyMoneky broke the package when they changed their API, and it's only on GitHub for now as it comes back to life.

Installation
------------

Rmonkey is available on GitHub:

``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("sfirke/Rmonkey")
#> Downloading GitHub repo sfirke/Rmonkey@master
#> from URL https://api.github.com/repos/sfirke/Rmonkey/zipball/master
#> Installing Rmonkey
#> "C:/PROGRA~1/R/R-33~1.2/bin/x64/R" --no-site-file --no-environ --no-save  \
#>   --no-restore --quiet CMD INSTALL  \
#>   "C:/Users/SFirke/AppData/Local/Temp/RtmpOQxnjd/devtools37852006c92/sfirke-Rmonkey-85d1c88"  \
#>   --library="C:/Users/SFirke/Documents/R/win-library/3.3"  \
#>   --install-tests
#> 
```

Setup
-----

To use Rmonkey, the user must have a Survey Monkey account, a Mashery Survey Monkey Developer account, and a registered API application. To create a Survey Monkey account, visit <https://www.surveymonkey.com/user/sign-in/>. You'll need to get an OAuth token (*provide more detailed instructions*).

Once everything is registered, you can load your token with:

``` r
# Use your OAuth token here
options(sm_oauth_token = "yourtoken")
```

Using Rmonkey
-------------

### Retrieving survey responses

First get a list of surveys in your account and fetch the one you want by name:

``` r
s <- survey_list(per_page = 200) # might need to increase to get the one you're looking for if your survey is really old and you give a lot of surveys
test_survey <- find_survey_by_name("testing rmonkey", s)
```

Then get the responses from that survey into a data.frame:

``` r
dat <- survey_responses(test_survey)
dat
```
