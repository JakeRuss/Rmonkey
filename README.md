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
s <- survey_list(per_page = 200) # increase the per_page if your survey is really old and you give a lot of surveys
test_survey <- find_survey_by_name("testing rmonkey", s)
```

Then get the responses from that survey into a data.frame:

``` r
dat <- survey_responses(test_survey)
dat
```
