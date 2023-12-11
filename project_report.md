Untitled
================

### Load packages

``` r
# Load packages
library(tidyverse)
library(survival)
library(broom)
library(knitr)

# Set default figure options
knitr::opts_chunk$set(
  fig.width = 6,
  out.width = "90%"
)

theme_set(theme_bw() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Abstract

## Introduction (brief context & background of the problem)

dataset of breast cancer patients from a prospective study.

information was collected at baseline, survival months: length of
following up, status: survival status

interested in predicting risk of death based on some/or all predictors.

Variables in dataset:

1.  Age
2.  Race
3.  Marital Status
4.  T Stage: Adjusted AJCC 6th T
5.  N Stage: Adjusted AJCC 6th N
6.  6th Stage: Breast Adjusted AJCC 6th Stage
7.  Differentiate
8.  Grade
9.  A Stage: Regional — A neoplasm that has extended; Distant — A
    neoplasm that has spread to parts of the body remote from
10. Tumor Size: Each indicates exact size in millimeters.
11. Estrogen Status
12. Progesterone Status
13. Regional Node Examined
14. Reginol Node Positive
15. Survival Months
16. Status: Dead / Alive

Background info/research on breast cancer patients:

Breast cancer occurs due to abnormal cell growths in breast tissue.
Although it is most often found in females, 1 out of every 100 diagnosed
patients in the US is a male. Other breast cancer risk factors include,
increase in age, family history or personal history of breast cancer,
radiation exposure, obesity, alcohol use, among many more. An
interesting risk factor: postmenopausal hormone therapy (combines
estrogren and progesterone to treat signs and symptoms of menopause).

Most recently, breast cancer survival rates have increase and number of
deaths decreased.

    ## Rows: 4024 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): Race, Marital Status, T Stage, N Stage, 6th Stage, differentiate, ...
    ## dbl  (5): Age, Tumor Size, Regional Node Examined, Reginol Node Positive, Su...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### logistic

``` r
logistic_model =
  breast_cancer |> 
  glm(status ~ ., data = _, family = binomial())
```

``` r
logistic_model |> 
  MASS::stepAIC(
    direction = "both",
    k = 2,
    trace = 0) |> 
  tidy()
```

    ## # A tibble: 18 × 5
    ##    term                                   estimate std.error statistic  p.value
    ##    <chr>                                     <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)                             -3.98     0.339      -11.7  7.90e-32
    ##  2 age                                      0.0227   0.00557      4.07 4.75e- 5
    ##  3 raceBlack                                0.498    0.161        3.09 2.02e- 3
    ##  4 raceOther                               -0.435    0.202       -2.15 3.12e- 2
    ##  5 marital_statusDivorced                   0.227    0.141        1.61 1.07e- 1
    ##  6 marital_statusSingle                     0.158    0.134        1.18 2.37e- 1
    ##  7 marital_statusWidowed                    0.233    0.192        1.21 2.26e- 1
    ##  8 marital_statusSeparated                  0.840    0.366        2.30 2.17e- 2
    ##  9 x6th_stageIIIA                           0.685    0.141        4.85 1.24e- 6
    ## 10 x6th_stageIIIC                           1.18     0.176        6.73 1.73e-11
    ## 11 x6th_stageIIB                            0.497    0.144        3.45 5.68e- 4
    ## 12 x6th_stageIIIB                           1.30     0.303        4.29 1.81e- 5
    ## 13 differentiateModerately differentiated  -0.392    0.104       -3.75 1.75e- 4
    ## 14 differentiateWell differentiated        -0.924    0.192       -4.80 1.57e- 6
    ## 15 differentiateUndifferentiated            0.990    0.529        1.87 6.12e- 2
    ## 16 estrogen_statusNegative                  0.732    0.177        4.14 3.46e- 5
    ## 17 progesterone_statusNegative              0.578    0.127        4.54 5.61e- 6
    ## 18 regional_prop                            1.23     0.185        6.66 2.81e-11
