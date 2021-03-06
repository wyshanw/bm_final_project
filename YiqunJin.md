YiqunJin
================
Yiqun Jin
11/22/2021

## Load Data

``` r
cdi_df = read.csv("data/cdi.csv") %>% 
  janitor::clean_names() %>%
  mutate(
    cty_state = str_c(cty,",",state),
    docs_rate_1000 = 1000 * docs/pop, # Compute number of doctors/hospital beds per 1000 people.
    beds_rate_1000 = 1000 * beds/pop,
    crime_rate_1000 = 1000 * crimes/pop) %>% # Compute number of crimes per 1000 people.) 
  select(-docs,-beds,-crimes) %>%
  relocate(id,cty_state,cty)
```

## Crime Rate Correlation

``` r
cdi_cor = cdi_df %>% 
  select(-id,-cty_state, -cty, -state) %>% 
  cor() 
 
corrplot(cdi_cor)
```

![](YiqunJin_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

According to the plot above, we can see the poverty (Percent below
poverty level), beds(Number of hospital beds), docs (Number of active
physicians) have positive relationship with crime rate.

Percent high school graduates (Percent of persons 25 years old or older
twho completed 12 or more years of school) show a light negative
relationship with crime rate.

## Variable Information

| var      | var\_meaning                     |
|:---------|:---------------------------------|
| id       | ID number                        |
| cty      | County name                      |
| state    | State name                       |
| area     | Land area                        |
| pop      | Total population                 |
| pop18    | Percent of population aged 18-34 |
| pop65    | Percent of population aged 65+   |
| docs     | Number of active physicians      |
| beds     | Number of hospital beds          |
| crimes   | Total serious crimes             |
| hsgrad   | Percent high school graduates    |
| bagrad   | Percent bachelor’s degrees       |
| poverty  | Percent below poverty level      |
| unemp    | Percent unemployment             |
| pcincome | Per capita income                |
| totalinc | Total personal income            |
| region   | Geographic region                |

## Crime Rate v.s. Poverty

Poverty: Percent of 1990 total population with income below poverty
level

``` r
ggplot(cdi_df, aes(x = poverty, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
reg_poverty = lm(cdi_df$crime_rate_1000 ~ cdi_df$poverty) 
summary(reg_poverty)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$poverty)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -64.008 -14.578  -2.561  13.605 208.853 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     33.1390     2.4435   13.56   <2e-16 ***
    ## cdi_df$poverty   2.7690     0.2472   11.20   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 24.12 on 438 degrees of freedom
    ## Multiple R-squared:  0.2226, Adjusted R-squared:  0.2209 
    ## F-statistic: 125.4 on 1 and 438 DF,  p-value: < 2.2e-16

``` r
fitted_value =  reg_poverty$fitted.values
reg_poverty %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term           estimate std.error statistic  p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       33.1      2.44       13.6 3.14e-35
    ## 2 cdi_df$poverty     2.77     0.247      11.2 8.92e-26

## Crime Rate v.s. Beds

Beds: Total number of beds, cribs, and bassinets during 1990

``` r
ggplot(cdi_df, aes(x = beds_rate_1000, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
reg_beds = lm(cdi_df$crime_rate_1000 ~ cdi_df$beds_rate_1000) 
summary(reg_beds)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$beds_rate_1000)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -65.817 -16.918  -2.435  14.607 237.519 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            39.1234     2.5284  15.474  < 2e-16 ***
    ## cdi_df$beds_rate_1000   4.9771     0.6076   8.191 2.87e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 25.48 on 438 degrees of freedom
    ## Multiple R-squared:  0.1328, Adjusted R-squared:  0.1308 
    ## F-statistic: 67.09 on 1 and 438 DF,  p-value: 2.875e-15

``` r
reg_beds %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              39.1      2.53      15.5  2.12e-43
    ## 2 cdi_df$beds_rate_1000     4.98     0.608      8.19 2.87e-15

## Crime Rate v.s. Docs

docs: Number of active physicians

``` r
ggplot(cdi_df, aes(x = docs_rate_1000, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
reg_docs = lm(cdi_df$crime_rate_1000 ~ cdi_df$docs_rate_1000) 
summary(reg_docs)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$docs_rate_1000)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -98.454 -17.719  -3.074  16.651 238.756 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            45.6642     2.1220  21.520  < 2e-16 ***
    ## cdi_df$docs_rate_1000   5.4744     0.8107   6.753 4.62e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.04 on 438 degrees of freedom
    ## Multiple R-squared:  0.0943, Adjusted R-squared:  0.09223 
    ## F-statistic:  45.6 on 1 and 438 DF,  p-value: 4.616e-11

``` r
reg_docs %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              45.7      2.12      21.5  1.29e-70
    ## 2 cdi_df$docs_rate_1000     5.47     0.811      6.75 4.62e-11

## Crime Rate v.s. hsgrad

hsgrad: Percent of persons 25 years old or older who completed 12 or
more years of school

``` r
ggplot(cdi_df, aes(x = hsgrad, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
reg_docs = lm(cdi_df$crime_rate_1000 ~ cdi_df$hsgrad) 
summary(reg_docs)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$hsgrad)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -54.07 -18.46  -3.64  16.37 226.47 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   125.6947    14.1191   8.902  < 2e-16 ***
    ## cdi_df$hsgrad  -0.8820     0.1813  -4.865  1.6e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.65 on 438 degrees of freedom
    ## Multiple R-squared:  0.05126,    Adjusted R-squared:  0.0491 
    ## F-statistic: 23.67 on 1 and 438 DF,  p-value: 1.601e-06

``` r
reg_docs %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    126.       14.1        8.90 1.46e-17
    ## 2 cdi_df$hsgrad   -0.882     0.181     -4.86 1.60e- 6

## Crime Rate v.s. pop

pop: Estimated 1990 population

``` r
ggplot(cdi_df, aes(x = pop, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
reg_pop = lm(cdi_df$crime_rate_1000 ~ cdi_df$pop) 
summary(reg_pop)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$pop)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -87.26 -18.37  -3.65  14.44 214.44 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.229e+01  1.496e+00  34.957  < 2e-16 ***
    ## cdi_df$pop  1.271e-05  2.082e-06   6.106 2.25e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.26 on 438 degrees of freedom
    ## Multiple R-squared:  0.07846,    Adjusted R-squared:  0.07635 
    ## F-statistic: 37.29 on 1 and 438 DF,  p-value: 2.248e-09

``` r
reg_pop %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term          estimate  std.error statistic   p.value
    ##   <chr>            <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept) 52.3       1.50           35.0  8.41e-129
    ## 2 cdi_df$pop   0.0000127 0.00000208      6.11 2.25e-  9

## Crime Rate v.s. pop18

pop18: Percent of total population in age range from 18-34

``` r
ggplot(cdi_df, aes(x = pop18, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
reg_pop18 = lm(cdi_df$crime_rate_1000 ~ cdi_df$pop18) 
summary(reg_pop18)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$pop18)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -63.493 -18.850  -3.212  15.114 239.034 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   21.7875     8.8309   2.467    0.014 *  
    ## cdi_df$pop18   1.2426     0.3058   4.063 5.75e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.86 on 438 degrees of freedom
    ## Multiple R-squared:  0.03632,    Adjusted R-squared:  0.03412 
    ## F-statistic: 16.51 on 1 and 438 DF,  p-value: 5.747e-05

``` r
reg_pop18 %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term         estimate std.error statistic   p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     21.8      8.83       2.47 0.0140   
    ## 2 cdi_df$pop18     1.24     0.306      4.06 0.0000575

## Crime Rate v.s. pop65

``` r
ggplot(cdi_df, aes(x = pop65, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
reg_pop65 = lm(cdi_df$crime_rate_1000 ~ cdi_df$pop65) 
summary(reg_pop65)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$pop65)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -53.810 -18.991  -4.039  15.095 238.805 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   62.8284     4.1790  15.034   <2e-16 ***
    ## cdi_df$pop65  -0.4554     0.3263  -1.396    0.164    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.3 on 438 degrees of freedom
    ## Multiple R-squared:  0.004427,   Adjusted R-squared:  0.002154 
    ## F-statistic: 1.948 on 1 and 438 DF,  p-value: 0.1636

``` r
reg_pop65 %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term         estimate std.error statistic  p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    62.8       4.18      15.0  1.73e-41
    ## 2 cdi_df$pop65   -0.455     0.326     -1.40 1.64e- 1

## Crime Rate v.s. Area

Land area measured in square miles

``` r
ggplot(cdi_df, aes(x = area, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
reg_area = lm(cdi_df$crime_rate_1000 ~ cdi_df$area) 
summary(reg_area)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$area)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -52.216 -18.968  -4.955  15.294 239.435 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 5.650e+01  1.570e+00   35.98   <2e-16 ***
    ## cdi_df$area 7.573e-04  8.417e-04    0.90    0.369    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.33 on 438 degrees of freedom
    ## Multiple R-squared:  0.001845,   Adjusted R-squared:  -0.0004343 
    ## F-statistic: 0.8094 on 1 and 438 DF,  p-value: 0.3688

``` r
reg_pop65 %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term         estimate std.error statistic  p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    62.8       4.18      15.0  1.73e-41
    ## 2 cdi_df$pop65   -0.455     0.326     -1.40 1.64e- 1

## Crime Rate v.s. bagrad

Percent of persons 25 years old or older with bachelor’s degrees

``` r
ggplot(cdi_df, aes(x = bagrad, y = crime_rate_1000)) + geom_point(alpha = .5) + geom_smooth(method = "lm", se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
reg_bagrad = lm(cdi_df$crime_rate_1000 ~ cdi_df$bagrad) 
summary(reg_bagrad)
```

    ## 
    ## Call:
    ## lm(formula = cdi_df$crime_rate_1000 ~ cdi_df$bagrad)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -52.264 -19.407  -4.478  15.727 239.313 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    54.4035     3.8226  14.232   <2e-16 ***
    ## cdi_df$bagrad   0.1368     0.1705   0.802    0.423    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 27.34 on 438 degrees of freedom
    ## Multiple R-squared:  0.001467,   Adjusted R-squared:  -0.0008125 
    ## F-statistic: 0.6436 on 1 and 438 DF,  p-value: 0.4228

``` r
reg_bagrad %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)     54.4       3.82     14.2   4.74e-38
    ## 2 cdi_df$bagrad    0.137     0.170     0.802 4.23e- 1

## Multiple Linear Regression

``` r
mlr = lm(crime_rate_1000 ~ docs_rate_1000 + beds_rate_1000 + poverty, data = cdi_df)

qplot(x = poverty, y = crime_rate_1000, color = factor(region), data = cdi_df, alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
```

    ## `geom_smooth()` using formula 'y ~ x'

![](YiqunJin_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## Abstract

## Introduction

## Methods

## Results

## Conclusion/Discussion
