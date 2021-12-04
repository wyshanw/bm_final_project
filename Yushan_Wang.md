Yushan Wang
================

## Variable information:

``` r
var <- c("id", "cty", "state", "area",  "pop", "pop18", "pop65",    "docs", "beds", "crimes",   "hsgrad",   "bagrad",   "poverty",  "unemp",    "pcincome", "totalinc", "region")

var_meaning <- c("ID number", "
County name", "State name", "Land area", "Total population", "Percent of population aged 18-34", "Percent of population aged 65+", "Number of active physicians", "Number of hospital beds", "Total serious crimes", "Percent high school graduates", "Percent bachelor’s degrees
", "Percent below poverty level", "Percent unemployment", "Per capita income", "Total personal income", "Geographic region")

var_info <- data.frame(var, var_meaning)

knitr::kable(var_info)
```

| var      | var_meaning                      |
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

## load data

``` r
cdi_data = read_csv("./data/cdi.csv") %>%
  janitor::clean_names() %>%
  mutate(
    cty_state = str_c(cty,",",state),
    docs_rate_1000 = 1000 * docs/pop, # Compute number of doctors/hospital beds per 1000 people.
    beds_rate_1000 = 1000 * beds/pop,
    density = as.numeric(pop)/as.numeric(area),
    crime_rate_1000 = 1000 * crimes/pop) %>% # Compute number of crimes per 1000 people. 
  dplyr::select(-docs,-beds,-crimes) %>%
  relocate(id,cty_state,cty)
```

    ## Rows: 440 Columns: 17

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): cty, state
    ## dbl (15): id, area, pop, pop18, pop65, docs, beds, crimes, hsgrad, bagrad, p...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
cdi_model = cdi_data %>% 
  dplyr::select(-id,-cty_state, -cty,-state) %>% 
  mutate(region = factor(region))
```

## Criteria based selection

``` r
sb = regsubsets(crime_rate_1000 ~ ., data = cdi_model, nvmax = 14)
sumsb = summary(sb) # pop pop18 hsgrad bagrad poverty pcincome totalinc region beds_rate_1000 density
sumsb
```

    ## Subset selection object
    ## Call: regsubsets.formula(crime_rate_1000 ~ ., data = cdi_model, nvmax = 14)
    ## 16 Variables  (and intercept)
    ##                Forced in Forced out
    ## area               FALSE      FALSE
    ## pop                FALSE      FALSE
    ## pop18              FALSE      FALSE
    ## pop65              FALSE      FALSE
    ## hsgrad             FALSE      FALSE
    ## bagrad             FALSE      FALSE
    ## poverty            FALSE      FALSE
    ## unemp              FALSE      FALSE
    ## pcincome           FALSE      FALSE
    ## totalinc           FALSE      FALSE
    ## region2            FALSE      FALSE
    ## region3            FALSE      FALSE
    ## region4            FALSE      FALSE
    ## docs_rate_1000     FALSE      FALSE
    ## beds_rate_1000     FALSE      FALSE
    ## density            FALSE      FALSE
    ## 1 subsets of each size up to 14
    ## Selection Algorithm: exhaustive
    ##           area pop pop18 pop65 hsgrad bagrad poverty unemp pcincome totalinc
    ## 1  ( 1 )  " "  " " " "   " "   " "    " "    " "     " "   " "      " "     
    ## 2  ( 1 )  " "  " " " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 3  ( 1 )  " "  " " " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 4  ( 1 )  " "  " " " "   " "   " "    " "    " "     " "   " "      " "     
    ## 5  ( 1 )  " "  " " " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 6  ( 1 )  " "  "*" " "   " "   " "    " "    " "     " "   " "      "*"     
    ## 7  ( 1 )  " "  "*" " "   " "   " "    " "    " "     " "   " "      "*"     
    ## 8  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 9  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 10  ( 1 ) " "  "*" "*"   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 11  ( 1 ) " "  "*" "*"   " "   " "    "*"    "*"     " "   "*"      "*"     
    ## 12  ( 1 ) " "  "*" "*"   " "   "*"    "*"    "*"     " "   "*"      "*"     
    ## 13  ( 1 ) " "  "*" "*"   " "   "*"    "*"    "*"     "*"   "*"      "*"     
    ## 14  ( 1 ) "*"  "*" "*"   " "   "*"    "*"    "*"     "*"   "*"      "*"     
    ##           region2 region3 region4 docs_rate_1000 beds_rate_1000 density
    ## 1  ( 1 )  " "     " "     " "     " "            " "            "*"    
    ## 2  ( 1 )  " "     " "     " "     " "            " "            "*"    
    ## 3  ( 1 )  " "     "*"     " "     " "            " "            "*"    
    ## 4  ( 1 )  " "     "*"     "*"     " "            "*"            "*"    
    ## 5  ( 1 )  " "     "*"     "*"     " "            "*"            "*"    
    ## 6  ( 1 )  " "     "*"     "*"     " "            "*"            "*"    
    ## 7  ( 1 )  "*"     "*"     "*"     " "            "*"            "*"    
    ## 8  ( 1 )  " "     "*"     "*"     " "            "*"            "*"    
    ## 9  ( 1 )  "*"     "*"     "*"     " "            "*"            "*"    
    ## 10  ( 1 ) "*"     "*"     "*"     " "            "*"            "*"    
    ## 11  ( 1 ) "*"     "*"     "*"     " "            "*"            "*"    
    ## 12  ( 1 ) "*"     "*"     "*"     " "            "*"            "*"    
    ## 13  ( 1 ) "*"     "*"     "*"     " "            "*"            "*"    
    ## 14  ( 1 ) "*"     "*"     "*"     " "            "*"            "*"

``` r
# plot of Cp and Adj-R2 as functions of parameters
par(mfrow=c(1,2))
plot(2:15, sumsb$cp, xlab="No. of parameters", ylab="Cp Statistic") 
abline(0,1)

plot(2:15, sumsb$adjr2, xlab="No of parameters", ylab="Adj R2")
```

![](Yushan_Wang_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

remove totalinc?

## Model building with interaction

region\*poverty 0.2 cut-off

``` r
fit = lm(crime_rate_1000 ~  
                  pop + pop18 + hsgrad + bagrad + 
                  poverty + pcincome + totalinc + region +
                  beds_rate_1000 + density + 
                  region*poverty , data = cdi_model)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + 
    ##     poverty + pcincome + totalinc + region + beds_rate_1000 + 
    ##     density + region * poverty, data = cdi_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -60.262 -10.655  -0.562   9.511  66.241 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -9.144e+01  2.476e+01  -3.693 0.000251 ***
    ## pop              7.998e-05  1.221e-05   6.552 1.65e-10 ***
    ## pop18            8.656e-01  2.740e-01   3.159 0.001698 ** 
    ## hsgrad           5.138e-01  2.585e-01   1.988 0.047482 *  
    ## bagrad          -5.888e-01  2.725e-01  -2.161 0.031239 *  
    ## poverty          1.914e+00  7.115e-01   2.690 0.007436 ** 
    ## pcincome         2.674e-03  5.237e-04   5.106 4.98e-07 ***
    ## totalinc        -3.628e-03  5.875e-04  -6.176 1.54e-09 ***
    ## region2          1.929e+00  6.039e+00   0.319 0.749568    
    ## region3          2.729e+01  5.308e+00   5.140 4.19e-07 ***
    ## region4          1.672e+01  6.727e+00   2.486 0.013316 *  
    ## beds_rate_1000   2.220e+00  4.973e-01   4.465 1.03e-05 ***
    ## density          4.293e-03  5.016e-04   8.559  < 2e-16 ***
    ## poverty:region2  9.568e-01  7.663e-01   1.248 0.212538    
    ## poverty:region3  1.544e-02  6.637e-01   0.023 0.981446    
    ## poverty:region4  4.351e-01  7.808e-01   0.557 0.577637    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.05 on 424 degrees of freedom
    ## Multiple R-squared:  0.6242, Adjusted R-squared:  0.6109 
    ## F-statistic: 46.95 on 15 and 424 DF,  p-value: < 2.2e-16

``` r
anova(fit)
```

    ## Analysis of Variance Table
    ## 
    ## Response: crime_rate_1000
    ##                 Df Sum Sq Mean Sq  F value    Pr(>F)    
    ## pop              1  25721   25721  88.5153 < 2.2e-16 ***
    ## pop18            1   9379    9379  32.2754 2.489e-08 ***
    ## hsgrad           1  24468   24468  84.2014 < 2.2e-16 ***
    ## bagrad           1   6752    6752  23.2367 1.999e-06 ***
    ## poverty          1  44679   44679 153.7539 < 2.2e-16 ***
    ## pcincome         1   4373    4373  15.0473 0.0001215 ***
    ## totalinc         1  22575   22575  77.6865 < 2.2e-16 ***
    ## region           3  29301    9767  33.6114 < 2.2e-16 ***
    ## beds_rate_1000   1   9655    9655  33.2275 1.580e-08 ***
    ## density          1  26824   26824  92.3079 < 2.2e-16 ***
    ## poverty:region   3    911     304   1.0451 0.3723805    
    ## Residuals      424 123209     291                       
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### collinearity

``` r
check_collinearity(fit)
```

    ## Warning: Model has interaction terms. VIFs might be inflated. You may check
    ##   multicollinearity among predictors of a model without interaction terms.

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##            Term  VIF Increased SE Tolerance
    ##             pop 1.00         1.00      1.00
    ##           pop18 1.95         1.40      0.51
    ##          hsgrad 3.30         1.82      0.30
    ##          bagrad 3.41         1.85      0.29
    ##        pcincome 1.03         1.01      0.97
    ##        totalinc 1.00         1.00      1.00
    ##  beds_rate_1000 1.42         1.19      0.71
    ##         density 1.01         1.01      0.99
    ## 
    ## Moderate Correlation
    ## 
    ##     Term  VIF Increased SE Tolerance
    ##  poverty 9.19         3.03      0.11
    ## 
    ## High Correlation
    ## 
    ##            Term    VIF Increased SE Tolerance
    ##          region 227.34        15.08      0.00
    ##  poverty:region 573.24        23.94      0.00

## Model building without interaction

``` r
fit_nest = lm(crime_rate_1000 ~  
                  pop + pop18 + 
                  poverty + pcincome + totalinc + region +
                  beds_rate_1000 + density, data = cdi_model)
summary(fit_nest)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1000 ~ pop + pop18 + poverty + pcincome + 
    ##     totalinc + region + beds_rate_1000 + density, data = cdi_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -63.081 -10.282  -1.156   9.531  67.241 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -4.269e+01  1.032e+01  -4.137 4.24e-05 ***
    ## pop             8.088e-05  1.219e-05   6.633 9.94e-11 ***
    ## pop18           5.849e-01  1.992e-01   2.936   0.0035 ** 
    ## poverty         1.655e+00  2.719e-01   6.086 2.57e-09 ***
    ## pcincome        2.049e-03  3.812e-04   5.376 1.25e-07 ***
    ## totalinc       -3.661e-03  5.863e-04  -6.244 1.03e-09 ***
    ## region2         1.045e+01  2.440e+00   4.283 2.28e-05 ***
    ## region3         2.594e+01  2.366e+00  10.963  < 2e-16 ***
    ## region4         2.101e+01  2.750e+00   7.638 1.45e-13 ***
    ## beds_rate_1000  2.437e+00  4.813e-01   5.062 6.17e-07 ***
    ## density         4.205e-03  4.387e-04   9.585  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.12 on 429 degrees of freedom
    ## Multiple R-squared:  0.6164, Adjusted R-squared:  0.6075 
    ## F-statistic: 68.93 on 10 and 429 DF,  p-value: < 2.2e-16

``` r
anova(fit_nest)
```

    ## Analysis of Variance Table
    ## 
    ## Response: crime_rate_1000
    ##                 Df Sum Sq Mean Sq F value    Pr(>F)    
    ## pop              1  25721   25721  87.739 < 2.2e-16 ***
    ## pop18            1   9379    9379  31.992 2.831e-08 ***
    ## poverty          1  68317   68317 233.038 < 2.2e-16 ***
    ## pcincome         1  11378   11378  38.813 1.114e-09 ***
    ## totalinc         1  22943   22943  78.261 < 2.2e-16 ***
    ## region           3  27606    9202  31.390 < 2.2e-16 ***
    ## beds_rate_1000   1   9803    9803  33.439 1.418e-08 ***
    ## density          1  26935   26935  91.878 < 2.2e-16 ***
    ## Residuals      429 125765     293                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

remove hsgrad, then ba?

## anova

``` r
# compare nested (small vs large) models
# Ho: smaller model is defensible
anova(fit_nest, fit)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: crime_rate_1000 ~ pop + pop18 + poverty + pcincome + totalinc + 
    ##     region + beds_rate_1000 + density
    ## Model 2: crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + beds_rate_1000 + density + region * poverty
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1    429 125765                           
    ## 2    424 123209  5    2555.6 1.7589 0.1201

``` r
# choose nest model(without region*poverty interaction)
```

## test

``` r
fit_test = lm(crime_rate_1000 ~  
                  pop + pop18   
                  + pcincome + totalinc + region +
                  beds_rate_1000 + density + 
                  poverty*pcincome, data = cdi_model)
summary(fit_test)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1000 ~ pop + pop18 + pcincome + totalinc + 
    ##     region + beds_rate_1000 + density + poverty * pcincome, data = cdi_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -58.902  -9.336  -0.832   9.255  73.749 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -2.950e+01  1.051e+01  -2.806  0.00524 ** 
    ## pop               7.203e-05  1.209e-05   5.958 5.34e-09 ***
    ## pop18             5.626e-01  1.949e-01   2.886  0.00410 ** 
    ## pcincome          1.257e-03  4.125e-04   3.047  0.00246 ** 
    ## totalinc         -3.303e-03  5.791e-04  -5.703 2.20e-08 ***
    ## region2           9.792e+00  2.391e+00   4.095 5.06e-05 ***
    ## region3           2.470e+01  2.331e+00  10.596  < 2e-16 ***
    ## region4           1.843e+01  2.751e+00   6.701 6.54e-11 ***
    ## beds_rate_1000    1.213e+00  5.440e-01   2.230  0.02630 *  
    ## density           3.765e-03  4.402e-04   8.554  < 2e-16 ***
    ## poverty          -8.555e-01  6.186e-01  -1.383  0.16739    
    ## pcincome:poverty  2.033e-04  4.523e-05   4.495 8.99e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.75 on 428 degrees of freedom
    ## Multiple R-squared:  0.6337, Adjusted R-squared:  0.6243 
    ## F-statistic: 67.31 on 11 and 428 DF,  p-value: < 2.2e-16

``` r
anova(fit_test)
```

    ## Analysis of Variance Table
    ## 
    ## Response: crime_rate_1000
    ##                   Df Sum Sq Mean Sq F value    Pr(>F)    
    ## pop                1  25721   25721  91.666 < 2.2e-16 ***
    ## pop18              1   9379    9379  33.424 1.430e-08 ***
    ## pcincome           1   6603    6603  23.531 1.724e-06 ***
    ## totalinc           1  24651   24651  87.851 < 2.2e-16 ***
    ## region             3  49672   16557  59.007 < 2.2e-16 ***
    ## beds_rate_1000     1  36018   36018 128.359 < 2.2e-16 ***
    ## density            1  39180   39180 139.631 < 2.2e-16 ***
    ## poverty            1  10859   10859  38.698 1.179e-09 ***
    ## pcincome:poverty   1   5668    5668  20.201 8.986e-06 ***
    ## Residuals        428 120096     281                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
check_collinearity(fit_test)
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##              Term  VIF Increased SE Tolerance
    ##               pop 1.00         1.00      1.00
    ##             pop18 1.02         1.01      0.98
    ##          pcincome 1.06         1.03      0.94
    ##          totalinc 1.02         1.01      0.98
    ##            region 1.20         1.09      0.84
    ##    beds_rate_1000 1.26         1.12      0.79
    ##           density 1.01         1.01      0.99
    ##           poverty 1.18         1.09      0.85
    ##  pcincome:poverty 1.00         1.00      1.00

pcincome*bagrad significant poverty*income significnat, hsgrad, bagrad,
poverty insignificant
