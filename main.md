BIST8130 - Final Proejct Codings
================
11/22/2021

``` r
library(tidyverse)
library(corrplot)
library(leaps)
library(performance)
library(MASS)
```

### Step 1: Data Preprocessing

After importing the csv file containing the County Demographic
Information (CDI) data, we notice that crimes, physicians, and hospital
beds are given as numbers, while other info are given as proportions. We
therefore compute the number of crimes, physicians, and hospital beds
per 1000 people.

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
knitr::kable(head(cdi_data))
```

|  id | cty\_state   | cty       | state | area |     pop | pop18 | pop65 | hsgrad | bagrad | poverty | unemp | pcincome | totalinc | region | docs\_rate\_1000 | beds\_rate\_1000 |    density | crime\_rate\_1000 |
|----:|:-------------|:----------|:------|-----:|--------:|------:|------:|-------:|-------:|--------:|------:|---------:|---------:|-------:|-----------------:|-----------------:|-----------:|------------------:|
|   1 | Los\_Ange,CA | Los\_Ange | CA    | 4060 | 8863164 |  32.1 |   9.7 |   70.0 |   22.3 |    11.6 |   8.0 |    20786 |   184230 |      4 |         2.671394 |         3.125295 |  2183.0453 |          77.73026 |
|   2 | Cook,IL      | Cook      | IL    |  946 | 5105067 |  29.2 |  12.4 |   73.4 |   22.8 |    11.1 |   7.2 |    21729 |   110928 |      2 |         2.968227 |         4.221296 |  5396.4767 |          85.58869 |
|   3 | Harris,TX    | Harris    | TX    | 1729 | 2818199 |  31.3 |   7.1 |   74.9 |   25.4 |    12.5 |   5.7 |    19517 |    55003 |      3 |         2.680080 |         4.417360 |  1629.9589 |          89.96029 |
|   4 | San\_Dieg,CA | San\_Dieg | CA    | 4205 | 2498016 |  33.5 |  10.9 |   81.9 |   25.3 |     8.1 |   6.1 |    19588 |    48931 |      4 |         2.363876 |         2.473563 |   594.0585 |          69.58362 |
|   5 | Orange,CA    | Orange    | CA    |  790 | 2410556 |  32.6 |   9.2 |   81.2 |   27.8 |     5.2 |   4.8 |    24400 |    58818 |      4 |         2.514772 |         2.642129 |  3051.3367 |          59.95463 |
|   6 | Kings,NY     | Kings     | NY    |   71 | 2300664 |  28.3 |  12.4 |   63.7 |   16.6 |    19.5 |   9.5 |    16803 |    38658 |      1 |         2.112868 |         3.886704 | 32403.7183 |         295.98672 |

### Step 2 - Exploratory Analysis

We then take a closer look of each variables, calculate the pairwise
correlations between variables, and list all the correlations between
the crime rate (our interest) and all other variables.

``` r
cdi_data_exp = cdi_data %>%
  dplyr::select(-id,-cty,-state, -cty_state,-region) 

par(mfrow=c(2,3))
boxplot(cdi_data_exp$area,main="Area")
boxplot(cdi_data_exp$pop,main="Population")
boxplot(cdi_data_exp$pop18,main="Population 18-34")
boxplot(cdi_data_exp$pop65,main="Population 65+")
boxplot(cdi_data_exp$hsgrad,main="Highschool grads")
boxplot(cdi_data_exp$bagrad,main="Bachelor's grads")
```

![](main_files/figure-gfm/exploration-1.png)<!-- -->

``` r
par(mfrow=c(2,3))
boxplot(cdi_data_exp$poverty,main="Poverty Rate")
boxplot(cdi_data_exp$unemp,main="Unemployment Rate")
boxplot(cdi_data_exp$pcincome,main="Income Per Capita")
boxplot(cdi_data_exp$totalinc,main="Income Total")
boxplot(cdi_data_exp$docs_rate_1000,main="Active Physicians")
boxplot(cdi_data_exp$beds_rate_1000,main="Hospital Beds")
```

![](main_files/figure-gfm/exploration-2.png)<!-- -->

``` r
par(mfrow=c(1,1))

ggplot(cdi_data,aes(region)) + 
  geom_histogram(binwidth = 0.5) +
  theme_classic() +
  xlab("Region")+
  ylab("Count")
```

![](main_files/figure-gfm/exploration-3.png)<!-- -->

``` r
boxplot(cdi_data_exp$crime_rate_1000,main="Crime Rate",horizontal = TRUE)
```

![](main_files/figure-gfm/exploration-4.png)<!-- -->

``` r
# data exploratory
pairs(cdi_data_exp)
```

![](main_files/figure-gfm/exploration-5.png)<!-- -->

``` r
# correlation plot
cdi_data_cor = cor(cdi_data_exp)
corrplot(cdi_data_cor, type = "upper", diag = FALSE)
```

![](main_files/figure-gfm/exploration-6.png)<!-- -->

``` r
crime_1000_cor = data.frame(cdi_data_cor) %>% 
  dplyr::select("Crime Rate (Per 1000)" = crime_rate_1000) %>% 
  t()

knitr::kable(crime_1000_cor,digits = 2) 
```

|                       | area |  pop | pop18 | pop65 | hsgrad | bagrad | poverty | unemp | pcincome | totalinc | docs\_rate\_1000 | beds\_rate\_1000 | density | crime\_rate\_1000 |
|:----------------------|-----:|-----:|------:|------:|-------:|-------:|--------:|------:|---------:|---------:|-----------------:|-----------------:|--------:|------------------:|
| Crime Rate (Per 1000) | 0.04 | 0.28 |  0.19 | -0.07 |  -0.23 |   0.04 |    0.47 |  0.04 |    -0.08 |     0.23 |             0.31 |             0.36 |    0.48 |                 1 |

### Model construction

Data used for building model:

``` r
cdi_model = cdi_data %>% 
  dplyr::select(-id,-cty_state, -cty,-state) %>% 
  mutate(region = factor(region))
```

``` r
full.fit = lm(crime_rate_1000 ~ ., data = cdi_model)
summary(full.fit) %>% 
  broom::tidy() %>%
  mutate(p_rank = rank(p.value))
```

    ## # A tibble: 17 × 6
    ##    term              estimate  std.error statistic  p.value p_rank
    ##    <chr>                <dbl>      <dbl>     <dbl>    <dbl>  <dbl>
    ##  1 (Intercept)    -95.5       26.7          -3.58  3.80e- 4      9
    ##  2 area            -0.000314   0.000670     -0.469 6.39e- 1     15
    ##  3 pop              0.0000812  0.0000127     6.41  3.88e-10      4
    ##  4 pop18            0.870      0.319         2.73  6.65e- 3     11
    ##  5 pop65           -0.0580     0.295        -0.196 8.44e- 1     17
    ##  6 hsgrad           0.509      0.259         1.96  5.02e- 2     12
    ##  7 bagrad          -0.542      0.286        -1.90  5.83e- 2     13
    ##  8 poverty          1.96       0.372         5.27  2.23e- 7      6
    ##  9 unemp            0.444      0.512         0.867 3.87e- 1     14
    ## 10 pcincome         0.00267    0.000527      5.06  6.31e- 7      7
    ## 11 totalinc        -0.00367    0.000607     -6.04  3.28e- 9      5
    ## 12 region2          9.30       2.63          3.54  4.41e- 4     10
    ## 13 region3         27.3        2.56         10.7   1.08e-23      1
    ## 14 region4         21.4        3.23          6.62  1.08e-10      3
    ## 15 docs_rate_1000  -0.415      0.981        -0.423 6.72e- 1     16
    ## 16 beds_rate_1000   2.75       0.767         3.59  3.70e- 4      8
    ## 17 density          0.00423    0.000461      9.19  1.83e-18      2

``` r
backward = step(full.fit, direction='backward') %>%  broom::tidy() %>%  rename(backward = "term")
```

    ## Start:  AIC=2515.41
    ## crime_rate_1000 ~ area + pop + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + docs_rate_1000 + 
    ##     beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - pop65           1        11 123801 2513.4
    ## - docs_rate_1000  1        52 123843 2513.6
    ## - area            1        64 123854 2513.6
    ## - unemp           1       220 124010 2514.2
    ## <none>                        123790 2515.4
    ## - bagrad          1      1055 124845 2517.1
    ## - hsgrad          1      1129 124919 2517.4
    ## - pop18           1      2176 125967 2521.1
    ## - beds_rate_1000  1      3770 127560 2526.6
    ## - pcincome        1      7488 131278 2539.2
    ## - poverty         1      8114 131904 2541.3
    ## - totalinc        1     10694 134484 2549.9
    ## - pop             1     12027 135817 2554.2
    ## - density         1     24699 148489 2593.5
    ## - region          3     37378 161168 2625.5
    ## 
    ## Step:  AIC=2513.45
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - docs_rate_1000  1        51 123853 2511.6
    ## - area            1        69 123871 2511.7
    ## - unemp           1       211 124012 2512.2
    ## <none>                        123801 2513.4
    ## - bagrad          1      1057 124858 2515.2
    ## - hsgrad          1      1143 124945 2515.5
    ## - pop18           1      3134 126935 2522.4
    ## - beds_rate_1000  1      3993 127795 2525.4
    ## - pcincome        1      7618 131419 2537.7
    ## - poverty         1      8607 132408 2541.0
    ## - totalinc        1     10842 134643 2548.4
    ## - pop             1     12180 135981 2552.7
    ## - density         1     24841 148642 2591.9
    ## - region          3     37399 161200 2623.6
    ## 
    ## Step:  AIC=2511.63
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - area            1        72 123925 2509.9
    ## - unemp           1       207 124060 2510.4
    ## <none>                        123853 2511.6
    ## - hsgrad          1      1188 125041 2513.8
    ## - bagrad          1      1253 125105 2514.1
    ## - pop18           1      3084 126937 2520.4
    ## - beds_rate_1000  1      7105 130957 2534.2
    ## - pcincome        1      7567 131420 2535.7
    ## - poverty         1      8679 132531 2539.4
    ## - totalinc        1     10924 134776 2546.8
    ## - pop             1     12259 136112 2551.2
    ## - density         1     24888 148741 2590.2
    ## - region          3     37348 161201 2621.6
    ## 
    ## Step:  AIC=2509.89
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - unemp           1       196 124120 2508.6
    ## <none>                        123925 2509.9
    ## - hsgrad          1      1219 125144 2512.2
    ## - bagrad          1      1253 125178 2512.3
    ## - pop18           1      3072 126996 2518.7
    ## - beds_rate_1000  1      7166 131091 2532.6
    ## - pcincome        1      7507 131431 2533.8
    ## - poverty         1      8630 132554 2537.5
    ## - totalinc        1     11073 134998 2545.6
    ## - pop             1     12545 136470 2550.3
    ## - density         1     26974 150899 2594.5
    ## - region          3     37292 161217 2619.6
    ## 
    ## Step:  AIC=2508.58
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## <none>                        124120 2508.6
    ## - hsgrad          1      1080 125200 2510.4
    ## - bagrad          1      1524 125644 2511.9
    ## - pop18           1      3070 127190 2517.3
    ## - beds_rate_1000  1      7090 131210 2531.0
    ## - pcincome        1      8195 132315 2534.7
    ## - poverty         1     10410 134531 2542.0
    ## - totalinc        1     11238 135358 2544.7
    ## - pop             1     12697 136817 2549.4
    ## - density         1     26824 150944 2592.7
    ## - region          3     39000 163120 2622.8

``` r
both = step(full.fit, direction = "both") %>% broom::tidy() %>% rename(stepwise = "term")
```

    ## Start:  AIC=2515.41
    ## crime_rate_1000 ~ area + pop + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + docs_rate_1000 + 
    ##     beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - pop65           1        11 123801 2513.4
    ## - docs_rate_1000  1        52 123843 2513.6
    ## - area            1        64 123854 2513.6
    ## - unemp           1       220 124010 2514.2
    ## <none>                        123790 2515.4
    ## - bagrad          1      1055 124845 2517.1
    ## - hsgrad          1      1129 124919 2517.4
    ## - pop18           1      2176 125967 2521.1
    ## - beds_rate_1000  1      3770 127560 2526.6
    ## - pcincome        1      7488 131278 2539.2
    ## - poverty         1      8114 131904 2541.3
    ## - totalinc        1     10694 134484 2549.9
    ## - pop             1     12027 135817 2554.2
    ## - density         1     24699 148489 2593.5
    ## - region          3     37378 161168 2625.5
    ## 
    ## Step:  AIC=2513.45
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - docs_rate_1000  1        51 123853 2511.6
    ## - area            1        69 123871 2511.7
    ## - unemp           1       211 124012 2512.2
    ## <none>                        123801 2513.4
    ## - bagrad          1      1057 124858 2515.2
    ## + pop65           1        11 123790 2515.4
    ## - hsgrad          1      1143 124945 2515.5
    ## - pop18           1      3134 126935 2522.4
    ## - beds_rate_1000  1      3993 127795 2525.4
    ## - pcincome        1      7618 131419 2537.7
    ## - poverty         1      8607 132408 2541.0
    ## - totalinc        1     10842 134643 2548.4
    ## - pop             1     12180 135981 2552.7
    ## - density         1     24841 148642 2591.9
    ## - region          3     37399 161200 2623.6
    ## 
    ## Step:  AIC=2511.63
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - area            1        72 123925 2509.9
    ## - unemp           1       207 124060 2510.4
    ## <none>                        123853 2511.6
    ## + docs_rate_1000  1        51 123801 2513.4
    ## + pop65           1        10 123843 2513.6
    ## - hsgrad          1      1188 125041 2513.8
    ## - bagrad          1      1253 125105 2514.1
    ## - pop18           1      3084 126937 2520.4
    ## - beds_rate_1000  1      7105 130957 2534.2
    ## - pcincome        1      7567 131420 2535.7
    ## - poverty         1      8679 132531 2539.4
    ## - totalinc        1     10924 134776 2546.8
    ## - pop             1     12259 136112 2551.2
    ## - density         1     24888 148741 2590.2
    ## - region          3     37348 161201 2621.6
    ## 
    ## Step:  AIC=2509.89
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - unemp           1       196 124120 2508.6
    ## <none>                        123925 2509.9
    ## + area            1        72 123853 2511.6
    ## + docs_rate_1000  1        54 123871 2511.7
    ## + pop65           1        15 123910 2511.8
    ## - hsgrad          1      1219 125144 2512.2
    ## - bagrad          1      1253 125178 2512.3
    ## - pop18           1      3072 126996 2518.7
    ## - beds_rate_1000  1      7166 131091 2532.6
    ## - pcincome        1      7507 131431 2533.8
    ## - poverty         1      8630 132554 2537.5
    ## - totalinc        1     11073 134998 2545.6
    ## - pop             1     12545 136470 2550.3
    ## - density         1     26974 150899 2594.5
    ## - region          3     37292 161217 2619.6
    ## 
    ## Step:  AIC=2508.58
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## <none>                        124120 2508.6
    ## + unemp           1       196 123925 2509.9
    ## + area            1        60 124060 2510.4
    ## - hsgrad          1      1080 125200 2510.4
    ## + docs_rate_1000  1        50 124070 2510.4
    ## + pop65           1         4 124116 2510.6
    ## - bagrad          1      1524 125644 2511.9
    ## - pop18           1      3070 127190 2517.3
    ## - beds_rate_1000  1      7090 131210 2531.0
    ## - pcincome        1      8195 132315 2534.7
    ## - poverty         1     10410 134531 2542.0
    ## - totalinc        1     11238 135358 2544.7
    ## - pop             1     12697 136817 2549.4
    ## - density         1     26824 150944 2592.7
    ## - region          3     39000 163120 2622.8

``` r
bind_cols(backward[-1,1],both[-1,1]) %>% knitr::kable()
```

| backward         | stepwise         |
|:-----------------|:-----------------|
| pop              | pop              |
| pop18            | pop18            |
| hsgrad           | hsgrad           |
| bagrad           | bagrad           |
| poverty          | poverty          |
| pcincome         | pcincome         |
| totalinc         | totalinc         |
| region2          | region2          |
| region3          | region3          |
| region4          | region4          |
| beds\_rate\_1000 | beds\_rate\_1000 |
| density          | density          |

## Criteria based selection

pop pop18 hsgrad bagrad poverty pcincome totalinc region
beds\_rate\_1000 density

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

![](main_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Interaction

Does the relationship between the crime\_rate\_1000 and poverty vary by
region status?

``` r
ggplot(cdi_data, aes(x = poverty, y = crime_rate_1000, color = region, alpha = .5)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(group = region, color = region)) 
```

    ## `geom_smooth()` using formula 'y ~ x'

![](main_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# fit model with interaction
interact = lm(crime_rate_1000 ~ poverty*region, data = cdi_data)
summary(interact)
```

    ## 
    ## Call:
    ## lm(formula = crime_rate_1000 ~ poverty * region, data = cdi_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -68.988 -14.192  -0.386  11.921 193.219 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -5.3110     6.3170  -0.841    0.401    
    ## poverty          6.0407     0.7897   7.650 1.30e-13 ***
    ## region          16.4326     2.3909   6.873 2.19e-11 ***
    ## poverty:region  -1.3409     0.2768  -4.844 1.77e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 22.78 on 436 degrees of freedom
    ## Multiple R-squared:  0.3096, Adjusted R-squared:  0.3049 
    ## F-statistic: 65.18 on 3 and 436 DF,  p-value: < 2.2e-16

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

## find outlier

``` r
plot(fit_test, which = 4) 
```

![](main_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
cdiOut = cdi_model[-c(1,6),]

# fit model with and without influential points
with = fit_test 

without = lm(crime_rate_1000 ~  
                  pop + pop18   
                  + pcincome + totalinc + region +
                  beds_rate_1000 + density + 
                  poverty*pcincome, data = cdiOut)

summary(with); summary(without) 
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

    ## 
    ## Call:
    ## lm(formula = crime_rate_1000 ~ pop + pop18 + pcincome + totalinc + 
    ##     region + beds_rate_1000 + density + poverty * pcincome, data = cdiOut)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.441  -9.094  -0.221   8.454  61.470 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -3.637e+01  9.917e+00  -3.668 0.000276 ***
    ## pop               5.928e-05  1.157e-05   5.125 4.52e-07 ***
    ## pop18             7.543e-01  1.841e-01   4.098 4.99e-05 ***
    ## pcincome          1.177e-03  3.879e-04   3.033 0.002570 ** 
    ## totalinc         -2.452e-03  5.524e-04  -4.439 1.15e-05 ***
    ## region2           9.541e+00  2.236e+00   4.267 2.44e-05 ***
    ## region3           2.475e+01  2.178e+00  11.359  < 2e-16 ***
    ## region4           1.827e+01  2.571e+00   7.105 5.09e-12 ***
    ## beds_rate_1000    1.622e+00  5.112e-01   3.173 0.001617 ** 
    ## density           4.066e-04  5.973e-04   0.681 0.496490    
    ## poverty          -1.313e+00  5.826e-01  -2.254 0.024710 *  
    ## pcincome:poverty  2.467e-04  4.302e-05   5.735 1.85e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.65 on 426 degrees of freedom
    ## Multiple R-squared:  0.6138, Adjusted R-squared:  0.6038 
    ## F-statistic: 61.54 on 11 and 426 DF,  p-value: < 2.2e-16

``` r
par(mfrow = c(2,2))
plot(without)
```

![](main_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
boxcox(without)
```

![](main_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->
