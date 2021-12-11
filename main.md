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

|  id | cty_state   | cty      | state | area |     pop | pop18 | pop65 | hsgrad | bagrad | poverty | unemp | pcincome | totalinc | region | docs_rate_1000 | beds_rate_1000 |    density | crime_rate_1000 |
|----:|:------------|:---------|:------|-----:|--------:|------:|------:|-------:|-------:|--------:|------:|---------:|---------:|-------:|---------------:|---------------:|-----------:|----------------:|
|   1 | Los_Ange,CA | Los_Ange | CA    | 4060 | 8863164 |  32.1 |   9.7 |   70.0 |   22.3 |    11.6 |   8.0 |    20786 |   184230 |      4 |       2.671394 |       3.125295 |  2183.0453 |        77.73026 |
|   2 | Cook,IL     | Cook     | IL    |  946 | 5105067 |  29.2 |  12.4 |   73.4 |   22.8 |    11.1 |   7.2 |    21729 |   110928 |      2 |       2.968227 |       4.221296 |  5396.4767 |        85.58869 |
|   3 | Harris,TX   | Harris   | TX    | 1729 | 2818199 |  31.3 |   7.1 |   74.9 |   25.4 |    12.5 |   5.7 |    19517 |    55003 |      3 |       2.680080 |       4.417360 |  1629.9589 |        89.96029 |
|   4 | San_Dieg,CA | San_Dieg | CA    | 4205 | 2498016 |  33.5 |  10.9 |   81.9 |   25.3 |     8.1 |   6.1 |    19588 |    48931 |      4 |       2.363876 |       2.473563 |   594.0585 |        69.58362 |
|   5 | Orange,CA   | Orange   | CA    |  790 | 2410556 |  32.6 |   9.2 |   81.2 |   27.8 |     5.2 |   4.8 |    24400 |    58818 |      4 |       2.514772 |       2.642129 |  3051.3367 |        59.95463 |
|   6 | Kings,NY    | Kings    | NY    |   71 | 2300664 |  28.3 |  12.4 |   63.7 |   16.6 |    19.5 |   9.5 |    16803 |    38658 |      1 |       2.112868 |       3.886704 | 32403.7183 |       295.98672 |

### Step 2 - Exploratory Analysis

We then take a closer look of each variables, calculate the pairwise
correlations between variables, and list all the correlations between
the crime rate (our interest) and all other variables.

``` r
cdi_data_exp = cdi_data %>%
  dplyr::select(-id,-cty,-state, -cty_state) 

cdi_data_original = cdi_data_exp

cdi_data_exp = cdi_data_exp %>%
  mutate(
    area = ifelse(area %in% boxplot.stats(cdi_data_exp$area)$out,NA,area),
    pop = ifelse(pop %in% boxplot.stats(cdi_data_exp$pop)$out,NA,pop),
    crime_rate_1000 = ifelse(crime_rate_1000 %in% boxplot.stats(cdi_data_exp$crime_rate_1000)$out,NA,crime_rate_1000),
    poverty = ifelse(poverty %in% boxplot.stats(cdi_data_exp$poverty)$out,NA,poverty)
  ) %>%
  na.omit()


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

|                       |  area |  pop | pop18 | pop65 | hsgrad | bagrad | poverty | unemp | pcincome | totalinc | region | docs_rate_1000 | beds_rate_1000 | density | crime_rate_1000 |
|:----------------------|------:|-----:|------:|------:|-------:|-------:|--------:|------:|---------:|---------:|-------:|---------------:|---------------:|--------:|----------------:|
| Crime Rate (Per 1000) | -0.04 | 0.29 |  0.24 | -0.08 |  -0.13 |   0.09 |    0.49 | -0.07 |    -0.08 |     0.19 |   0.46 |           0.32 |           0.39 |    0.29 |               1 |

### Remove outliers and high leverage point

``` r
# Remove high leverage points

cdi_data_clean = cdi_data[cdi_data$area >= quantile(cdi_data$area,0.002) & cdi_data$area <= quantile(cdi_data$area,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$pop >= quantile(cdi_data_clean$pop,0.002) & cdi_data_clean$pop <= quantile(cdi_data_clean$pop,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$pop18 >= quantile(cdi_data_clean$pop18,0.002) & cdi_data_clean$pop18 <= quantile(cdi_data_clean$pop18,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$pop65 >= quantile(cdi_data_clean$pop65,0.002) & cdi_data_clean$pop65 <= quantile(cdi_data_clean$pop65,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$hsgrad >= quantile(cdi_data_clean$hsgrad,0.002) & cdi_data_clean$hsgrad <= quantile(cdi_data_clean$hsgrad,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$bagrad >= quantile(cdi_data_clean$bagrad,0.002) & cdi_data_clean$bagrad <= quantile(cdi_data_clean$bagrad,0.998),]

cdi_data_clean = cdi_data_clean[cdi_data_clean$poverty >= quantile(cdi_data_clean$poverty,0.002) & cdi_data_clean$poverty <= quantile(cdi_data_clean$poverty,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$unemp >= quantile(cdi_data_clean$unemp,0.002) & cdi_data_clean$unemp <= quantile(cdi_data_clean$unemp,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$pcincome >= quantile(cdi_data_clean$pcincome,0.002) & cdi_data_clean$pcincome <= quantile(cdi_data_clean$pcincome,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$totalinc >= quantile(cdi_data_clean$totalinc,0.002) & cdi_data_clean$totalinc <= quantile(cdi_data_clean$totalinc,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$docs_rate_1000 >= quantile(cdi_data_clean$docs_rate_1000,0.002) & cdi_data_clean$docs_rate_1000 <= quantile(cdi_data_clean$docs_rate_1000,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$beds_rate_1000 >= quantile(cdi_data_clean$beds_rate_1000,0.002) & cdi_data_clean$beds_rate_1000 <= quantile(cdi_data_clean$beds_rate_1000,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$beds_rate_1000 >= quantile(cdi_data_clean$beds_rate_1000,0.002) & cdi_data_clean$beds_rate_1000 <= quantile(cdi_data_clean$beds_rate_1000,0.998),]
cdi_data_clean = cdi_data_clean[cdi_data_clean$density >= quantile(cdi_data_clean$density,0.002) & cdi_data_clean$density <= quantile(cdi_data_clean$density,0.998),]

cdi_data_clean = cdi_data_clean[cdi_data_clean$crime_rate_1000 >= quantile(cdi_data_clean$crime_rate_1000,0.002) & cdi_data_clean$beds_rate_1000 <= quantile(cdi_data_clean$crime_rate_1000,0.998),]
```

``` r
par(mfrow=c(3,4))
boxplot(cdi_data_clean$area,main="Area")
boxplot(cdi_data_clean$pop,main="Population")
boxplot(cdi_data_clean$pop18,main="Population 18-34")
boxplot(cdi_data_clean$pop65,main="Population 65+")
boxplot(cdi_data_clean$hsgrad,main="Highschool grads")
boxplot(cdi_data_clean$bagrad,main="Bachelor's grads")

boxplot(cdi_data_clean$poverty,main="Poverty Rate")
boxplot(cdi_data_clean$unemp,main="Unemployment Rate")
boxplot(cdi_data_clean$pcincome,main="Income Per Capita")
boxplot(cdi_data_clean$totalinc,main="Income Total")
boxplot(cdi_data_clean$docs_rate_1000,main="Active Physicians")
boxplot(cdi_data_clean$beds_rate_1000,main="Hospital Beds")
```

![](main_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Training/Test set split

``` r
cdi_data_clean = cdi_data_clean %>% 
  dplyr::select(-id,-cty_state, -cty,-state) %>% 
  mutate(region = factor(region))

set.seed(1)
dt = sort(sample(nrow(cdi_data_clean), nrow(cdi_data_clean)*.9))
train_data = cdi_data_clean[dt,]
test_data = cdi_data_clean[-dt,]
```

### Model construction

Data used for building model:

``` r
cdi_model = train_data
```

``` r
full.fit = lm(crime_rate_1000 ~ ., data = cdi_model)
summary(full.fit) %>% 
  broom::tidy() %>%
  mutate(p_rank = rank(p.value))
```

    ## # A tibble: 17 × 6
    ##    term               estimate  std.error statistic  p.value p_rank
    ##    <chr>                 <dbl>      <dbl>     <dbl>    <dbl>  <dbl>
    ##  1 (Intercept)    -107.        29.0          -3.71  2.42e- 4      8
    ##  2 area             -0.000471   0.000881     -0.535 5.93e- 1     16
    ##  3 pop               0.0000788  0.0000130     6.08  3.10e- 9      3
    ##  4 pop18             1.25       0.358         3.50  5.26e- 4      9
    ##  5 pop65             0.0779     0.318         0.245 8.07e- 1     17
    ##  6 hsgrad            0.354      0.276         1.28  2.00e- 1     13
    ##  7 bagrad           -0.687      0.320        -2.15  3.23e- 2     10
    ##  8 poverty           2.18       0.428         5.09  5.95e- 7      6
    ##  9 unemp             0.628      0.532         1.18  2.39e- 1     14
    ## 10 pcincome          0.00325    0.000608      5.35  1.62e- 7      4
    ## 11 totalinc         -0.00333    0.000638     -5.23  2.92e- 7      5
    ## 12 region2          10.6        2.61          4.08  5.63e- 5      7
    ## 13 region3          29.5        2.60         11.4   1.04e-25      1
    ## 14 region4          22.1        3.34          6.62  1.34e-10      2
    ## 15 docs_rate_1000    1.81       1.20          1.51  1.31e- 1     12
    ## 16 beds_rate_1000    1.75       0.834         2.10  3.67e- 2     11
    ## 17 density           0.000743   0.000691      1.07  2.83e- 1     15

``` r
backward = step(full.fit, direction='backward') %>%  broom::tidy() %>%  rename(backward = "term")
```

    ## Start:  AIC=2059.35
    ## crime_rate_1000 ~ area + pop + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + docs_rate_1000 + 
    ##     beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - pop65           1        15  89297 2057.4
    ## - area            1        73  89354 2057.7
    ## - density         1       293  89574 2058.6
    ## - unemp           1       353  89635 2058.8
    ## - hsgrad          1       418  89699 2059.1
    ## <none>                         89281 2059.3
    ## - docs_rate_1000  1       581  89863 2059.7
    ## - beds_rate_1000  1      1116  90397 2061.9
    ## - bagrad          1      1171  90452 2062.2
    ## - pop18           1      3107  92388 2070.0
    ## - poverty         1      6562  95844 2083.5
    ## - totalinc        1      6938  96219 2085.0
    ## - pcincome        1      7248  96529 2086.2
    ## - pop             1      9382  98663 2094.2
    ## - region          3     34907 124189 2175.1
    ## 
    ## Step:  AIC=2057.41
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - area            1        67  89364 2055.7
    ## - density         1       304  89601 2056.7
    ## - unemp           1       383  89680 2057.0
    ## - hsgrad          1       408  89705 2057.1
    ## <none>                         89297 2057.4
    ## - docs_rate_1000  1       592  89889 2057.8
    ## - bagrad          1      1169  90466 2060.2
    ## - beds_rate_1000  1      1277  90574 2060.7
    ## - pop18           1      3748  93044 2070.6
    ## - poverty         1      6706  96002 2082.1
    ## - totalinc        1      6926  96222 2083.0
    ## - pcincome        1      7255  96552 2084.2
    ## - pop             1      9371  98668 2092.2
    ## - region          3     34901 124197 2173.2
    ## 
    ## Step:  AIC=2055.69
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - unemp           1       356  89720 2055.2
    ## - density         1       429  89793 2055.5
    ## - hsgrad          1       431  89795 2055.5
    ## <none>                         89364 2055.7
    ## - docs_rate_1000  1       588  89952 2056.1
    ## - bagrad          1      1171  90535 2058.5
    ## - beds_rate_1000  1      1280  90644 2058.9
    ## - pop18           1      3723  93087 2068.8
    ## - poverty         1      6662  96027 2080.2
    ## - totalinc        1      6868  96232 2081.0
    ## - pcincome        1      7226  96590 2082.4
    ## - pop             1      9406  98770 2090.6
    ## - region          3     34833 124198 2171.2
    ## 
    ## Step:  AIC=2055.16
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - hsgrad          1       328  90049 2054.5
    ## - density         1       408  90129 2054.8
    ## <none>                         89720 2055.2
    ## - docs_rate_1000  1       590  90310 2055.6
    ## - beds_rate_1000  1      1043  90764 2057.4
    ## - bagrad          1      1505  91225 2059.3
    ## - pop18           1      3735  93455 2068.2
    ## - totalinc        1      7018  96738 2080.9
    ## - pcincome        1      7957  97677 2084.5
    ## - poverty         1      8464  98184 2086.4
    ## - pop             1      9555  99275 2090.5
    ## - region          3     36161 125881 2174.1
    ## 
    ## Step:  AIC=2054.51
    ## crime_rate_1000 ~ pop + pop18 + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - density         1       336  90385 2053.9
    ## <none>                         90049 2054.5
    ## - docs_rate_1000  1       500  90549 2054.6
    ## - beds_rate_1000  1      1207  91255 2057.4
    ## - bagrad          1      1244  91292 2057.6
    ## - pop18           1      3512  93560 2066.6
    ## - totalinc        1      7116  97164 2080.6
    ## - pcincome        1      7638  97686 2082.6
    ## - poverty         1      8900  98948 2087.3
    ## - pop             1      9648  99697 2090.1
    ## - region          3     35889 125937 2172.3
    ## 
    ## Step:  AIC=2053.88
    ## crime_rate_1000 ~ pop + pop18 + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## <none>                         90385 2053.9
    ## - docs_rate_1000  1       791  91175 2055.1
    ## - beds_rate_1000  1      1114  91498 2056.4
    ## - bagrad          1      1662  92046 2058.6
    ## - pop18           1      4171  94556 2068.5
    ## - totalinc        1      7177  97562 2080.1
    ## - pcincome        1      8992  99377 2086.9
    ## - pop             1      9887 100271 2090.2
    ## - poverty         1      9966 100351 2090.5
    ## - region          3     35598 125982 2170.4

``` r
both = step(full.fit, direction = "both") %>% broom::tidy() %>% rename(stepwise = "term")
```

    ## Start:  AIC=2059.35
    ## crime_rate_1000 ~ area + pop + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + totalinc + region + docs_rate_1000 + 
    ##     beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - pop65           1        15  89297 2057.4
    ## - area            1        73  89354 2057.7
    ## - density         1       293  89574 2058.6
    ## - unemp           1       353  89635 2058.8
    ## - hsgrad          1       418  89699 2059.1
    ## <none>                         89281 2059.3
    ## - docs_rate_1000  1       581  89863 2059.7
    ## - beds_rate_1000  1      1116  90397 2061.9
    ## - bagrad          1      1171  90452 2062.2
    ## - pop18           1      3107  92388 2070.0
    ## - poverty         1      6562  95844 2083.5
    ## - totalinc        1      6938  96219 2085.0
    ## - pcincome        1      7248  96529 2086.2
    ## - pop             1      9382  98663 2094.2
    ## - region          3     34907 124189 2175.1
    ## 
    ## Step:  AIC=2057.41
    ## crime_rate_1000 ~ area + pop + pop18 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - area            1        67  89364 2055.7
    ## - density         1       304  89601 2056.7
    ## - unemp           1       383  89680 2057.0
    ## - hsgrad          1       408  89705 2057.1
    ## <none>                         89297 2057.4
    ## - docs_rate_1000  1       592  89889 2057.8
    ## + pop65           1        15  89281 2059.3
    ## - bagrad          1      1169  90466 2060.2
    ## - beds_rate_1000  1      1277  90574 2060.7
    ## - pop18           1      3748  93044 2070.6
    ## - poverty         1      6706  96002 2082.1
    ## - totalinc        1      6926  96222 2083.0
    ## - pcincome        1      7255  96552 2084.2
    ## - pop             1      9371  98668 2092.2
    ## - region          3     34901 124197 2173.2
    ## 
    ## Step:  AIC=2055.69
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + totalinc + region + docs_rate_1000 + beds_rate_1000 + 
    ##     density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - unemp           1       356  89720 2055.2
    ## - density         1       429  89793 2055.5
    ## - hsgrad          1       431  89795 2055.5
    ## <none>                         89364 2055.7
    ## - docs_rate_1000  1       588  89952 2056.1
    ## + area            1        67  89297 2057.4
    ## + pop65           1        10  89354 2057.7
    ## - bagrad          1      1171  90535 2058.5
    ## - beds_rate_1000  1      1280  90644 2058.9
    ## - pop18           1      3723  93087 2068.8
    ## - poverty         1      6662  96027 2080.2
    ## - totalinc        1      6868  96232 2081.0
    ## - pcincome        1      7226  96590 2082.4
    ## - pop             1      9406  98770 2090.6
    ## - region          3     34833 124198 2171.2
    ## 
    ## Step:  AIC=2055.16
    ## crime_rate_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - hsgrad          1       328  90049 2054.5
    ## - density         1       408  90129 2054.8
    ## <none>                         89720 2055.2
    ## - docs_rate_1000  1       590  90310 2055.6
    ## + unemp           1       356  89364 2055.7
    ## + area            1        40  89680 2057.0
    ## + pop65           1        37  89683 2057.0
    ## - beds_rate_1000  1      1043  90764 2057.4
    ## - bagrad          1      1505  91225 2059.3
    ## - pop18           1      3735  93455 2068.2
    ## - totalinc        1      7018  96738 2080.9
    ## - pcincome        1      7957  97677 2084.5
    ## - poverty         1      8464  98184 2086.4
    ## - pop             1      9555  99275 2090.5
    ## - region          3     36161 125881 2174.1
    ## 
    ## Step:  AIC=2054.51
    ## crime_rate_1000 ~ pop + pop18 + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000 + density
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## - density         1       336  90385 2053.9
    ## <none>                         90049 2054.5
    ## - docs_rate_1000  1       500  90549 2054.6
    ## + hsgrad          1       328  89720 2055.2
    ## + unemp           1       253  89795 2055.5
    ## + area            1        60  89989 2056.3
    ## + pop65           1        18  90030 2056.4
    ## - beds_rate_1000  1      1207  91255 2057.4
    ## - bagrad          1      1244  91292 2057.6
    ## - pop18           1      3512  93560 2066.6
    ## - totalinc        1      7116  97164 2080.6
    ## - pcincome        1      7638  97686 2082.6
    ## - poverty         1      8900  98948 2087.3
    ## - pop             1      9648  99697 2090.1
    ## - region          3     35889 125937 2172.3
    ## 
    ## Step:  AIC=2053.88
    ## crime_rate_1000 ~ pop + pop18 + bagrad + poverty + pcincome + 
    ##     totalinc + region + docs_rate_1000 + beds_rate_1000
    ## 
    ##                  Df Sum of Sq    RSS    AIC
    ## <none>                         90385 2053.9
    ## + density         1       336  90049 2054.5
    ## + hsgrad          1       256  90129 2054.8
    ## + unemp           1       247  90138 2054.9
    ## - docs_rate_1000  1       791  91175 2055.1
    ## + area            1       159  90225 2055.2
    ## + pop65           1        28  90356 2055.8
    ## - beds_rate_1000  1      1114  91498 2056.4
    ## - bagrad          1      1662  92046 2058.6
    ## - pop18           1      4171  94556 2068.5
    ## - totalinc        1      7177  97562 2080.1
    ## - pcincome        1      8992  99377 2086.9
    ## - pop             1      9887 100271 2090.2
    ## - poverty         1      9966 100351 2090.5
    ## - region          3     35598 125982 2170.4

``` r
bind_cols(backward[-1,1],both[-1,1]) %>% knitr::kable()
```

| backward       | stepwise       |
|:---------------|:---------------|
| pop            | pop            |
| pop18          | pop18          |
| bagrad         | bagrad         |
| poverty        | poverty        |
| pcincome       | pcincome       |
| totalinc       | totalinc       |
| region2        | region2        |
| region3        | region3        |
| region4        | region4        |
| docs_rate_1000 | docs_rate_1000 |
| beds_rate_1000 | beds_rate_1000 |

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
    ## 1  ( 1 )  " "  " " " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 2  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 3  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 4  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 5  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 6  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   " "      " "     
    ## 7  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 8  ( 1 )  " "  "*" " "   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 9  ( 1 )  " "  "*" "*"   " "   " "    " "    "*"     " "   "*"      "*"     
    ## 10  ( 1 ) " "  "*" "*"   " "   " "    "*"    "*"     " "   "*"      "*"     
    ## 11  ( 1 ) " "  "*" "*"   " "   " "    "*"    "*"     " "   "*"      "*"     
    ## 12  ( 1 ) " "  "*" "*"   " "   " "    "*"    "*"     " "   "*"      "*"     
    ## 13  ( 1 ) " "  "*" "*"   " "   "*"    "*"    "*"     " "   "*"      "*"     
    ## 14  ( 1 ) " "  "*" "*"   " "   "*"    "*"    "*"     "*"   "*"      "*"     
    ##           region2 region3 region4 docs_rate_1000 beds_rate_1000 density
    ## 1  ( 1 )  " "     " "     " "     " "            " "            " "    
    ## 2  ( 1 )  " "     " "     " "     " "            " "            " "    
    ## 3  ( 1 )  " "     "*"     " "     " "            " "            " "    
    ## 4  ( 1 )  " "     "*"     " "     "*"            " "            " "    
    ## 5  ( 1 )  " "     "*"     "*"     "*"            " "            " "    
    ## 6  ( 1 )  "*"     "*"     "*"     "*"            " "            " "    
    ## 7  ( 1 )  "*"     "*"     "*"     " "            " "            " "    
    ## 8  ( 1 )  "*"     "*"     "*"     "*"            " "            " "    
    ## 9  ( 1 )  "*"     "*"     "*"     " "            "*"            " "    
    ## 10  ( 1 ) "*"     "*"     "*"     " "            "*"            " "    
    ## 11  ( 1 ) "*"     "*"     "*"     "*"            "*"            " "    
    ## 12  ( 1 ) "*"     "*"     "*"     "*"            "*"            "*"    
    ## 13  ( 1 ) "*"     "*"     "*"     "*"            "*"            "*"    
    ## 14  ( 1 ) "*"     "*"     "*"     "*"            "*"            "*"

``` r
# plot of Cp and Adj-R2 as functions of parameters
par(mfrow=c(1,2))
plot(2:15, sumsb$cp, xlab="No. of parameters", ylab="Cp Statistic") 
abline(0,1)

plot(2:15, sumsb$adjr2, xlab="No of parameters", ylab="Adj R2")
```

![](main_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
