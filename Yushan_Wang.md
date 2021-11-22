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

``` r
cdi_data = read.csv("./data/cdi.csv") %>%
  janitor::clean_names() %>%
  mutate(crime_rate = crimes/pop) 
```

poverty vs crime rate

``` r
reg <- lm(crimes ~ poverty, data = cdi_data)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = crimes ~ poverty, data = cdi_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -65035 -20362 -12640   1354 655904 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9181.3     5825.7   1.576 0.115750    
    ## poverty       2056.1      589.4   3.488 0.000535 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57510 on 438 degrees of freedom
    ## Multiple R-squared:  0.02703,    Adjusted R-squared:  0.02481 
    ## F-statistic: 12.17 on 1 and 438 DF,  p-value: 0.0005354

``` r
cdi_data %>% ggplot() + 
  geom_point(aes(x = poverty, y = crime_rate, alpha = 0.5))
```

![](Yushan_Wang_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
