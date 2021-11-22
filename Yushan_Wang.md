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
