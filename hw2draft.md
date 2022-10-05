hw2draft
================

### Problem 0

This solution focuses on a reproducible report containing code and text
necessary for Problems 1-3, and is organized as an R Project. This was
not prepared as a GitHub repo; examples for repository structure and git
commits should be familiar from other elements of the course.

Throughout, we use appropriate text to describe our code and results,
and use clear styling to ensure code is readable.

``` r
library(tidyverse)
library(readxl)
```

### Problem 1

Below we import and clean data from
`NYC_Transit_Subway_Entrance_And_Exit_Data.csv`. The process begins with
data import, updates variable names, and selects the columns that will
be used in later parts fo this problem. We update `entry` from `yes` /
`no` to a logical variable. As part of data import, we specify that
`Route` columns 8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>% 
  janitor::clean_names() %>% 
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))
```

As it stands, these data are not “tidy”: route number should be a
variable, as should route. That is, to obtain a tidy dataset we would
need to convert `route` variables from wide to long format. This will be
useful when focusing on specific routes, but may not be necessary when
considering questions that focus on station-level variables. 7 The
following code chunk selects station name and line, and then uses
`distinct()` to obtain all unique combinations. As a result, the number
of rows in this dataset is the number of unique stations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 465 × 2
##    station_name             line    
##    <chr>                    <chr>   
##  1 25th St                  4 Avenue
##  2 36th St                  4 Avenue
##  3 45th St                  4 Avenue
##  4 53rd St                  4 Avenue
##  5 59th St                  4 Avenue
##  6 77th St                  4 Avenue
##  7 86th St                  4 Avenue
##  8 95th St                  4 Avenue
##  9 9th St                   4 Avenue
## 10 Atlantic Av-Barclays Ctr 4 Avenue
## # … with 455 more rows
```

-   There are 465 distinct stations.

The next code chunk is similar, but filters according to ADA compliance
as an initial step. This produces a dataframe in which the number of
rows is the number of ADA compliant stations.

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 84 × 2
##    station_name                   line           
##    <chr>                          <chr>          
##  1 Atlantic Av-Barclays Ctr       4 Avenue       
##  2 DeKalb Av                      4 Avenue       
##  3 Pacific St                     4 Avenue       
##  4 Grand Central                  42nd St Shuttle
##  5 34th St                        6 Avenue       
##  6 47-50th Sts Rockefeller Center 6 Avenue       
##  7 Church Av                      6 Avenue       
##  8 21st St                        63rd Street    
##  9 Lexington Av                   63rd Street    
## 10 Roosevelt Island               63rd Street    
## # … with 74 more rows
```

-   There are 84 distinct stations, that are ADA compliant.

To compute the proportion of station entrances / exits without vending
allow entrance, we first exclude station entrances that do not allow
vending. Then, we focus on the `entry` variable – this logical, so
taking the mean will produce the desired proportion (recall that R will
coerce logical to numeric in cases like this).

``` r
trans_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
## [1] 0.3770492
```

-   the proportion of station entrances / exits without vending allow
    entrance is 0.38.

Lastly, we write a code chunk to identify stations that serve the A
train, and to assess how many of these are ADA compliant. As a first
step, we tidy the data as alluded to previously; that is, we convert
`route` from wide to long format. After this step, we can use tools from
previous parts of the question (filtering to focus on the A train, and
on ADA compliance; selecting and using `distinct` to obtain dataframes
with the required stations in rows).

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 60 × 2
##    station_name                  line           
##    <chr>                         <chr>          
##  1 Times Square                  42nd St Shuttle
##  2 125th St                      8 Avenue       
##  3 145th St                      8 Avenue       
##  4 14th St                       8 Avenue       
##  5 168th St - Washington Heights 8 Avenue       
##  6 175th St                      8 Avenue       
##  7 181st St                      8 Avenue       
##  8 190th St                      8 Avenue       
##  9 34th St                       8 Avenue       
## 10 42nd St                       8 Avenue       
## # … with 50 more rows

trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 17 × 2
##    station_name                  line            
##    <chr>                         <chr>           
##  1 14th St                       8 Avenue        
##  2 168th St - Washington Heights 8 Avenue        
##  3 175th St                      8 Avenue        
##  4 34th St                       8 Avenue        
##  5 42nd St                       8 Avenue        
##  6 59th St                       8 Avenue        
##  7 Inwood - 207th St             8 Avenue        
##  8 West 4th St                   8 Avenue        
##  9 World Trade Center            8 Avenue        
## 10 Times Square-42nd St          Broadway        
## 11 59th St-Columbus Circle       Broadway-7th Ave
## 12 Times Square                  Broadway-7th Ave
## 13 8th Av                        Canarsie        
## 14 Franklin Av                   Franklin        
## 15 Euclid Av                     Fulton          
## 16 Franklin Av                   Fulton          
## 17 Howard Beach                  Rockaway
```

-   There are 60 distinct stations, that serve the A train.
-   There are 17 distinct stations, that serve the A train and are ADA
    compliant.

### Problem 2

``` r
trash_W1 = 
  readxl::read_excel("data/Trash Wheel Collection Data.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N549") %>%
  janitor::clean_names() %>%
mutate (
        sports_balls = as.integer(sports_balls),
        wheel = "mrtrash")
```

Now, I will repeat the same for Professor data

``` r
prof_W2 = 
  read_excel("data/Trash Wheel Collection Data.xlsx", sheet = "Professor Trash Wheel", range = "A2:M96") %>%
  janitor::clean_names() %>%
mutate (year = as.character(year),
       wheel = "proftrash" )
```

Combining the both data sets:

``` r
merged_trash = bind_rows (trash_W1, prof_W2) %>%
  janitor::clean_names()
```

-   Description of the data set post merging:

|                                                  |              |
|:-------------------------------------------------|:-------------|
| Name                                             | merged_trash |
| Number of rows                                   | 641          |
| Number of columns                                | 15           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |              |
| Column type frequency:                           |              |
| character                                        | 3            |
| numeric                                          | 11           |
| POSIXct                                          | 1            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |              |
| Group variables                                  | None         |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| month         |         0 |             1 |   3 |   9 |     0 |       13 |          0 |
| year          |         0 |             1 |   4 |   4 |     0 |        9 |          0 |
| wheel         |         0 |             1 |   7 |   9 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable      | n_missing | complete_rate |     mean |       sd |     p0 |     p25 |     p50 |      p75 |      p100 | hist  |
|:-------------------|----------:|--------------:|---------:|---------:|-------:|--------:|--------:|---------:|----------:|:------|
| dumpster           |         0 |          1.00 |   240.78 |   166.88 |   1.00 |   81.00 |  227.00 |   387.00 |    547.00 | ▇▅▅▅▅ |
| weight_tons        |         0 |          1.00 |     3.02 |     0.84 |   0.61 |    2.48 |    3.08 |     3.62 |      5.62 | ▁▅▇▅▁ |
| volume_cubic_yards |         0 |          1.00 |    15.22 |     1.44 |   6.00 |   15.00 |   15.00 |    15.00 |     20.00 | ▁▁▁▇▁ |
| plastic_bottles    |         0 |          1.00 |  2464.81 |  1817.94 | 210.00 | 1110.00 | 2110.00 |  3100.00 |   9830.00 | ▇▆▁▁▁ |
| polystyrene        |         0 |          1.00 |  2088.81 |  1990.25 |  48.00 |  780.00 | 1460.00 |  2870.00 |  11528.00 | ▇▃▁▁▁ |
| cigarette_butts    |         0 |          1.00 | 19663.80 | 28187.00 | 900.00 | 4400.00 | 8000.00 | 23000.00 | 310000.00 | ▇▁▁▁▁ |
| glass_bottles      |         0 |          1.00 |    20.71 |    15.82 |   0.00 |    9.00 |   18.00 |    28.00 |    110.00 | ▇▃▁▁▁ |
| grocery_bags       |         0 |          1.00 |  1217.66 |  1634.36 |  24.00 |  360.00 |  780.00 |  1480.00 |  13450.00 | ▇▁▁▁▁ |
| chip_bags          |         0 |          1.00 |  2405.54 |  3050.01 | 180.00 |  800.00 | 1340.00 |  2684.00 |  20100.00 | ▇▁▁▁▁ |
| sports_balls       |        94 |          0.85 |    12.56 |     9.28 |   0.00 |    6.00 |   11.00 |    18.00 |     56.00 | ▇▅▂▁▁ |
| homes_powered      |        73 |          0.89 |    44.11 |    20.73 |   0.00 |   34.67 |   49.00 |    57.50 |     93.67 | ▂▃▇▅▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 1900-01-20 | 2022-07-29 | 2018-08-09 |      359 |

-   The total number of rows are 641 and the total number of variables
    are 15
-   There are 3 character variables, 11 numeric variables and 1 date
    variable
-   The main variables in the dataset are weight of the trash (in tons),
    volume of the trash (in cubic yards) and the amount of various types
    of trash collected like plastic bottles, chips bags, cigarette
    butts, glass bottles, sports balls, polystyrene), the homes powered,
    and the distribution by names (Mr and Prof Trash).
-   The total weight of trash collected combined is 1938.48 tons
-   The total weight of trash collected by Professor Trash is 190.12
    tons
-   The total number of sports balls collected by Mr Trash is 856

### Problem 3

``` r
pols_ds = read_csv(
    "data/fivethirtyeight_datasets/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(col=mon, into = c("year", "month", "day"), sep ='-', convert = TRUE) %>%
  mutate(month = month.abb[month],
         president = case_when (prez_gop == 1 ~ "gop", prez_dem == 1 ~ "dem")) %>%
  select(-prez_gop, -prez_dem, -day)
```

``` r
snp_ds = read_csv(
    "data/fivethirtyeight_datasets/snp.csv") %>%
  janitor::clean_names() %>%
  separate(col= date, into = c("month", "day", "year"), sep ='/', convert = TRUE) %>%
  mutate(month = month.abb[month],
         year = ifelse(year > 49, year + 1900, year + 2000)) %>%
select(-day) %>%
  select (year, month, everything())
```

``` r
unemploy_ds = read_csv(
    "data/fivethirtyeight_datasets/unemployment.csv") %>%
  janitor::clean_names() %>%
  pivot_longer (jan:dec,
names_to = "month",
values_to = "unemployment") %>%
  mutate (month = str_to_title(month))
  
```

``` r
pomo_snp = left_join(pols_ds, snp_ds)
```

``` r
pomosnp_unemp = left_join(pomo_snp,unemploy_ds)
```

-   Summary of the combined data set

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | pomosnp_unemp |
| Number of rows                                   | 822           |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 2             |
| numeric                                          | 9             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| month         |         0 |          1.00 |   3 |   3 |     0 |       12 |          0 |
| president     |         5 |          0.99 |   3 |   3 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |     sd |      p0 |     p25 |     p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|--------:|-------:|--------:|--------:|--------:|--------:|--------:|:------|
| year          |         0 |          1.00 | 1980.75 |  19.79 | 1947.00 | 1964.00 | 1981.00 | 1998.00 | 2015.00 | ▇▇▇▇▇ |
| gov_gop       |         0 |          1.00 |   22.48 |   5.68 |   12.00 |   18.00 |   22.00 |   28.00 |   34.00 | ▆▆▇▅▅ |
| sen_gop       |         0 |          1.00 |   46.10 |   6.38 |   32.00 |   42.00 |   46.00 |   51.00 |   56.00 | ▃▃▇▇▇ |
| rep_gop       |         0 |          1.00 |  194.92 |  29.24 |  141.00 |  176.00 |  195.00 |  222.00 |  253.00 | ▃▇▆▃▅ |
| gov_dem       |         0 |          1.00 |   27.20 |   5.94 |   17.00 |   22.00 |   28.00 |   32.00 |   41.00 | ▆▅▇▆▂ |
| sen_dem       |         0 |          1.00 |   54.41 |   7.37 |   44.00 |   48.00 |   53.00 |   58.00 |   71.00 | ▇▆▇▃▂ |
| rep_dem       |         0 |          1.00 |  244.97 |  31.37 |  188.00 |  211.00 |  250.00 |  268.00 |  301.00 | ▇▂▇▇▅ |
| close         |        36 |          0.96 |  472.85 | 543.29 |   17.05 |   83.67 |  137.26 |  932.06 | 2107.39 | ▇▁▂▁▁ |
| unemployment  |        12 |          0.99 |    5.83 |   1.65 |    2.50 |    4.70 |    5.60 |    6.90 |   10.80 | ▃▇▅▂▁ |

-   the merged data set has 822 observations and 11 variables.
