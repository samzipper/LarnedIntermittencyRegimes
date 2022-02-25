Messing around with Empirical Dynamic Modeling (EDM)
================

# EDM package and documentation

[rEDM package github](https://github.com/SugiharaLab/rEDM)

# Load packages

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(rEDM)
options(dplyr.summarise.inform=F)   # suppress summarize info
```

# Load streamflow data, summarize to weekly and monthly no-flow days

``` r
# daily discharge
df_day <- 
  read_csv(file.path("..", "data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Week = week(Date))
```

    ## Rows: 8401 Columns: 6
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): stage_cd, discharge_cd
    ## dbl  (3): WaterYear, stage_masl, discharge_cms
    ## date (1): Date
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ggplot(df_day, aes(x = Date, y = discharge_cms)) +
  geom_line()
```

![](EDM_Sandbox_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# weekly no-flow
df_week <- 
  df_day %>% 
  group_by(Year, Week) %>% 
  summarize(Date_Start = min(Date),
            noflow_n = sum(discharge_cms == 0),
            noflow_prc = noflow_n/n())

ggplot(df_week, aes(x = Date_Start, y = noflow_prc)) +
  geom_line()
```

![](EDM_Sandbox_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# monthly no-flow
df_month <- 
  df_day %>% 
  group_by(Year, Month) %>% 
  summarize(Date_Start = min(Date),
            noflow_n = sum(discharge_cms == 0),
            noflow_prc = noflow_n/n())

ggplot(df_month, aes(x = Date_Start, y = noflow_prc)) +
  geom_line()
```

![](EDM_Sandbox_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

# Apply EDM to daily discharge data

``` r
# directly following github example
df_day_forEDM <- df_day %>% 
  dplyr::select(Date, discharge_cms)
  
E.opt = EmbedDimension( dataFrame = df_day_forEDM,    # input data
                        lib     = "1 8401", # portion of data to train
                        pred    = "1 8401", # portion of data to predict
                        columns = "discharge_cms",
                        target  = "discharge_cms" )
```

![](EDM_Sandbox_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
E.opt
```

    ##     E       rho
    ## 1   1 0.8895323
    ## 2   2 0.9453611
    ## 3   3 0.9439231
    ## 4   4 0.9454378
    ## 5   5 0.9430793
    ## 6   6 0.9402448
    ## 7   7 0.9383177
    ## 8   8 0.9382510
    ## 9   9 0.9373923
    ## 10 10 0.9375371

``` r
simplex = Simplex( dataFrame = df_day_forEDM, 
                   lib     = "1   5600", # portion of data to train
                   pred    = "5601 8401", # portion of data to predict
                   columns = "discharge_cms",
                   target  = "discharge_cms",
                   E       = 2 )

simplex %>% 
  pivot_longer(c("Observations", "Predictions")) %>% 
  ggplot(aes(x = Date, y = value, color = name)) +
  geom_line()
```

    ## Warning: Removed 2 row(s) containing missing values (geom_path).

![](EDM_Sandbox_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# fit stat
hydroGOF::KGE(simplex$Predictions, simplex$Observations)
```

    ## [1] 0.6425848
