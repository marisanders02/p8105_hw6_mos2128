Homework 6
================
Mari Sanders
2024-12-01

# Problem 1

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## using cached file: C:\Users\marsi\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:18:58.392981 (8.668)

    ## file min/max dates: 1869-01-01 / 2024-09-30

# Problem 2

Data Cleaning

``` r
homicide_df <- read_csv("data/homicide-data.csv", na = c("", "NA", "Unknown")) %>% filter(victim_race != "NA") %>% 
  janitor::clean_names() %>% 
  unite(city_state, c(city, state), sep = ", ") %>% 
  filter(city_state != "Dallas, TX", 
         city_state != "Phoenix, AZ", 
         city_state != "Kansas City, MO", 
         city_state != "Tulsa, AL",
         victim_race %in% c("White", "Black"),
         victim_age != "Unknown"
  ) %>%
  mutate(victim_age = as.numeric(victim_age), 
           resolved = as.numeric(disposition == "Closed by arrest"), 
        victim_race = fct_relevel(victim_race, "White")) %>% 
  select(city_state, resolved, victim_age, victim_race, victim_sex) 
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Baltimore

``` r
logistic <- 
  homicide_df %>% 
  filter(city_state == "Baltimore, MD") %>% 
  glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = binomial())

broom::tidy(logistic, conf.int = TRUE) %>% mutate(OR = exp(estimate), 
                                                  conf_low = exp(conf.low), 
                                                  conf_high = exp(conf.high)) %>% 
  select(term, log_OR = estimate, OR, conf.low, conf.high) %>% filter(term == "victim_sexMale") %>% 
  knitr::kable(digits = 3)
```

| term           | log_OR |    OR | conf.low | conf.high |
|:---------------|-------:|------:|---------:|----------:|
| victim_sexMale | -0.854 | 0.426 |   -1.126 |    -0.584 |

In Baltimore, homicides of Males were 0.426 times more likely to be
unsolved compared to homicides of white victims after adjusting for all
other variables.

``` r
city_results <- homicide_df %>%
  group_by(city_state) %>%
  nest() %>%
  mutate(
    model = map(data, ~glm(resolved ~ victim_age + victim_sex + victim_race, data = ., family = "binomial")),
    results = map(model, broom::tidy, conf.int = TRUE)) %>%
  unnest(results) %>%
  mutate(OR = exp(estimate), 
         CI_lower = exp(estimate - std.error * qnorm(0.975)),
         CI_upper = exp(estimate + std.error * qnorm(0.975))) %>% 
  select(city_state, term, OR, CI_lower, CI_upper) %>%
   filter(term == "victim_sexMale") 

city_results %>% knitr::kable(digits = 3)
```

| city_state         | term           |    OR | CI_lower | CI_upper |
|:-------------------|:---------------|------:|---------:|---------:|
| Albuquerque, NM    | victim_sexMale | 1.767 |    0.831 |    3.761 |
| Atlanta, GA        | victim_sexMale | 1.000 |    0.684 |    1.463 |
| Baltimore, MD      | victim_sexMale | 0.426 |    0.325 |    0.558 |
| Baton Rouge, LA    | victim_sexMale | 0.381 |    0.209 |    0.695 |
| Birmingham, AL     | victim_sexMale | 0.870 |    0.574 |    1.318 |
| Boston, MA         | victim_sexMale | 0.667 |    0.354 |    1.260 |
| Buffalo, NY        | victim_sexMale | 0.521 |    0.290 |    0.935 |
| Charlotte, NC      | victim_sexMale | 0.884 |    0.557 |    1.403 |
| Chicago, IL        | victim_sexMale | 0.410 |    0.336 |    0.501 |
| Cincinnati, OH     | victim_sexMale | 0.400 |    0.236 |    0.677 |
| Columbus, OH       | victim_sexMale | 0.532 |    0.378 |    0.750 |
| Denver, CO         | victim_sexMale | 0.479 |    0.236 |    0.971 |
| Detroit, MI        | victim_sexMale | 0.582 |    0.462 |    0.734 |
| Durham, NC         | victim_sexMale | 0.812 |    0.392 |    1.683 |
| Fort Worth, TX     | victim_sexMale | 0.669 |    0.397 |    1.127 |
| Fresno, CA         | victim_sexMale | 1.335 |    0.581 |    3.071 |
| Houston, TX        | victim_sexMale | 0.711 |    0.558 |    0.907 |
| Indianapolis, IN   | victim_sexMale | 0.919 |    0.679 |    1.242 |
| Jacksonville, FL   | victim_sexMale | 0.720 |    0.537 |    0.966 |
| Las Vegas, NV      | victim_sexMale | 0.837 |    0.608 |    1.154 |
| Long Beach, CA     | victim_sexMale | 0.410 |    0.156 |    1.082 |
| Los Angeles, CA    | victim_sexMale | 0.662 |    0.458 |    0.956 |
| Louisville, KY     | victim_sexMale | 0.491 |    0.305 |    0.790 |
| Memphis, TN        | victim_sexMale | 0.723 |    0.529 |    0.988 |
| Miami, FL          | victim_sexMale | 0.515 |    0.304 |    0.872 |
| Milwaukee, wI      | victim_sexMale | 0.727 |    0.499 |    1.060 |
| Minneapolis, MN    | victim_sexMale | 0.947 |    0.478 |    1.875 |
| Nashville, TN      | victim_sexMale | 1.034 |    0.685 |    1.562 |
| New Orleans, LA    | victim_sexMale | 0.585 |    0.422 |    0.811 |
| New York, NY       | victim_sexMale | 0.262 |    0.138 |    0.499 |
| Oakland, CA        | victim_sexMale | 0.563 |    0.365 |    0.868 |
| Oklahoma City, OK  | victim_sexMale | 0.974 |    0.624 |    1.520 |
| Omaha, NE          | victim_sexMale | 0.382 |    0.203 |    0.721 |
| Philadelphia, PA   | victim_sexMale | 0.496 |    0.378 |    0.652 |
| Pittsburgh, PA     | victim_sexMale | 0.431 |    0.265 |    0.700 |
| Richmond, VA       | victim_sexMale | 1.006 |    0.498 |    2.033 |
| San Antonio, TX    | victim_sexMale | 0.705 |    0.398 |    1.249 |
| Sacramento, CA     | victim_sexMale | 0.669 |    0.335 |    1.336 |
| Savannah, GA       | victim_sexMale | 0.867 |    0.422 |    1.780 |
| San Bernardino, CA | victim_sexMale | 0.500 |    0.171 |    1.462 |
| San Diego, CA      | victim_sexMale | 0.413 |    0.200 |    0.855 |
| San Francisco, CA  | victim_sexMale | 0.608 |    0.317 |    1.165 |
| St. Louis, MO      | victim_sexMale | 0.703 |    0.530 |    0.932 |
| Stockton, CA       | victim_sexMale | 1.352 |    0.621 |    2.942 |
| Tampa, FL          | victim_sexMale | 0.808 |    0.348 |    1.876 |
| Tulsa, OK          | victim_sexMale | 0.976 |    0.614 |    1.552 |
| Washington, DC     | victim_sexMale | 0.691 |    0.469 |    1.018 |

``` r
city_results %>% 
  ggplot(aes(x = fct_reorder(city_state, OR), y = OR)) + 
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) +
  labs(
    title = "Unsolved homicides, Male vs. Female Victims",
    x = "City", 
    y = "Adjusted Odds Ratio"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![](Homework-6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Problem 3

``` r
birthweight <- read_csv("data/birthweight.csv") %>% 
  mutate(babysex = as.factor(babysex), 
         frace = as.factor(frace), 
         mrace = as.factor(mrace), 
         malform = as.factor(malform))
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
birthweight %>% 
  is.na() %>% 
  summary()
```

    ##   babysex          bhead          blength           bwt         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    delwt          fincome          frace          gaweeks       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##   malform         menarche        mheight          momage       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    mrace           parity         pnumlbw         pnumsga       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    ppbmi            ppwt           smoken          wtgain       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342

There are no missing values in this dataset.

``` r
birthweight_model <- 
  lm(bwt ~ babysex + bhead + blength + smoken + ppbmi + gaweeks + momage + delwt, data = birthweight)
summary(birthweight_model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + smoken + ppbmi + 
    ##     gaweeks + momage + delwt, data = birthweight)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1087.35  -183.44    -6.37   174.60  2481.40 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6154.9668    99.7784 -61.686  < 2e-16 ***
    ## babysex2       30.9564     8.6548   3.577 0.000352 ***
    ## bhead         135.7629     3.5072  38.710  < 2e-16 ***
    ## blength        77.4074     2.0605  37.567  < 2e-16 ***
    ## smoken         -2.6295     0.5792  -4.540 5.78e-06 ***
    ## ppbmi         -14.8462     1.9470  -7.625 2.98e-14 ***
    ## gaweeks        13.0137     1.4889   8.740  < 2e-16 ***
    ## momage          6.2195     1.1097   5.604 2.22e-08 ***
    ## delwt           3.6169     0.2869  12.605  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 279.3 on 4333 degrees of freedom
    ## Multiple R-squared:  0.7031, Adjusted R-squared:  0.7026 
    ## F-statistic:  1283 on 8 and 4333 DF,  p-value: < 2.2e-16

``` r
birthweight %>%
  add_predictions(birthweight_model) %>%
  add_residuals(birthweight_model) %>% 
  ggplot(aes(x = pred, y = resid)) + 
  geom_point() 
```

![](Homework-6_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
main_effects <- 
  lm(bwt ~ blength + gaweeks, data = birthweight)

interactions <- 
  lm(bwt ~bhead + blength + babysex + bhead*blength*babysex, data = birthweight)
```

``` r
cv_df <- 
  crossv_mc(birthweight, 100) %>% 
  mutate(
    train = map(train, as_tibble), 
    test = map(test, as_tibble)
  ) %>% 
  mutate(my_model = map(train, \(df) lm(bwt ~ babysex + bhead + blength + smoken + ppbmi + gaweeks + momage + delwt, data = df)),
    main_effects = map(train, \(df) lm(bwt ~ blength, gaweeks, data = df)), 
         interactions = map(train, \(df) lm(bwt ~bhead + blength + babysex + bhead*blength*babysex, data = df))) %>% 
  mutate(
    rmse_mymodel = map2_dbl(my_model, test, \(mod, df) rmse(model = mod, data = df)), 
    rmse_maineffects = map2_dbl(main_effects, test, \(mod, df) rmse(model = mod, data = df)), 
    rmse_interactions = map2_dbl(interactions, test, \(mod, df) rmse(model = mod, data = df))
  )

cv_df %>% 
  select(starts_with("rmse")) %>% 
  pivot_longer(everything(), 
               names_to = "model", 
               values_to = "rmse", 
               names_prefix = "rmse_") %>% 
  mutate(model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) +
  geom_violin()
```

![](Homework-6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
