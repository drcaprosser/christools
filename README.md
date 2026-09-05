<!-- README.md is generated from README.Rmd. Please edit that file -->

# christools

<!-- badges: start -->
<!-- badges: end -->

## Installation

I made this for myself, but if anyone else wants to use it, you can install christools like so:

``` r
# install.packages("remotes")

remotes::install_github("drcaprosser/christools")
```

## Functions

### `inv_logit()`

Transforms log-odds into probabilities.

``` r
inv_logit(c(-2, -1, 0, 1, 2))
```

### `collapse_with_and()`

Formats values as a grammatical list, with optional connectors and serial commas.

``` r
collapse_with_and(c("apples", "bananas", "pears"))
# [1] "apples, bananas, and pears"

collapse_with_and(
  c("apples", "bananas", "pears"),
  connector = "or",
  serial_comma = FALSE
)
# [1] "apples, bananas or pears"
```

### `get_decade()`

Returns the first year of the decade for each date.

``` r
get_decade(as.Date(c("1974-06-01", "1989-01-01", "2026-09-05")))
# [1] 1970 1980 2020
```

### `mix_hex()`

Mixes two hexadecimal colours at a specified proportion.

``` r
mix_hex("#E69F00", "#56B4E9")
mix_hex("#E69F00", "#56B4E9", alpha = 0.25)
```

### `pop_sd()`

Calculates the population standard deviation.

``` r
pop_sd(c(1, 2, 3, 4))
```

### `approx_text()`

Expresses non-negative numbers as short approximate phrases.

``` r
approx_text(c(0, 0.25, 3.2, 1250000))
```

### `number_to_word()`

Converts whole numbers from zero to ten into words.

``` r
number_to_word(c(0, 5, 10, 11))
```

### `xtile()`

Divides values into quantile intervals, with an option for labelled boundaries.

``` r
xtile(1:100, n = 4, labels = TRUE)
```

### `cut_breaks_labelled()`

Cuts values into explicitly defined, labelled intervals.

``` r
cut_breaks_labelled(
  c(3, 17, 42),
  breaks = c(0, 10, 20, 50),
  last_plus = TRUE
)
```
