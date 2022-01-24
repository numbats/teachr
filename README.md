
<!-- README.md is generated from README.Rmd. Please edit that file -->

# teachr

<!-- badges: start -->
<!-- badges: end -->

An R package to aid teaching of introduction to R

<!-- ## Installation -->

You can install the development version of teachr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/teachr")
```

## Example

```` markdown
```{teachr substitute}
z <- mean(<<1:5>>)
z

---
  
Hint: Add a vector inside mean()

???

expect_identical(z, 3)
```
````
