# teachr
An R package to aid teaching of introduction to R


## Literature Review

* Look at existing R packages for teaching statistics and summarise what is done, e.g. [CRAN Task view for teaching statistics](https://cran.r-project.org/web/views/TeachingStatistics.html)
* Gather some introduction to R materials: 
  * https://resources.numbat.space/
  * https://rladiessydney.org/courses/ryouwithme/
* Look at:
  * `learnr`
  * `gradethis`
  * `swirl`
  * R exams http://www.r-exams.org/


## Topics

1. Installing R + RStudio IDE + tour of RStudio IDE:
  * PDF Preview -> System viewer
  * Save workspace to .RData on exit: Never
  * Untick "Restore .RData into workspace at startup"
3. R Programming Basics (e.g. assignment, basic arithmetics, vectors, list, ...)
4. R folder and file structure (Setting up R project, tips for best ways to organise analytical projects, etc)
5. Reading data into R (base + `readr`)
6. Basics of data cleaning with R using `dplyr` + `tidyr` 
7. Basics of plotting in R with `ggplot2`
8. Functional mapping in R with `purrr`
9. How to ask for help (`reprex`)
10. Basic linear regression with `lm()` and summary statistics with `summary()`
11. Basic R Markdown


## Goals 

* Build an infrastructure to easily write multiple choice quizzes for topics listed above
* Write a number of multiple choice quizzes 
* Curate some local and fresh data suitable for teaching
