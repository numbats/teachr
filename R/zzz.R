.onLoad <- function(...) {
  knitr::knit_engines$set(teachr = teachr_engine)
  invisible()
}
