teachr_engine <- function(options) {
  label <- knitr::opts_current$get()$label
  code <- options$code
  outdir <- options$outdir %||% "../exercises"

  testthat_sep <- cumsum(grepl("^\\?\\?\\?\\s*", code))
  if(testthat_sep[length(testthat_sep)] > 1) {
    stop("Only one testthat section can be provided in each teachr chunk.")
  }
  code <- split(code, c("code", "test")[testthat_sep+1])
  hint_sep <- cumsum(grepl("^---\\s*", code$code))
  if(hint_sep[length(hint_sep)] > 1) {
    stop("Only one hint can be provided in each teachr chunk.")
  }
  code[c("code", "hint")] <- split(code$code, factor(c("code", "hint"))[hint_sep+1])
  if(length(code$hint) > 0) code$hint <- code$hint[-1]

  # Add default starter code substitutes
  code$code <- gsub(
    paste0(options[[".open"]]%||%"\\{<","([^🎯]+)",options[[".close"]]%||%">\\}"),
    paste0(options[[".open"]]%||%"\\{<","\\1🎯___",options[[".close"]]%||%">\\}"),
    code$code
  )

  # Separate solutions from starter
  solution_patterns <- list(
    inline = paste0(options[[".open"]]%||%"\\{<","([^🎯]+)🎯?(.*?)",options[[".close"]]%||%">\\}")
  )
  starter <- gsub(solution_patterns$inline, "\\2", code$code)
  solution <- gsub(solution_patterns$inline, "\\1", code$code)

  # If ran outside of a knit, run the solution code.
  if(is.null(getOption('knitr.in.progress'))) {
    # Don't look! To fix this properly (safely), start up a new R session and load testthat to run code in.
    library(testthat)

    options$code <- solution
    options$cache <- FALSE
    # Run R code as usual
    out <- knitr::knit_engines$get("R")(options)

    code$test <- c(
      "suppressPackageStartupMessages(library(testthat))",
      paste0("test_that(\"", label, "\", {"),
      code$test[-1],
      "})"
    )

    # Run tests on R code
    res <- capture.output(eval(str2expression(code$test)))

    # Return combined result
    return(paste0(c(out, "", res), collapse = "\n"))
  }

  # Otherwise, create exercise files for the course website
  xfun::dir_create(outdir)
  xfun::write_utf8(starter, file.path(outdir, paste0("exc_", label, ".R")))
  xfun::write_utf8(solution, file.path(outdir, paste0("solution_", label, ".R")))
  xfun::write_utf8(code$test[-1], file.path(outdir, paste0("test_", label, ".R")))
  knitr::asis_output(
    paste0(
      '<codeblock id="', label, '">\n',
      paste0(code$hint, collapse = "\n"),
      '\n</codeblock>'
    )
  )
}
