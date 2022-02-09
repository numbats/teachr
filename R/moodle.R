
#' R Markdown format for Moodle XML cloze quizzes
#'
#' Provides an alternative interface to working with the exams package for
#' producing Moodle questions of the cloze type.
#'
#' @inheritParams rmarkdown::html_document
#'
#' @import rmarkdown
#'
#' @export
teachr_cloze <- function(self_contained = TRUE,
                         extra_dependencies = NULL,
                         theme = NULL,
                         includes = NULL,
                         lib_dir = NULL,
                         md_extensions = NULL,
                         pandoc_args = NULL,
                         ...) {
  pre_knit <- function(input, ...) {
    # Parse yaml
    front_matter <- rmarkdown::yaml_front_matter(input)

    # Get cloze types
    ## Replace cloze inline chunks with `r cloze()` functions
    cloze_pattern <- "(?<!(^|\n)``)`cloze[ #]([^`]+)\\s*`"
    cloze_rmd <- stringr::str_replace_all(
      readLines(input),
      cloze_pattern,
      replacement = "`r teachr::cloze(\\2)`"
    )
    cloze_sol <- stringr::str_match_all(readLines(input),cloze_pattern)

    cloze_table$reset()
    knitr::knit(output = tempfile(), text = cloze_rmd, envir = new.env())

    delimiters <- grep("^(---)\\s*$", cloze_rmd)
    if(length(delimiters) == 2) delimiters <- c(delimiters, length(cloze_rmd))

    exams_rmd <- c(
      "`r teachr:::cloze_table$reset()`",
      "Question",
      "========",
      cloze_rmd[(delimiters[2]+1):(delimiters[3]-1)],
      "Answerlist",
      "----------",
      "`r teachr:::cloze_questionlist()`",
      "",
      "Solution",
      "========",
      cloze_rmd[-(delimiters[1]:delimiters[3])],
      "Answerlist",
      "----------",
      "`r teachr:::cloze_answerlist()`",
      # paste("*", paste0("`r ", do.call(rbind, cloze_sol)[,3], "`")),
      "",
      "Meta-information",
      "========",
      "extype: cloze",
      "exsolution: `r paste0(do.call(c, teachr:::cloze_table$list()), collapse = '|')`",
      paste("exclozetype:", paste0(vapply(cloze_table$list(), function(x) cloze_type(x), character(1L)), collapse = "|")),
      paste("exname:", xfun::sans_ext(basename(input))),
      paste("extol:", front_matter$tolerance%||%0.05)
    )
    xfun::write_utf8(exams_rmd, tmp <- tempfile(fileext = ".Rmd"))

    file_nm <- xfun::sans_ext(basename(input))
    exams::exams2moodle(
      tmp, name = file_nm, stitle = file_nm, n = front_matter$times
    )

    # Fix bug with exams package producing empty 2-deep list on first question
    out_file <- xfun::with_ext(file_nm, ".xml")
    xfun::write_utf8(
      stringr::str_replace(
        paste0(xfun::read_utf8(out_file), collapse = "\n"),
        fixed("<ul>\n<li><ul>\n<li></li>\n</ul></li>\n</ul> {"),
        "{"
      ),
      out_file
    )

    exams::exams2html(tmp, name = file_nm)
    stop("Done! This error is a quick hack to stop render() proceeding pointlessly.")
  }

  # return format
  output_format(
    knitr = knitr_options(),
    pandoc = pandoc_options(to = "markdown+raw_tex", ext = ".md"),
    pre_knit = pre_knit,
    base_format = rmarkdown::md_document(
      ...
    )
  )
}

cloze_table <- function() {
  table <- list()
  list(
    add = function(result) {
      table[[length(table) + 1]] <<- result
    },
    list = function() {
      table
    },
    length = function() {
      length(table)
    },
    reset = function() {
      table <<- list()
    }
  )
}

cloze_table <- cloze_table()

#' Cloze question
#'
#' Use this function to declare a cloze question in an R Markdown document
#' with the `teachr::teachr_cloze` output format.
#'
#' @param x The question's solution.
#'
#' @export
cloze <- function(x) {
  cloze_table$add(x)
  paste0("##ANSWER", cloze_table$length(), "##")
}

cloze_type <- function(x, ...) {
  UseMethod("cloze_type")
}
cloze_type.numeric <- function(x, ...) "num"
cloze_type.character <- function(x, ...) "string"
cloze_type.moodle_schoice <- function(x, ...) "schoice"
cloze_type.moodle_mchoice <- function(x, ...) "mchoice"
cloze_type.default <- function(x, ...) stop("Unsupported cloze question type.")

cloze_questionlist <- function() {
  paste0(
    do.call(c, lapply(cloze_table$list(), function(x) paste("*", cloze_question(x)))),
    collapse = "\n"
  )
}
cloze_question <- function(x, ...) {
  UseMethod("cloze_question")
}
cloze_question.numeric <- function(x, ...) ""
cloze_question.character <- function(x, ...) ""
cloze_question.moodle_schoice <- function(x, ...) {
  attr(x, "opts")
}

cloze_answerlist <- function() {
  paste0(
    do.call(c, lapply(cloze_table$list(), function(x) paste("*", cloze_answer(x)))),
    collapse = "\n"
  )
}
cloze_answer <- function(x, ...) {
  UseMethod("cloze_answer")
}
cloze_answer.numeric <- function(x, ...) x
cloze_answer.character <- function(x, ...) x
cloze_answer.moodle_schoice <- function(x, ...) {
  pos <- str_locate_all(x, fixed("1"))[[1]][,1]
  qs <- attr(x, "opts")
  qs[pos] <- paste0("**", attr(x, "opts")[pos], "**")
  qs
}

#' Single choice question
#'
#' Use this function in a cloze inline chunk to describe a single choice
#' question.
#'
#' @param opts The options to choose from (character vector)
#' @param sol The correct solution (the correct value from `opts`, or the
#' position of the correct value)
#'
#' @export
schoice <- function(opts, sol) {
  if(is.character(sol)) sol <- match(sol, opts)
  sol_code <- rep_along(opts, "0")
  sol_code[sol] <- "1"
  sol_code <- paste0(sol_code, collapse = "")
  return(structure(sol_code, opts = opts, class = "moodle_schoice"))
}

#' Multiple choice question
#'
#' Use this function in a cloze inline chunk to describe a single choice
#' question.
#'
#' @param opts The options to choose from (character vector)
#' @param sol The correct solution (the correct value from `opts`, or the
#' position of the correct value)
mchoice <- function(opts, sol) {
  if(is.character(sol)) sol <- match(sol, opts)
  sol_code <- rep_along(opts, "0")
  sol_code[sol] <- "1"
  sol_code <- paste0(sol_code, collapse = "")
  return(structure(sol_code, opts = opts, class = c("moodle_mchoice", "moodle_schoice")))
}

