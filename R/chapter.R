
#' R Markdown format for Distill articles
#'
#' Scientific and technical writing, native to the web.
#'
#' Distill articles feature attractive, reader-friendly typography, flexible
#' layout options for visualizations, and full support for footnotes and
#' citations.
#'
#' @inheritParams rmarkdown::html_document
#'
#' @import rmarkdown
#'
#' @export
teachr_chapter <- function(self_contained = TRUE,
                            extra_dependencies = NULL,
                            theme = NULL,
                            includes = NULL,
                            keep_md = FALSE,
                            lib_dir = NULL,
                            md_extensions = NULL,
                            pandoc_args = NULL,
                            ...) {
  post_processor <- function(front_matter, input, output_file, ...) {
    # Add front matter to md file
    front_matter$output <- NULL
    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(front_matter),
        "---",
        xfun::read_utf8(output_file)
      ),
      output_file
    )
    output_file
  }

  on_exit <- function() {
    # validate_rstudio_version()
  }

  # return format
  output_format(
    knitr = knitr_options(),
    pandoc = pandoc_options(to = "markdown_strict", ext = ".md"),# args = c("-f", "raw_html")),
    keep_md = keep_md,
    clean_supporting = self_contained,
    # post_knit = post_knit,
    post_processor = post_processor,
    on_exit = on_exit,
    base_format = rmarkdown::md_document(
      # pandoc_args = pandoc_args,
      ...
    )
  )
}


#' Slide output for the chapter
#'
#' @param name The name of the slide without the file extension
#' @export
slides <- function(name) {
  if(knitr::is_html_output(excludes = c("markdown"))) {
    glue::glue("<iframe src='../slides/{name}.html' width='800' height='500'></iframe>")
  } else {
    glue::glue('<slides source="{name}">\n</slides>')
  }
}

#' Mulitple choice options for the chapter
#' @param ... A name-value pair where name shows the text, value shows the
#'   message. Each pair should be an option.
#' @param correct The number of the option that is correct.
#' @export
mc_opts <- function(..., correct = NULL) {
  messages <- list(...)
  text <- names(messages)
  if(knitr::is_html_output(excludes = c("markdown"))) {
    out <- '<div>'
    for(i in seq_along(messages)) {
      answer <- ifelse(i==correct, '<span style="color:red">CORRECT</span>', '')
      out <- c(out, glue::glue('<input type="radio"> {text[i]} | {answer} {messages[i]}'))
    }
    out <- c(out, "</div>")
  } else {
    out <- '<choice>'
    for(i in seq_along(messages)) {
      answer <- ifelse(i==correct, 'correct="true"', '')
      out <- c(out, glue::glue('<opt text="{text[i]}" {answer}>'), messages[i], '</opt>')
    }
    out <- c(out, "</choice>")
  }
  paste(out, collapse = "\n\n")
}


#' The codeblock for the chapter
#' @param id The id of the codeblock
#' @param hint The hint.
#' @export
codeblock <- function(id, hint) {
  if(knitr::is_html_output(excludes = c("markdown"))) {
    out <- c('<pre class="r">',
             paste("<code>",
                   readLines(sprintf("../exercises/exc_%s.R", id)),
                   "</code>"),
             '</pre>',
             '<details open><summary>Hint</summary>',
             hint,
             '</details>')
  } else {
    out <- c(glue::glue('<codeblock id="{id}">'),
             hint,
             "</codeblock>")
  }
  paste(out, collapse = "\n")
}
