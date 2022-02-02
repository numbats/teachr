
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
    out <- glue::glue('<choice id={runif(1L)}>')
    for(i in seq_along(messages)) {
      answer <- ifelse(i==correct, 'correct="true"', '')
      out <- c(out, glue::glue('<opt text="{text[i]}" {answer}>'), messages[i], '</opt>')
    }
    out <- c(out, "</choice>")
  }
  knitr::asis_output(paste(out, collapse = "\n\n"))
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
  knitr::asis_output(paste(out, collapse = "\n"))
}
