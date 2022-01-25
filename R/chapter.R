
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
  # shared variables
  site_config <- NULL
  encoding <- NULL

  # add template
  args <- c("--template",
            pandoc_path_arg(
              system.file(
                "rmarkdown/templates/teachr_chapter/resources/chapter.html",
                package = "teachr", mustWork = TRUE
              )
            )
  )


  # post-knit
  post_knit <- function(metadata, input_file, runtime, encoding, ...) {

    # save encoding
    encoding <<- encoding

    # run R code in metadata
    metadata <- eval_metadata(metadata)

    # pandoc args
    args <- c()

    # compute knitr output file
    output_file <- file_with_meta_ext(input_file, "knit", "md")

    # normalize site config and see if we are in a collection
    site_config <<- site_config(input_file, encoding)
    if (is.null(site_config)) {

      # default site_config to empty
      site_config <<- list()
    }

    # # header includes: distill then user
    # in_header <- c(metadata_in_header(site_config, metadata, self_contained),
    #                citation_references_in_header(input_file, metadata$bibliography),
    #                metadata_json,
    #                manifest_in_header(site_config, input_file, metadata, self_contained),
    #                navigation_in_header_file(site_config),
    #                distill_in_header_file(theme))
    #
    # # before body includes: distill then user
    # before_body <- c(front_matter_before_body(metadata),
    #                  navigation_before_body_file(dirname(input_file), site_config),
    #                  site_before_body_file(site_config),
    #                  metadata_includes$before_body,
    #                  listing$html)
    #
    # # after body includes: user then distill
    # after_body <- c(metadata_includes$after_body,
    #                 site_after_body_file(site_config),
    #                 appendices_after_body_file(input_file, site_config, metadata),
    #                 navigation_after_body_file(dirname(input_file), site_config))
    #
    # # populate args
    # args <- c(args,  pandoc_include_args(
    #   in_header = in_header,
    #   before_body = before_body,
    #   after_body = after_body
    # ))

    # return args
    args

  }

  pre_processor <- function(yaml_front_matter, utf8_input, runtime, knit_meta,
                            files_dir, output_dir, ...) {
    pandoc_include_args(in_header = c(site_in_header_file(site_config),
                                      metadata_includes$in_header))
  }

  on_exit <- function() {
    # validate_rstudio_version()
  }

  # return format
  output_format(
    knitr = knitr_options(),
    pandoc = pandoc_options(to = "html5", args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    # post_knit = post_knit,
    # pre_processor = pre_processor,
    on_exit = on_exit,
    base_format = html_document_base(
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
