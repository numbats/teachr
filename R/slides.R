
#' R Markdown format for course-starter slides
#'
#' Generates the md output suitable for producing slides in the
#' ines/course-starter template.
#'
#' @inheritParams rmarkdown::html_document
#'
#' @import rmarkdown
#'
#' @export
teachr_slides <- function(self_contained = TRUE,
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
    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(list(title = front_matter$title, type = "slides")),
        "---",
        gsub(
          # Quick hack to fix slide seperators
          "^------+$",
          "---",
          xfun::read_utf8(output_file)
        )
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
    pandoc = pandoc_options(to = "markdown+raw_tex", ext = ".md"),
    keep_md = keep_md,
    clean_supporting = self_contained,
    post_processor = post_processor,
    on_exit = on_exit,
    base_format = rmarkdown::md_document(
      ...
    )
  )
}
