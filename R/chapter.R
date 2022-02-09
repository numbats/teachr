
#' R Markdown format for course-starter chapter
#'
#' Generates the md output suitable for producing chapters in the
#' ines/course-starter template.
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
        gsub("`(.+?)`\\{=html\\}", "\\1", xfun::read_utf8(output_file))
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
    pandoc = pandoc_options(to = "markdown", ext = ".md"),# args = c("-f", "raw_html")),
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
