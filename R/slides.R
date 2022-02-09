
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
    body <- xfun::read_utf8(output_file)
    # Quick hack to fix slide seperators
    body <- gsub("^------+$", "---", body)
    # Replace escaped HTML language chunks
    body <- gsub("`(.+?)`\\{=html\\}", "\\1", body)

    # Add front matter to md file
    xfun::write_utf8(
      c(
        "---",
        yaml::as.yaml(list(title = front_matter$title, type = "slides")),
        "---",
        body

      ),
      output_file
    )

    output_file
  }

  post_knit <- function(front_matter, knit_input, runtime, encoding = "UTF-8") {
    if(is.null(front_matter$chapter)) stop("You must specify `chapter: <your_chapter_number>` in the YAML.")

    # Copy figures to static folder
    fig_path <- knitr::opts_chunk$get("fig.path")
    xfun::dir_create(
      ch_path <- file.path("..", "static", paste0("chapter", front_matter$chapter), dirname(fig_path))
    )

    file.copy(fig_path, ch_path, recursive = TRUE)
    NULL
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
    post_knit = post_knit,
    post_processor = post_processor,
    on_exit = on_exit,
    base_format = rmarkdown::md_document(
      ...
    )
  )
}
