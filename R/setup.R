# book-specific code to include on every page

dcc_theme <- list(
  comp = "#4864DB",
  dist = "#10D4CF",
  rels = "#8DEA74",
  time = "#F5C748",
  cert = "#EA4E11"
)

# custom knit_print functions for data.frame and tbl_df
library(knitr)

`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}

knit_print.data.frame <- function (x, options, ...) {
  # get options
  digits <- options$digits %||% getOption("digits")
  rownames <- options$rownames %||% FALSE
  pageLength <- options$pageLength %||% 10 
  escape <- options$escape %||% TRUE
  caption <- options$fig.cap
  
  # remove caption so it doesn't print twice (NOT WORKING)
  options$fig.cap <- NULL 
  
  # use DT for longer tables in html
  if (nrow(x) > pageLength & knitr::is_html_output()) {
    numeric_cols <- sapply(x, is.numeric) |> which() |> names()
    dt <- DT::datatable(x, 
                        rownames = rownames,
                        caption = caption,
                        escape = escape,
                        width = "100%",
                        height = "auto",
                        options = list(pageLength = pageLength),
                        selection = "none")
    if (length(numeric_cols) > 0) {
      dt <- DT::formatRound(dt, 
                            columns = numeric_cols,
                            digits = digits)
    }
    knitr::knit_print(dt, options)
  } else {
    # use kableExtra::kable for PDFs or shorter tables
    k <- kableExtra::kable(x, 
                           digits = digits, 
                           row.names = rownames, 
                           caption = caption,
                           escape = escape) |>
      kableExtra::kable_styling(
        full_width = options$full_width,
        bootstrap_options = c("striped", "hover")
      )
    
    if (knitr::is_html_output()) {
      k <- c("<div class=\"kable-table\">", k, "</div>") |>
        paste(collapse = "\n")
    }
    
    knitr::asis_output(k)
  }
}

registerS3method("knit_print", "data.frame", knit_print.data.frame)


# default knitr options
knitr::opts_chunk$set(
  echo       = TRUE,
  warning    = FALSE,
  message    = FALSE,
  results    = "hold",
  out.width  = '100%',
  fig.width  = 8,
  fig.height = 8,
  fig.align  = 'center',
  digits = 3
)

pkg <- function(txt, url = NULL) {
  if (is.null(url)) {
    sprintf("<code class='package'>%s</code>", txt)
  } else {
    sprintf("<code class='package'><a href='%s' target='_blank'>%s</a></code>", url, txt)
  }
}
