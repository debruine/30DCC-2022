# book-specific code to include on every page

dcc_theme <- list(
  comp = "#4864DB",
  dist = "#10D4CF",
  rels = "#8DEA74",
  time = "#F5C748",
  cert = "#EA4E11"
)

# default knitr options
knitr::opts_chunk$set(
  echo       = TRUE,
  warning    = FALSE,
  message    = FALSE,
  results    = "hold",
  out.width  = '100%',
  fig.width  = 8,
  fig.height = 4.5,
  fig.align  = 'center'
)

pkg <- function(txt, url = NULL) {
  if (is.null(url)) {
    sprintf("<code class='package'>%s</code>", txt)
  } else {
    sprintf("<code class='package'><a href='%s' target='_blank'>%s</a></code>", url, txt)
  }
}
