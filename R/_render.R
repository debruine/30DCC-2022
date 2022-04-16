# copy data directory to docs
R.utils::copyDirectory(
  from = "data",
  to = "docs/data",
  overwrite = TRUE,
  recursive = TRUE)

# copy image directory to docs
R.utils::copyDirectory(
  from = "images",
  to = "docs/images",
  overwrite = TRUE,
  recursive = TRUE)
