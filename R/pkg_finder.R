# find all packages used in this project ----

# include this file to test odd library formations:
library( 'dplyr' ) # for pipes
library("purrr") # for iteration
library( stringr ) # for text extraction

# find files that might have library() or pkg::func() notation
# EDIT FOR YOUR PROJECT
qmd_files <- list.files(pattern = "\\.qmd$")
r_files <- list.files("R", full.names = TRUE)
files <- c(qmd_files, r_files)

# read files contents
contents <- map(files, readLines)

# look for library function
lib_pattern <- "(?<=library\\()[A-Za-z0-9_\\.\'\"]+(?=\\))"
library <- map(contents, str_extract, pattern = lib_pattern) %>%
  unlist() %>% 
  unique() %>% 
  gsub("\"|\'", "", .) %>% 
  trimws()

# look for pkg::fnc() or pkg:::func()
# colon_pattern <- "[A-Za-z0-9_\\.]+(?=:{2,3}[A-Za-z0-9_\\.]+)"
# :{2,3} doesn't work with lookahead ^, so do 2 and 3 separately
colon_pattern2 <- "[A-Za-z0-9_\\.]+(?=::[A-Za-z0-9_\\.]+)"
colon2 <- map(contents, str_extract, pattern = colon_pattern2) %>%
  unlist() %>% unique()
colon_pattern3 <- "[A-Za-z0-9_\\.]+(?=:::[A-Za-z0-9_\\.]+)"
colon3 <- map(contents, str_extract, pattern = colon_pattern3) %>%
  unlist() %>% unique()

# combine and find packages not installed here
packages <- c(library, colon2, colon3) %>% 
  unique() %>% 
  setdiff(c(NA, "")) # get rid of NA or ""
# missing <- setdiff(packages, installed.packages()) # can take a long time
pkg_locations <- map(packages, find.package, quiet = TRUE)
missing_idx <- !map_dbl(pkg_locations, length)
missing <- packages[missing_idx]

# return missing package install code
# beware - this will also pick up demo code like pkg::func()
# and can't tell if a package is on CRAN
if (length(missing) > 0) {
  paste0("install.packages(\"", missing, "\")", collapse = "\n") %>%
    paste0("You have ", length(missing), " missing packages:\n", .) %>%
    warning(call. = FALSE)
} else {
  message("You have 0 missing packages.")
}
