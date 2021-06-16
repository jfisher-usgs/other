# Function that returns the latest valid snapshot from MRAN
get_mran_url <- function() {
  if (!requireNamespace("checkpoint", quietly = TRUE)) {
    utils::install.packages("checkpoint", repos = "https://cloud.r-project.org", type = "binary")
  }
  mran_url <- "https://mran.microsoft.com"
  valid_snapshots <- checkpoint::list_mran_snapshots(mran_url)
  snapshot_date <- utils::tail(valid_snapshots, 1)
  file.path(sub("/$", "", mran_url), "snapshot", snapshot_date)
}


# set global options
options(
  Ncpus = max(1L, parallel::detectCores(logical = FALSE) - 1L),
  repos = c("CRAN" = get_mran_url()),
  pkgType = "binary",
  install.packages.check.source = "no"
)


# install missing packages
local({
  pkgs <- c("remotes", "renv", "languageserver", "pkgbuild")
  paths <- find.package(pkgs, quiet = TRUE, verbose = FALSE)
  missing_pkgs <- setdiff(pkgs, basename(paths))
  if (length(missing_pkgs)) {
    try(utils::install.packages(missing_pkgs, quiet = TRUE), silent = TRUE)
  }
})


# Function to load package and open help documentation
ph <- function(package) {
  if (!tryCatch(is.character(package) && length(package) == 1L, error = function(e) FALSE)) {
    package <- deparse(substitute(package), nlines = 1)
  }
  choices <- as.character(utils::installed.packages()[, "Package"])
  package <- match.arg(package, choices)
  library((package), character.only = TRUE, quietly = TRUE)
  suppressMessages(utils::help(package = (package), help_type = "html"))
}


# Function to load package functions and datasets in the current environment for debugging
pd <- function(path = ".", envir = parent.frame()) {
  path <- normalizePath(path.expand(path), winslash = "/")
  if (!file.exists(file.path(path, "DESCRIPTION"))) stop("Invalid DESCRIPTION")
  suppressPackageStartupMessages(
    library(basename(path), character.only = TRUE, quietly = TRUE)
  )
  files <- list.files(file.path(path, "R"), "\\.R$", full.names = TRUE)
  for (file in files) source(file, local = envir)
  files <- list.files(file.path(path, "R"), "\\.rda$", full.names = TRUE)
  for (file in files) load(file, envir = envir)
  files <- list.files(file.path(path, "data"), "\\.rda$", full.names = TRUE)
  for (file in files) load(file, envir = envir)
  invisible()
}
