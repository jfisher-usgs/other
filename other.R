# Function that returns the latest valid MRAN URL
get_mran_url <- function() {
  if (!requireNamespace("checkpoint")) install.packages("checkpoint")
  mran_root_url <- checkpoint::mranUrl()
  valid_snapshots <- checkpoint::getValidSnapshots(mran_root_url)
  snapshot_date <-  max(as.Date(valid_snapshots))
  paste0(mran_root_url, snapshot_date)
}


# set options
options(
  pkgType = "binary",
  Ncpus = max(1L, parallel::detectCores(logical = FALSE) - 1L),
  repos = c("CRAN" = get_mran_url())
)


# install missing packages
local({
  pkgs <- c("remotes", "renv", "languageserver")
  paths <- find.package(pkgs, quiet = TRUE, verbose = FALSE)
  missing_pkgs <- setdiff(pkgs, basename(paths))
  if (length(missing_pkgs)) {
    if (Sys.info()["sysname"] %in% "Windows") {
      options(install.packages.check.source = "no", pkgType = "binary")
    }
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
