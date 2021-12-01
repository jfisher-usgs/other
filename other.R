# Function that returns the latest valid snapshot from MRAN
get_mran_url <- function() {
  url <- "https://mran.microsoft.com"
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  x <- try(readLines(file.path(url, "snapshot")), silent = TRUE)
  if (inherits(x, "try-error")) {
    stop("Unable to contact Microsoft R Application Network (MRAN) host.", call. = FALSE)
  }
  x <- utils::tail(grep(date_pattern, x, value = TRUE), 1)
  snapshot_date <- gsub(sprintf("<a href=.*?>(%s).*?</a>.*$", date_pattern), "\\1", x)
  file.path(url, "snapshot", snapshot_date)
}


# Function that returns the latest valid snapshot from RSPM (requires USGS VPN access)
get_rspm_url <- function() {
  "https://rpkg.chs.usgs.gov/prod-cran/latest"
}


# set global options
options(
  Ncpus = max(1L, parallel::detectCores(logical = FALSE) - 1L),
  repos = c("CRAN" = get_rspm_url()),
  pkgType = "both",
  install.packages.check.source = "no",
  vsc.use_httpgd = TRUE
)


# install missing packages
local({
  pkgs <- c("remotes", "renv", "languageserver", "httpgd", "pkgbuild")
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


# Function for rounding of numbers using the USGS method
round_usgs <- function(x, digits = 0) {
  z <- abs(x) * 10^digits + 0.5 + sqrt(.Machine$double.eps)
  trunc(z) / 10^digits * sign(x)
}
