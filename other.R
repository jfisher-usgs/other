ph <- function(package) {
  if (!tryCatch(is.character(package) && length(package) == 1L, error=function(e) FALSE)) {
    package <- deparse(substitute(package), nlines=1)
  }
  choices <- as.character(utils::installed.packages()[, "Package"])
  package <- match.arg(package, choices)
  library((package), character.only=TRUE, quietly=TRUE)
  suppressMessages(utils::help(package=(package), help_type="html"))
}
