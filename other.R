
.help <- function(package) {
  if (!tryCatch(is.character(package) && length(package) == 1L, error=function(e) FALSE)) {
    package <- deparse1(substitute(package))
  }
  suppressPackageStartupMessages(library((package), character.only=TRUE, quietly=TRUE))
  suppressMessages(utils::help(package=(package), help_type="html"))
}
