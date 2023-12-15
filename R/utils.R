#' Check and install dependencies
#'
#' Checks if a list of packages are installed and installs them if not. Trigger
#' an error if the user chooses not to install a package.
#'
#' @param deps A character vector of package names.
#'
#' @return Nothing.
#'
check_and_install_dependencies <- function(deps) {
  for (dep in deps) {
    stop_message <- paste0(dep, " is required but was not installed.")
    # Check if the package is installed
    is_installed = requireNamespace(dep, quietly = TRUE)

    if (!is_installed) {
      # If not, ask the user if they want to install it
      if (interactive()) { # Only ask if the session is interactive otherwise just stop
        is_installed <- utils::menu(
          c("Yes", "No"),
          title = paste0(dep, " is not installed. Install it now?")) == 1
      }
    }

    # Stop if the package is not installed
    if (!is_installed) stop(stop_message)
  }
}
