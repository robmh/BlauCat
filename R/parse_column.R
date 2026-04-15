#' Title
#'
#' @param x
#' @param col_name
#'
#' @returns
#'
#' @export
#' @examples
parse_column <- function(x) {

  # Checks.
  if (!is.vector(x)) cli::cli_abort("Input 'x' must be a vector")


  # Remove possible trailing or leading white spaces.
  x <- trimws(x,  whitespace = "[\\h\\v]")


  # If it is a character string, go on.
  if (!is.numeric(x)) {

    # First decimal commas are substituted by decimal dots. If there is a conflict, stop.
    x <- gsub(",", ".", x)

    # Remove percentage sign.
    x <- gsub("%", "", x)

    # Substitute single minus sign by NA.
    x[x == "-"] <- NA

    # Can it be coerced to numeric yet?
    w <- tryCatch(as.numeric(x), warning = function(w) w)
    if ("warning" %in% class(w)) {

      # Split by ±.
      x <- trimws(x,  whitespace = "[\\h\\v]")
      x[x == ""] <- NA
      x <- strsplit(x, "±")
      x <- lapply(x, function(y) trimws(y,  whitespace = "[\\h\\v]"))
      nx <- sapply(x, length)

      # Has it found elements to split?
      if (any(nx > 1)) {
        x_mean <- sapply(x, "[[", 1)
        x_mean <- as.numeric(x_mean)
        x_sd <- rep(NA_real_, length(x))
        x_sd[nx > 1] <- sapply(x[nx > 1], "[[", 2)
        x_sd <- as.numeric(x_sd)
        x <- data.frame(mean = x_mean, sd = x_sd)

      } else {
        x <- unlist(x)
      }

    } else {
      x <- unlist(w)
    }
  }

  return(x)

}
