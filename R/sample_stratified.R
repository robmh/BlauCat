#' Title
#'
#' @param df
#' @param p
#' @param colnam
#' @param seed
#'
#' @returns
#' @export
#'
#' @examples
sample_stratified <- function(df, p = 0.8, colnam = NULL, seed = NULL) {

    # Checks
    if (!is.data.frame(df)) {
      cli::cli_abort("Input 'df' must be a 'data.frame' object")
    }

    if (p <= 0 || p >= 1) {
      cli::cli_abort("Wrong p value. It must be >0 and <1")
    }

    if (is.null(colnam)) {
      cli::cli_abort("A column name must be provided as input in 'colnam'")
    }

    if (length(colnam) > 1 || !is.character(colnam)) {
      cli::cli_abort("Input 'colnam' must be a character vector of length = 1")
    }

    if (!(colnam %in% colnames(df))) {
      cli::cli_abort(paste0("Column name '", colnam, "' could not be found"))
    }


    # For reproducible random selection
    if (!is.null(seed)) set.seed(seed)


    # Add temporary row index tracking
    df_temp <- df
    df_temp$.row_id <- seq_len(nrow(df_temp))


    # Calculate overall target number of rows
    target_total <- round(p * nrow(df_temp))


    # Sample proportionally by group, keeping at least 1 point per level
    df_sample <- df_temp |>
      dplyr::group_by(.data[[colnam]]) |>
      dplyr::sample_n(size = max(1, round(p * dplyr::n())), replace = FALSE) |>
      dplyr::ungroup()


    # Adjust total sample size to hit exact target if rounding caused a discrepancy
    current_total <- nrow(df_sample)

    if (current_total < target_total) {
      # Add remaining points randomly from unselected rows using explicit key
      df_remaining <- dplyr::anti_join(df_temp, df_sample, by = ".row_id")
      needed <- target_total - current_total
      df_added <- dplyr::sample_n(df_remaining, size = needed)
      df_sample <- dplyr::bind_rows(df_sample, df_added)

    } else if (current_total > target_total) {
      # Trim excess from groups that have more than 1 point
      excess <- current_total - target_total
      trimmable_indices <- which(duplicated(df_sample[[colnam]]))
      if (length(trimmable_indices) >= excess) {
        trim_ids <- sample(trimmable_indices, size = excess)
        df_sample <- df_sample[-trim_ids, ]
      }
    }


    # Extract and return original row indices
    return(sort(df_sample$.row_id))

}
