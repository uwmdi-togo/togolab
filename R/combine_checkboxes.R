#' Collapse REDCap checkbox columns into a single labelled column
#'
#' REDCap exports a multi-select ("checkbox") field as one 0/1 column per
#' option, named `base___1`, `base___2`, ... This collapses them into a single
#' character column called `base`, joining the labels of every checked option
#' with `sep`. It is the R port of the Python `combine_checkboxes()` helper used
#' across the data-harmonization study scripts.
#'
#' @param df A data frame containing the `base___N` columns.
#' @param base_name Character. The checkbox field stem (e.g. "race").
#' @param levels Character vector of option labels, in REDCap option order
#'   (so `levels[1]` corresponds to `base___1`, etc.).
#' @param sep Separator used when more than one option is checked.
#' @param drop If TRUE (default) the original `base___N` columns are removed.
#' @return The data frame with a new `base_name` column (and, by default, the
#'   per-option columns dropped).
#' @export
combine_checkboxes <- function(df, base_name, levels, sep = " & ", drop = TRUE) {
  cols <- paste0(base_name, "___", seq_along(levels))
  present <- cols[cols %in% names(df)]
  if (length(present) == 0L) {
    warning("combine_checkboxes(): no '", base_name, "___N' columns found.",
            call. = FALSE)
    df[[base_name]] <- NA_character_
    return(df)
  }
  # A value counts as "checked" if it is 1, "1", or "Checked"
  checked <- vapply(
    present,
    function(cn) as.character(df[[cn]]) %in% c("1", "Checked"),
    logical(nrow(df))
  )
  if (is.null(dim(checked))) checked <- matrix(checked, nrow = nrow(df))
  level_idx <- match(present, cols) # which level each present column maps to

  df[[base_name]] <- apply(checked, 1L, function(row) {
    sel <- level_idx[which(row)]
    if (length(sel) == 0L) "" else paste(levels[sel], collapse = sep)
  })

  if (drop) df <- df[, !(names(df) %in% present), drop = FALSE]
  df
}
