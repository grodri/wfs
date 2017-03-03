#' Reshaping history data
#'
#' Reshape the union or birth history from wide to long format and back.
#'
#' @param data  Required data frame based on a WFS standard recode file
#' @param direction Required string set to "long" or "wide"
#' @param history Required string set to "unions" or "births"
#' @param drop Optional boolean indicating whether to drop empty entries
#' @return A reshaped data frame with attributes
#'
#' @section Details:
#' \code{wfs_reshape} relies on R's \code{reshape} to do the work,
#' generating the appropriate function call to go from wide to long format.
#' Note that all history variables must have been extracted, usually by
#' specifying the \code{unions} or \code{births} keyword.
#'
#' Typically you will first reshape in the "long" direction to create
#' one record per union or per birth. By default the function keeps only
#' actual entries in the history, but setting \code{drop = FALSE} will keep
#' them all.
#'
#' If you reshape back to "wide" format we rely on the fact that R's
#' \code{reshape} is reversible, checking only that the dataset is in the
#' appropriate long format. By default the restored file will have only
#' ever-married women or mothers unless empty entries were retained.
#' @export
wfs_reshape <- function(data, direction, history, drop = TRUE) {
  if(!direction %in% c("long", "wide")) {
    stop("direction must be \"long\" or \"wide\"")
  }
  if(!history %in% c("unions", "births")) {
    stop("history must be \"unions\" or \"births\"")
  }
  if(history == "unions") {
    wide_names <- wfs_wide_names("m", 8, 4)
    long_names <- paste("m", 1:4, sep = "")
    counter = "union"
  }
  else {
    wide_names <- wfs_wide_names("b", 24, 5)
    long_names <- paste("b", 1:5, sep = "")
    counter = "birth"
  }
  if(direction == "long") {
    previous <- attr(data, "reshapeWide")
  }
  else {
    previous <- attr(data, "reshapeLong")
  }
  #debug cat("is.null(previous)=", is.null(previous),"\n")
  # undo previous reshape
  undo <- !is.null(previous) && previous$timevar == counter
  #debug cat("undo = ", undo, "\n")
  if(undo) {
    df <- reshape(data)
    if(direction == "long" && drop) {
      df <- df[!is.na(df[,long_names[1]]) ,]
    }
    else if (direction == "wide") {
      rw <- attr(df, "reshapeWide")
      if(ncol(rw$varying) != length(rw$times)) {
        attr(df, "reshapeWide")$varying <- rw$varying[, rw$times]
      }
    }
  }
  # reshape long for first time
  else if (direction == "long") {
    if(!all(wide_names %in% names(data))) {
      stop(paste("Reshape long requires data for all",history,"in wide format"))
    }
    if(!"v000" %in% names(data)) {
      data$v000 <- 1:nrow(data)
    }
    df <- reshape(data, varying = wide_names, v.names = long_names,
      timevar = counter, idvar = "v000", direction = "long")
    if(drop) {
      df <- df[!is.na(df[,long_names[1]]) ,]
    }
  }
  # reshape wide requires
  else {
    stop("can only reshape wide a WFS dataset previously reshaped long on same history")
  }
  df
}
