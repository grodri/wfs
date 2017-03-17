#' Data extraction
#'
#' Extract a set of variables from a dataset reading it from
#' the DHS data archive or the local file system
#'
#' @param varlist A required string with a comma-separated
#'  list of variables, for example "v010, v110"
#' @param dataset A required string with the name of a dataset,
#'   for example "cosr02"
#' @param source An optional string specifying a local folder where
#'  to find the files, leave blank to read from the DHS data archive
#' @param convert.factors An optional boolean, use value labels to
#'  create factors?
#' @return a data frame with attributes.
#'
#' @section Details:
#' The \code{varlist} is a comma-separated list of variable names, all in
#' lowercase (even if dictionaries use uppercase).
#' The list may include a \emph{range} such as  \code{v701-v705}
#' to extract variables v701 to v705.
#' It may also include the \emph{wildcards} \code{?} and \code{*}
#' to match one or more characters, so \code{m??2} extracts
#' the date of union for all unions.
#' You may also use the \emph{keywords} \code{unions} to extract
#' the union history, \code{births} for the birth history, or
#' \code{all} for all variables.
#'
#' The \code{dataset} is required and must be the name of a
#' dataset, for example \code{cosr02}. The dataset consists of
#' a dictionary file with extension \code{.dct} and an ASCII
#' data file with extension \code{.dat}.
#'
#' The \code{source} is the name of a local folder where the
#' two files mentioned above may be found. If left blank the
#' function will download the dictionary and data files directly
#' from the DHS data archive.
#'
#' By default \code{convert.factors} is \code{TRUE} and we
#' convert a variable to a factor if the dictionary specifies
#' value labels and \emph{all} values in the data other than
#' \code{NA} have a corresponding value label. The conversion
#' may be turned off by setting the flag to \code{FALSE}
#'
#' \strong{todo:}.
#' The function returns a data frame with attributes, namely
#' a \code{"variables"} and a \code{"labels"} attribute, both
#' data frames.
#' The \emph{variables} data frame has one row per variable and
#' columns for the name, min, max, not applicable, and special codes,
#' the variable label, and the name of a set of value labels.
#' The \emph{labels} data frame has one row per labeled value,
#' with columns for the set name, the value and the label.
#' If \code{convert.factors} is set to \code{FALSE} during
#' extraction this #' information may be used to do the conversion
#' at a later time.
#' @export
wfs_extract <- function(varlist, dataset, source = "",
  convert.factors = TRUE) {

  # data files
  dct <- wfs_dct(dataset, source)
  dat <- wfs_get(dataset, "dat", source)
  varlocs <- wfs_varlist(varlist, dct)
  varnames <- names(varlocs)
  recs <- dct[varlocs]

  # extract data
  bots <- strtoi(substr(recs, 10, 13), base=10)
  lens <- strtoi(substr(recs, 15, 16), base=10)
  tops <- bots + lens - 1
  df <- data.frame(matrix(NA, length(dat), length(varlocs)))
  for(j in 1:length(varlocs)) {
    raw <- substr(dat, bots[j], tops[j])
    if(lens[j] <= 8) {
      df[, j] <- strtoi(raw, base=10)
    }
    else {
      df[, j] <- as.character(raw)
    }
  }

  # process variables
  for(j in 1:length(varlocs)) {
    cat(varnames[j],"\n")##debug
    # handle na
    na <- strtoi(substr(recs[j], 26, 29), base=10)
    vl <- wfs_value_labels(varnames[j], dct)
    if(!is.na(na)) {
      isna <- df[,j] == na
      df[isna, j] <- NA
      if(!is.null(vl)){
        vl <- vl[vl != na]
      }
    }

    # handle factors
    isfactor <- FALSE
    if(convert.factors & !is.null(vl) & lens[j] < 4) {
      values <- c(vl, NA)
      if(all(df[,j] %in% values)) {
        isfactor <- TRUE
        df[,j] <- factor(df[,j], levels = vl, labels = names(vl))
      }
    }

    # value labels
    if(!is.null(vl) & !isfactor & lens[j] < 9) {
      labelled::val_labels(df[ ,j]) <- vl
    }

    # variable label
    label = stringr::str_trim(substr(dct[varlocs[j]], 36, 65))
    if(label != "") labelled::var_label(df[, j]) <- label
  }
  names(df) <- varnames
  df
}

