#' Data extraction
#'
#' Extract a set of variables from a dataset reading it from
#' the DHS data archive or the local file system
#'
#' @param varlist A required string with a comma-separated
#'  list of variables, for example "v010, v110". May include
#'  ranges, wilcards or keywords as explained under Details
#' @param dataset A required string with the name of a dataset,
#'  for example "cosr02"
#' @param source An optional string specifying a local folder where
#'  to find the files, leave blank to read the files from the DHS
#'  data archive
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
#' The function returns a data frame.
#'
#' Each variable with value labels has a \code{"labels"} attribute
#' with the value labels, unless it was converted to a factor.
#' This information can be used to convert a variable to a factor
#' at a later time using the function \code{labelled::to_factor()}
#'
#' Dictionaries may specify missing values and special codes.
#' We recode all missing values to \code{NA}.
#' For variables that are not converted to factors, we add the
#' special code, if any, as a \code{"special"} attribute of the variable.
#' Any numeric values greater or equal to the special code require
#' special treatment in analysis.
#'
#' @section Example:
#' wfs_extract("v011, v111", "cosr02")
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
    raw <- str_trim(substr(dat, bots[j], tops[j]))
    if(lens[j] <= 8) {
      if(str_detect(raw[1],"\\.")) df[,j] = as.numeric(raw)
      else df[, j] <- strtoi(raw, base=10) # non-numeric becomes NA
    }
    else {
      df[, j] <- as.character(raw)
    }
  }

  # process variables
  for(j in 1:length(varlocs)) {
    cat(varnames[j],"\n") # debug
    # handle na
    na <- strtoi(substr(recs[j], 26, 29), base=10)
    vl <- wfs_value_labels(varnames[j], dct)
    if(!is.na(na)) {
      isna <- df[,j] == na | is.na(df[,j]) # treat NA as TRUE
      df[isna, j] <- NA
      if(!is.null(vl)){
        vl <- vl[vl != na]
      }
    }
    # get special code
    sc <- strtoi(substr(recs[j], 31, 34), base=10)

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

    # special codes
    if(!isfactor & !is.na(sc)) attr(df[,j], "special") <- sc
  }
  names(df) <- varnames
  df
}

