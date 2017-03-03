#' View documentation
#'
#' When called with no arguments this function points your browser
#' to the WFS home page in the DHS data archive. When called with
#' a dataset name it opens the data dictionary, reading it from
#' a local file or from the data archive.use_vignett
#'
#' @param dataset String with optional dataset name, e.g. "cosr02"
#' @param source  String with source folder or blank for DHS archive
#'
#' @examples
#' wfs_view()
#' wfs_view("cosr02")
#' @export
wfs_view <- function(dataset = "", source = "") {
  if(dataset == "") {
    browseURL(wfs_url())
    return(invisible(NULL))
  }
  if(source == "") source <-  wfs_url(dataset)
  dctfile <- paste(paste(source, dataset, sep = "/"), "dct", sep = ".")
  temp <- tempfile()
  if(length(grep("^(http|ftp|https)://", dctfile))) {
    download.file(dctfile, temp, quiet = TRUE, mode = "wb")
  }
  else {
    file.copy(dctfile, temp)
  }
  dctfile <- temp
  on.exit(unlink(dctfile))
  file.show(dctfile, title=dataset)
}

