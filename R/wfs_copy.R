#' Make local copies
#'
#' Downloads the dictionary and data files for a dataset and saves local copies
#' in a specified folder or the working directory
#'
#' @param dataset A required string with the dataset name, e.g. "cosr02"
#' @param target An optional string with name of folder where to save the files,
#'   defaults to current working directory
#' @examples
#' wfs_copy("cosr02")
#' @export
wfs_copy <- function(dataset, target = "") {
  source <- paste(wfs_url(dataset), dataset, sep = "/")
  if(target == "") {
    target <- getwd()
  }
  target <- paste(target, dataset, sep = "/")
  for(filetype in c("dct","dat")) {
    infile <- paste(source, filetype, sep = ".")
    outfile <- paste(target, filetype, sep = ".")
    cat(outfile,"\n")
    download.file(infile, outfile, quiet = TRUE)
  }
}
