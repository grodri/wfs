# get a dictionary or data file from the data archive or
# the local file system
#
wfs_get <- function(dataset, filetype, source = "") {
  if(source == "") {
    source <- wfs_url(dataset)
  }
  infile <- paste(paste(source, dataset, sep = "/"), filetype, sep = ".")
  if(length(grep("^(http|ftp|https)://", infile))) {
    suffix <- paste(".", filetype, sep="")
    outfile <- tempfile(pattern="tmp", fileext = suffix)
    download.file(infile, outfile, quiet = TRUE, mode = "wb")
    infile <- outfile
    on.exit(unlink(infile))
  }
  readLines(infile)
}
