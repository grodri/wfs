wfs_url <- function(dataset = "") {
  url <- "https://wfs.dhsprogram.com"
  if(dataset != "") {
    cc <- substr(dataset, 1, 2)
    if(substr(dataset, 5, 5) == "2") cc <- paste0(cc,"2")
    url <- paste(url, cc, sep = "/")
  }
  url
}
