# return raw dictionary for a dataset
#
wfs_dct <- function(dataset, source = "") {
  all <- wfs_get(dataset, "dct", source)
  dct <- all[substr(all, 1, 1) != "*"] # remove * comments
  first <- substr(dct, 1, 1)
  alpha <- first >= "A" & first <= "Z"
  varlocs <- (1:length(dct))[alpha]
  varnames <- stringr::str_to_lower(stringr::str_trim(
    substr(dct[varlocs], 1, 6)))
  names(varlocs) <- varnames
  attr(dct, "map") <- varlocs
  dct
}
