# Extract own labels for variable in position loc of dct
#
wfs_label_extract <- function(loc, dct) {
  pos <- loc
  while(pos < length(dct) & substr(dct[pos+1], 1, 1) == " ") {
    pos <- pos + 1
  }
  if(pos - loc < 1) return(NULL)
  rows <- (loc+1):pos
  data.frame(
    value = strtoi(stringr::str_trim(substr(dct[rows], 36, 39)), base=10),
    label = str_trim(substr(dct[rows], 44, 63)),
    stringsAsFactors = FALSE
  )
}
