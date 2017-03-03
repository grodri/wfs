# Expand a pattern using wildcards ? and *
#
wfs_wild <- function(pattern, map) {
  pattern <- stringr::str_replace_all(pattern, "[?]", "[0-9a-z]")
  pattern <- stringr::str_replace_all(pattern, "[*]", "[0-9a-z]*")
  map[stringr::str_detect(names(map), pattern)]
}
