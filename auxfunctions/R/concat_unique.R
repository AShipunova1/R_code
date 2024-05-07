concat_unique <-
function(x) {
  paste0(unique(x[!is.na(x)]), collapse = ", ")
}
