list_sort_uniq <-
function(my_lists) {
  # browser()
  res <-
    my_lists |>
    stringr::str_trim() |>
    unique() |>
    sort() |>
    list() |>
    purrr::flatten()
  return(res)
}
