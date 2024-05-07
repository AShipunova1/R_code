list_sort_uniq <-
function(my_lists) {
  # browser()
  res <-
    my_lists |>
    str_trim() |>
    unique() |>
    sort() |>
    list() |>
    flatten()
  return(res)
}
