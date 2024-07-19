clean_headers <-
function(my_df) {
    # Use the 'fix_names' function to clean and fix the column names of the dataframe.
    new_names <-
        colnames(my_df) |>
        auxfunctions::fix_names()

    colnames(my_df) <- 
        new_names
    
    # Return the dataframe with cleaned and fixed column names.
    return(my_df)
}
