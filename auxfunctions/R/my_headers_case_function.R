my_headers_case_function <-
function(x) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    .Internal(tolower(x))
}
