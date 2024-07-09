\name{write_csv_with_list_col}
\alias{write_csv_with_list_col}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a df with List Columns to a csv
}
\description{
Convert list columns to a string with comma separated elements of the list.
Write into a csv.
}
\usage{
write_csv_with_list_col(my_df, csv_out_file_path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
A dataframe with list columns.
}
  \item{csv_out_file_path}{
A vaild file path for the output .csv file.
}
}
\details{
Explanations:

- `write_csv_with_list_col <- function(my_df, csv_out_file_path) { ... }` defines a function `write_csv_with_list_col` that takes a data frame `my_df` and a file path `csv_out_file_path` as inputs.

- `my_df |>` starts the pipeline with the data frame `my_df`.

- `rowwise()` applies subsequent operations row-wise instead of column-wise:

  - `rowwise()` is used to apply functions to each row individually.

- `mutate_if(is.list, ~ paste(unlist(.), collapse = ', '))` collapses list columns into a single string for each row:

  - `mutate_if(is.list, ...)` applies the specified function to columns that are lists.
  
  - `~ paste(unlist(.), collapse = ', ')` is an anonymous function that converts the list to a character vector using `unlist()`, then concatenates the elements into a single string separated by commas using `paste(..., collapse = ', ')`.

- `readr::write_csv(csv_out_file_path)` writes the modified data frame to a CSV file:

  - `readr::write_csv()` is a function from the `readr` package that writes a data frame to a CSV file.

  - `csv_out_file_path` specifies the path where the CSV file will be saved.

This function processes the data frame `my_df` to convert list columns into comma-separated strings, then writes the resulting data frame to a CSV file at the specified path `csv_out_file_path`.
}
\value{
Return nothing, creates a csv file.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anna Shipunova
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
