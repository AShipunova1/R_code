\name{load_csv_names_in_one_df}
\alias{load_csv_names_in_one_df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load Csv Names in One Dataframe
}
\description{
This function is useful for loading multiple CSV files into a single data frame, handling different column types, missing values, and repairing column names if necessary.
}
\usage{
load_csv_names_in_one_df(path_to_files, csv_names_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_to_files}{
%%     ~~Describe \code{path_to_files} here~~
}
  \item{csv_names_list}{
%%     ~~Describe \code{csv_names_list} here~~
}
}
\details{
The `load_csv_names_in_one_df` function is designed to load multiple CSV files into a single data frame. Here's a detailed explanation of each part of the function:

1. **Function Definition:**
   - `load_csv_names_in_one_df <- function(path_to_files, csv_names_list) \{ ... \}`: Defines a function named `load_csv_names_in_one_df` with two arguments: `path_to_files` (the directory path where CSV files are located) and `csv_names_list` (a list of CSV file names).

2. **Variable Initialization:**
   - `myfiles <- csv_names_list`: Initializes a variable `myfiles` with the list of CSV file names.

3. **Conditional Path Handling:**
   - `if (length(path_to_files) > 0) \{ ... \}`: Checks if the `path_to_files` argument is not empty.
   - `lapply(csv_names_list, function(x) file.path(path_to_files, x))`: If `path_to_files` is not empty, constructs the full file paths by concatenating `path_to_files` with each file name from `csv_names_list`. This is done using `lapply` to apply the `file.path` function to each element of `csv_names_list`.

4. **Reading CSV Files:**
   - `purrr::map_df(myfiles, function(file_name) \{ ... \})`: Reads each CSV file specified in `myfiles` into a data frame and combines them into a single data frame using `map_df` from the `purrr` package.
   - `readr::read_csv(file_name, col_types = cols(.default = "c"), trim_ws = TRUE, na = c("", "NA", "NaN"), name_repair = "universal")`: Reads each CSV file specified by `file_name` using the `read_csv` function from the `readr` package. It specifies the column types to read as characters, trims whitespace from column values, specifies the values to interpret as missing (`""`, `"NA"`, `"NaN"`), and repairs column names to make them syntactically valid.

5. **Return Result:**
   - `return(csv_content)`: Returns the combined data frame containing the contents of all CSV files.
}
\value{
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
%%  Anna Shipunova
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
