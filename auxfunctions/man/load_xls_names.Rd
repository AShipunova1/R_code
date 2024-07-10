\name{load_xls_names}
\alias{load_xls_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load Xls Names
}
\description{
This function efficiently loads data from multiple Excel files specified by their file paths and names into a single data frame, making it convenient to analyze and manipulate data from multiple sources.
}
\usage{
load_xls_names(my_paths, xls_names_list, sheet_n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_paths}{
%%     ~~Describe \code{my_paths} here~~
}
  \item{xls_names_list}{
%%     ~~Describe \code{xls_names_list} here~~
}
  \item{sheet_n}{
%%     ~~Describe \code{sheet_n} here~~
}
}
\details{
Here's a detailed explanation of the `load_xls_names` function:

1. **Function Definition:**
   - `load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) \{ ... \}`: Defines a function named `load_xls_names` with three arguments: `my_paths` (a list containing file paths), `xls_names_list` (a list of Excel file names), and `sheet_n` (the index of the sheet to read from each Excel file, with a default value of 1).

2. **Constructing File Paths:**
   - `my_inputs <- my_paths$inputs`: Extracts the path to the directory where Excel files are located from the `my_paths` list and assigns it to `my_inputs`.
   - `myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))`: Uses `lapply` to iterate over each Excel file name in `xls_names_list`. For each file name `x`, it constructs the full file path by combining `my_inputs` with `x` using `file.path`. The resulting list of file paths is stored in `myfiles`.

3. **Reading Excel Files:**
   - `contents <- map_df(myfiles, ~my_read_xlsx(.x, sheet_n, ))`: Uses `map_df` from the `purrr` package to read each Excel file specified by the file paths in `myfiles`. For each file path `.x`, it calls the `my_read_xlsx` function to read the Excel file and extract data from the specified sheet (`sheet_n`). The resulting data frames are combined row-wise into a single data frame using `map_df`.

4. **Return Result:**
   - `return(contents)`: Returns a single data frame containing the combined data from all Excel files. Each row corresponds to a record from one of the Excel files.
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
Anna Shipunova
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{my_read_xlsx}}

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
