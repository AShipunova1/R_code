\name{load_csv_names}
\alias{load_csv_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load Csv Names
}
\description{
This function efficiently loads multiple CSV files specified by their file paths and names into a list of data frames, making it convenient to work with multiple datasets simultaneously.}
\usage{
load_csv_names(csv_files_paths, csv_names_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csv_files_paths}{
%%     ~~Describe \code{csv_files_paths} here~~
}
  \item{csv_names_list}{
%%     ~~Describe \code{csv_names_list} here~~
}
}
\details{
1. **Function Definition:**
   - `load_csv_names <- function(csv_files_paths, csv_names_list) { ... }`: Defines a function named `load_csv_names` with two arguments: `csv_files_paths` (the directory path where CSV files are located) and `csv_names_list` (a list of CSV file names).

2. **Constructing File Paths:**
   - `myfiles <- lapply(csv_names_list, function(x) file.path(csv_files_paths, x))`: Uses `lapply` to iterate over each file name in `csv_names_list`. For each file name `x`, it constructs the full file path by combining `csv_files_paths` with `x` using `file.path`. The resulting list of file paths is stored in `myfiles`.

3. **Printing File Paths:**
   - `print(myfiles)`: Prints the list of file paths to the console. This is useful for debugging or verifying the correctness of the constructed file paths.

4. **Reading CSV Files:**
   - `contents <- lapply(myfiles, read_csv, col_types = cols(.default = "c"))`: Uses `lapply` to iterate over each file path in `myfiles`. For each file path, it reads the corresponding CSV file using the `read_csv` function from the `readr` package. It specifies that all columns should be read as characters (`col_types = cols(.default = "c")`), which is useful when the column types are unknown or mixed.

5. **Return Result:**
   - `return(contents)`: Returns a list containing the contents of all CSV files. Each element of the list corresponds to a CSV file, and the contents are stored as data frames.
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
