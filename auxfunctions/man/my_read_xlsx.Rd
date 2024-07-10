\name{my_read_xlsx}
\alias{my_read_xlsx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read xlsx shortcut
}
\description{
This function reads data from an Excel file, optionally starting from a specified row, and applies header cleaning to ensure consistent formatting of column names.
}
\usage{
my_read_xlsx(file_path, sheet_n, start_row = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_path}{
%%     ~~Describe \code{file_path} here~~
}
  \item{sheet_n}{
%%     ~~Describe \code{sheet_n} here~~
}
  \item{start_row}{
%%     ~~Describe \code{start_row} here~~
}
}
\details{
1. **Function Definition:**
   - `my_read_xlsx <- function(file_path, sheet_n, start_row = 1) \{ ... \}`: Defines a function named `my_read_xlsx` with three arguments: `file_path` (the path to the Excel file), `sheet_n` (the index of the sheet to read), and `start_row` (the starting row from which to read data, with a default value of 1).

2. **Reading Excel File:**
   - `read.xlsx(file_path, sheet_n, startRow = start_row, detectDates = TRUE, colNames = TRUE, sep.names = "_")`: Uses the `read.xlsx` function from the `openxlsx` package to read data from the specified Excel file.
     - `file_path`: The path to the Excel file to be read.
     - `sheet_n`: The index of the sheet to read from.
     - `startRow`: The row number from which to start reading data.
     - `detectDates`: A logical value indicating whether to automatically detect and convert date-like columns to date format.
     - `colNames`: A logical value indicating whether the first row of the Excel sheet contains column names.
     - `sep.names`: A character used to separate column names with multiple parts (e.g., spaces or underscores).

3. **Cleaning Headers:**
   - `clean_headers(...)`: Applies the `clean_headers` function to the data frame read from the Excel file. This function likely performs tasks such as removing leading/trailing spaces, converting column names to lowercase, and replacing spaces with underscores.

4. **Return Result:**
   - `return(res_df)`: Returns the cleaned data frame obtained from reading the Excel file. This data frame contains the data read from the specified Excel sheet, with any necessary header cleaning applied.

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
\code{\link{clean_headers}}
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
