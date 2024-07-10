\name{see_res_in_outfile}
\alias{see_res_in_outfile}

\title{Show the Long Text in an Outfile}

\usage{
see_res_in_outfile(var_to_output, extension = ".txt")
}

\arguments{
\item{var_to_output}{string or dataframe}

\item{extension}{file extension (string), e.g. ".csv", default = ".txt"}
}

\description{
This function provides a convenient way to visualize text data by saving it to a temporary text file and automatically opening it for viewing. It is particularly useful for displaying large or complex text outputs that may be difficult to analyze directly in the console.
}

\details{
- `see_res_in_outfile <- function(var_to_output, extension = ".txt")` defines the function `see_res_in_outfile` with two parameters: `var_to_output` (the variable to be outputted) and `extension` (the file extension for the temporary file, defaulting to ".txt").

- `outfile <- tempfile(fileext = extension)` creates a temporary file with the specified extension and assigns its path to the variable `outfile`.

- `if (extension == ".csv") { ... } else { ... }` checks if the specified extension is ".csv".

  - If `extension` is ".csv":
  
    - `readr::write_csv(var_to_output, file = outfile)` writes the content of `var_to_output` to the temporary CSV file using the `write_csv` function from the `readr` package.
    
  - If `extension` is not ".csv":
  
    - `sink(file = outfile)` redirects R output to the file specified by `outfile`, meaning subsequent output will be written to this file.
    
    - `print(var_to_output, na.print = "", n = nrow(var_to_output))` writes the content of `var_to_output` to the temporary file. The `na.print = ""` argument specifies that NA values should be printed as an empty string, and `n = nrow(var_to_output)` ensures all rows of the data frame are printed.
    
    - `sink()` restores the normal R output to the console, stopping the redirection of output to the temporary file.

- `file.show(outfile)` opens the temporary file in the default text editor for viewing.}

\value{
Instead of an output shows a temp file.
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
