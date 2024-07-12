\name{prepare_csv_names}
\alias{prepare_csv_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare Csv Names
}
\description{
This function takes a vector of filenames and prepends the appropriate directory path based on the filenames' prefixes. It returns a list of file paths corresponding to the input filenames.}
\usage{
prepare_csv_names(filenames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames}{
%%     ~~Describe \code{filenames} here~~
}
}
\details{
Here's an explanation of the `prepare_csv_names` function:

1. **Function Definition:**
   - `prepare_csv_names <- function(filenames) \{ ... \}`: Defines a function named `prepare_csv_names` with one argument `filenames`, which is a vector containing the names of CSV files.

2. **Adding File Paths:**
   - `add_path_corresp <- "from_Fhier/Correspondence"`: Defines a variable `add_path_corresp` to hold the directory path for correspondence files.
   - `add_path_compl <- "from_Fhier/FHIER Compliance"`: Defines a variable `add_path_compl` to hold the directory path for FHIER compliance files.

3. **Creating File Paths:**
   - `my_list <- sapply(filenames, function(x) \{ ... \})`: Uses `sapply` to iterate over each filename in the `filenames` vector and apply a function.
   - Within the function:
     - `my_headers_case_function(x)`: Calls a function `my_headers_case_function` with each filename `x` as an argument. This function adjusts the case of the filename.
     - `startsWith(...)`: Checks if the adjusted filename starts with specific prefixes.
     - `file.path(...)`: Constructs a file path using the appropriate directory (`add_path_corresp` or `add_path_compl`) based on the filename's prefix.
     - `dplyr::case_when(...)`: Performs a conditional operation based on the filename's prefix, assigning the corresponding file path.

4. **Returning the Result:**
   - `return(paste(my_list) %>% as.list())`: Combines the resulting file paths into a single character vector using `paste`, then converts it to a list and returns it as the output of the function.
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
