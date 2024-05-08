\name{fix_names}
\alias{fix_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean names
}
\description{
Cleans column names of uploaded files.

Provides a comprehensive approach to cleaning and standardizing column names within a dataframe, making them more consistent and suitable for further analysis or processing.
}
\usage{
fix_names(x)

fix_names(end_port_county) %in% fix_names(fl_counties$SA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
The `fix_names` function is designed to clean and standardize column names within a dataframe.

1. **Function Definition:**
   - `fix_names <- function (x) { ... }`: Defines a function named `fix_names` that takes one argument, `x`, which represents the input dataframe.

2. **Cleaning Column Names:**
   - `x %>% str_replace_all("\\.", "")`: Removes all occurrences of periods (`.`) from the column names using `str_replace_all` from the `stringr` package.
   - `str_replace_all("[^A-z0-9]", "_")`: Replaces all non-alphanumeric characters (excluding underscores) with underscores (`_`). This step ensures that the column names only contain letters, numbers, and underscores.
   - `str_replace_all("^(_*)(.+)", "\\2\\1")`: Ensures that column names do not start with multiple underscores. It captures the leading underscores (if any) and the rest of the column name using regular expressions, then rearranges them to place the underscores at the end of the column name.
   - `my_headers_case_function()`: Applies a custom function `my_headers_case_function` to further standardize the case of the column names. This function is defined in auxfunctions and is responsible for ensuring consistent capitalization of column names.

3. **Return Statement:**
   - The function returns the modified dataframe with cleaned and standardized column names.
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
