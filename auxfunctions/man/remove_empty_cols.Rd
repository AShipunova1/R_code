\name{remove_empty_cols}
\alias{remove_empty_cols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Empty Columns
}
\description{
This function effectively removes columns from the input data frame `my_df` that contain only missing values.
}
\usage{
remove_empty_cols(my_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
}
\details{
1. **Function Definition:**
   - `remove_empty_cols <- function (my_df)`: Defines a function named `remove_empty_cols` that takes a single argument `my_df`, which is expected to be a data frame.

2. **Inner Function Definition:**
   - `not_all_na <- function(x) any(!is.na(x))`: Defines an inner function named `not_all_na`. This function takes a vector `x` as input and returns `TRUE` if there is at least one non-missing value in the vector, otherwise it returns `FALSE`. This function will be used as a predicate to check if any column contains non-missing values.

3. **Selecting Columns:**
   - `select(my_df, where(not_all_na))`: Uses the `dplyr` function `select` to filter columns of `my_df` based on a condition. The condition is specified using the `where` function, which applies the `not_all_na` function to each column of `my_df`. Columns for which `not_all_na` returns `TRUE` (i.e., columns with at least one non-missing value) are retained, while columns with all missing values are removed.

4. **Returning Result:**
   - `\%>\% return()`: Pipes the result of the `select` operation into the `return` function, which ensures that the modified data frame is returned as the output of the `remove_empty_cols` function.
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
