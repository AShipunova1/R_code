\name{find_col_name}
\alias{find_col_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a column name in a dataframe
}
\description{
This function provides a flexible way to search for column names within a dataframe based on specified starting and ending patterns. It is useful for identifying columns that match a particular naming convention or pattern.}
\usage{
find_col_name(mydf, start_part, end_part)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydf}{
  the dataframe to search
%%     ~~Describe \code{mydf} here~~
}
  \item{start_part}{
the starting pattern of the column names to search for
}
  \item{end_part}{
the ending pattern of the column names to search for
}
}
\details{
The function is designed to search for column names within a dataframe that contain a specific pattern defined by `start_part` and `end_part`.

1. **Function Definition:**
   -`function (mydf, start_part, end_part) { ... }`: Defines a function with three arguments: `mydf` (the dataframe to search), `start_part` (the starting pattern of the column names to search for), and `end_part` (the ending pattern of the column names to search for).

2. **Constructing Regular Expression:**
   - `to_search <- paste0(start_part, ".*", end_part)`: Constructs a regular expression pattern by concatenating `start_part`, ".*" (which matches any characters zero or more times), and `end_part`. This pattern will be used to search for matching column names.

3. **Searching for Matching Column Names:**
   - `matching_names <- grep(to_search, tolower(names(mydf)), value = TRUE)`: Searches for column names in the dataframe `mydf` that match the regular expression pattern `to_search`. The `tolower` function is used to convert the column names to lowercase for case-insensitive matching. The `grep` function returns the matching column names as a vector.

4. **Return Statement:**
   - `return(matching_names)`: Returns the vector of matching column names found in the dataframe that satisfy the specified pattern.
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
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (mydf, start_part, end_part) 
{
    to_search <- paste0(start_part, ".*", end_part)
    matching_names <- grep(to_search, tolower(names(mydf)), value = TRUE)
    return(matching_names)
  }
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
