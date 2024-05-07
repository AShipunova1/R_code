\name{concat_unique_sorted}
\alias{concat_unique_sorted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Concatenate unique sorted values
}
\description{
Define a function to concatenate unique values in a sorted manner.
This function takes a vector 'x', removes NA values, sorts the unique values,
and then concatenates them with a comma separator.
}
\usage{
concat_unique_sorted(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
This `concat_unique_sorted` function combines the unique non-NA values of a vector into a single string with the values sorted in ascending order and separated by commas.

1. **Input Parameter:**
   - `x`: Represents the input vector from which unique non-NA values will be extracted and concatenated.

2. **Extract Non-NA Values:**
   - `non_na_values <- x[!is.na(x)]`: This line extracts the non-NA values from the input vector `x` using logical indexing. It ensures that only non-missing values are considered for concatenation.

3. **Sort Unique Values:**
   - `sorted_unique <- unique(sort(non_na_values))`: Here, the non-NA values are sorted in ascending order and duplicates are removed using the `unique` and `sort` functions. This ensures that the resulting concatenated string will have unique values in sorted order.

4. **Concatenate Values:**
   - `result <- paste0(sorted_unique, collapse = ", ")`: The sorted unique values are concatenated into a single string using `paste0`, where the `collapse` argument specifies that the values should be separated by a comma followed by a space.

5. **Return Statement:**
   - `return(result)`: Finally, the concatenated string `result` is returned as the output of the function.

This function is useful for creating a concise representation of unique non-NA values from a vector, particularly when presenting or summarizing data.
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
function (x) 
{
    non_na_values <- x[!is.na(x)]
    sorted_unique <- unique(sort(non_na_values))
    result <- paste0(sorted_unique, collapse = ", ")
    return(result)
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
