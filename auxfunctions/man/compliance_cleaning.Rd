\name{compliance_cleaning}
\alias{compliance_cleaning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compliance data cleaning
}
\description{
This function takes a list of data frames, cleans each data frame by extracting specific field names, and converts the corresponding columns to POSIXct date format.

}
\usage{
compliance_cleaning(compl_arr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{compl_arr}{
  A list of data frames with compliance information downloaded from FHIER.
}
}
\details{
1. **Function Definition**: 
   - `compliance_cleaning <- function(compl_arr) {`: Defines a function named `compliance_cleaning` that takes one argument `compl_arr`.

2. **Variable Assignment**:
   - `compl <- compl_arr`: Assigns the value of `compl_arr` to a new variable `compl`.

3. **Map Function**:
   - `compl_clean <- map(compl, clean_weeks)`: Applies the `clean_weeks` function to each element of `compl` using the `map` function from the `purrr` package.

4. **Mapping to Extract Specific Field Names**:
   - `permitgroupexpirations <- map(compl, \(x) { ... })`: Maps over each element of `compl` and extracts specific field names that contain "permit", "group", and "expiration".

5. **Regular Expression Search**:
   - `grep("permit.*group.*expiration", ... , value = TRUE)`: Searches for field names containing "permit", "group", and "expiration" using a regular expression. It returns the matched field names as a vector.

6. **Mapping with Index**:
   - `imap(\(x, idx) { ... })`: Maps over each element of `compl_clean` along with its corresponding index.

7. **Accessing Field Names**:
   - `field_name <- permitgroupexpirations[[idx]]`: Retrieves the field name corresponding to the current index from `permitgroupexpirations`.

8. **Data Transformation**:
   - `x |> mutate(...)`:
     - Pipes the data frame `x` into the `mutate` function.
     - Adds a new column to `x` with the name specified by `field_name`.
     - Converts the values of the selected column to POSIXct date format using `as.POSIXct`.
     - The `format` argument specifies the format of the date strings ("%m/%d/%Y").

9. **Return Statement**:
   - `return(compl_dates)`: Returns the modified list of data frames containing cleaned and transformed dates.

}
\value{
The list of dataframes (`compl_dates`)
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
function (compl_arr) 
{
    compl <- compl_arr
    compl_clean <- map(compl, clean_weeks)
    permitgroupexpirations <- map(compl, function(x) {
        grep("permit.*group.*expiration", tolower(names(x)), 
            value = TRUE)
    })
    compl_dates <- imap(compl_clean, function(x, idx) {
        field_name <- permitgroupexpirations[[idx]]
        mutate(x, `:=`({
            {
                field_name
            }
        }, as.POSIXct(pull(x[field_name]), format = "\%m/\%d/\%Y")))
    })
    return(compl_dates)
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
