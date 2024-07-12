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
1. **Initialization of Variables:**
   - `compl <- compl_arr`: This line initializes a variable `compl` with the same value as `compl_arr`, which represents a list of compliance data frames.

2. **Data Cleaning with `map`:**
   - `compl_clean <- map(compl, clean_weeks)`: Here, each data frame in the list `compl` undergoes cleaning using the `clean_weeks` function. The `map` function applies `clean_weeks` to each element of `compl`, returning a list of cleaned data frames stored in `compl_clean`.

3. **Column Name Extraction with `map`:**
   - `permitgroupexpirations <- map(compl, ...`: This line iterates over each data frame in `compl` using `map`.
   - `\(x) \{ .. \}`: An anonymous function is defined to operate on each data frame `x`.
   - Within the anonymous function:
      - `grep("permit.*group.*expiration", tolower(names(x)), value = TRUE)`: This function call searches for column names containing the pattern "permit", "group", and "expiration" (case-insensitive) within the names of the current data frame `x`. It returns the matching column names.

4. **Data Transformation with `imap`:**
   - `compl_dates <- compl_clean |>`: The cleaned data frames in `compl_clean` are processed further using the pipe (`|>`) operator.
   - `imap(\(x, idx) \{ .. \}`: The `imap` function iterates over each element of `compl_clean`, providing both the element (`x`, a data frame) and its index (`idx`).
   - Within the `imap` function:
      - `field_name <- permitgroupexpirations[[idx]]`: The variable `field_name` is assigned the column name extracted earlier, corresponding to the current data frame `x`.
      - `mutate(\{\{field_name\}\} := ...)`: This line adds a new column to the data frame `x`, where the column name is dynamically determined by `field_name`.
      - `as.POSIXct(pull(x[field_name]), format = "%m/%d/%Y")`: The values of the selected column are converted to POSIXct date-time format using the specified date format.

5. **Return Statement:**
   - `return(compl_dates)`: Finally, the function returns a list of data frames (`compl_dates`), where each data frame has undergone the necessary cleaning and transformation steps.

}
\value{
The list of dataframes (`compl_dates`)
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
