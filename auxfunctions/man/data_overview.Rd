\name{data_overview}
\alias{data_overview}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Overview
}
\description{
This function provides a quick way to get an overview of the data distribution and uniqueness within a dataframe.
}
\usage{
data_overview(my_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
}
\details{
The `data_overview` function provides an overview of the data contained within a dataframe.

1. **Function Definition:**
   - `data_overview <- function(my_df) \{ .. \}`: Defines a function named `data_overview` that takes a single argument `my_df`, representing the dataframe to be analyzed.

2. **Summary Statistics:**
   - `summary(my_df) \%>\% print()`: Generates summary statistics for the dataframe using the `summary` function. The `%>%` pipe operator is used to pass the summary to the `print` function for display.
   - `summary(my_df)`: Computes summary statistics such as minimum, 1st quartile, median, mean, 3rd quartile, and maximum for each column of the dataframe.

3. **Count Unique Values:**
   - `cat("\\nCount unique values in each column:\\n")`: Prints a message indicating that the unique values in each column will be counted.
   - `count_uniq_by_column(my_df)`: Calls the `count_uniq_by_column` function to count the number of unique values in each column of the dataframe. The result is printed to the console.
}
\value{
The function does not explicitly return any value. It prints summary statistics and counts of unique values for each column of the dataframe.
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
