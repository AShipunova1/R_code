\name{join_all_csvs}
\alias{join_all_csvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Join Compliance and Correspondence Datasets.
}
\description{
Combines correspondence and compliance information into one dataframe by "vesselofficialnumber" only. Not by time!

The join_all_csvs function is defined to perform a full join operation on two data frames: 'corresp_arr' and 'compl_arr'. It handles cases where these parameters might be lists of data frames or individual data frames.
}
\usage{
join_all_csvs(corresp_arr, compl_arr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corresp_arr}{
%%     ~~Describe \code{corresp_arr} here~~
}
  \item{compl_arr}{
%%     ~~Describe \code{compl_arr} here~~
}
}
\details{
The `join_all_csvs` function is designed to join multiple data frames containing correspondence and compliance data into a single data frame.

1. **Function Definition:**
   - `join_all_csvs <- function(corresp_arr, compl_arr) \{ .. \}`: Defines a function named `join_all_csvs` with two arguments: `corresp_arr` for the correspondence data and `compl_arr` for the compliance data.

2. **Data Frame Initialization:**
   - `corresp <- corresp_arr`: Initializes a variable `corresp` to hold the correspondence data. If `corresp_arr` is not already a data frame, it is converted to one using the `join_same_kind_csvs` function.
   - `compl <- compl_arr`: Initializes a variable `compl` to hold the compliance data. Similar to `corresp`, if `compl_arr` is not already a data frame, it is converted to one using the `join_same_kind_csvs` function.

3. **Joining Data Frames:**
   - `result_df <- compl %>% full_join(corresp, by = c("vesselofficialnumber"), multiple = "all")`: Performs a full join between the compliance data (`compl`) and the correspondence data (`corresp`) based on the common column `vesselofficialnumber`. The argument `multiple = "all"` ensures that all matching rows are retained.

4. **Return Result:**
   - The function returns the resulting data frame after the join operation.
}
\value{
It returns the resulting data frame ('result_df') containing the merged data from 'compl' and 'corresp' data frames.
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
