\name{join_same_kind_csvs}
\alias{join_same_kind_csvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Join the same kind csvs}
\description{
This function is helpful when you have multiple data frames with the same structure (i.e., same column names and types) and you want to combine them into a single data frame for analysis or further processing.
}
\usage{
join_same_kind_csvs(csvs_list_2_plus)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs_list_2_plus}{
%%     ~~Describe \code{csvs_list_2_plus} here~~
}
}
\details{
The `join_same_kind_csvs` function is designed to bind together multiple data frames into a single data frame.

1. **Function Definition:**
   - `join_same_kind_csvs <- function(csvs_list_2_plus) \{ .. \}`: Defines a function named `join_same_kind_csvs` with one argument, `csvs_list_2_plus`, which is a list containing two or more data frames.

2. **Binding Data Frames:**
   - `result_df <- dplyr::bind_rows(csvs_list_2_plus)`: Uses `bind_rows` from the `dplyr` package to stack the data frames vertically (row-wise). This function concatenates the data frames in the list `csvs_list_2_plus` into a single data frame, aligning columns by name.

3. **Return Result:**
   - The function returns the resulting data frame `result_df`.
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
Mostly used inside another functions.
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
