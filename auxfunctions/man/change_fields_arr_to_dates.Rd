\name{change_fields_arr_to_dates}
\alias{change_fields_arr_to_dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change columns in array to POSIXct
}
\description{
The change_fields_arr_to_dates function is defined to convert multiple columns specified in 'field_names_arr' in the input data frame ('my_df') to POSIXct date format using the provided 'date_format'.

Inside the function, it uses the mutate function along with across from the dplyr package to target and modify the specified columns in 'field_names_arr'. The all_of(field_names_arr) ensures that all the columns listed in 'field_names_arr' are selected.

Within the across function, it applies the as.POSIXct function to each column ('x') in 'field_names_arr' using the provided 'date_format'. This step converts the values in these columns to POSIXct date format.

It returns the 'result_df', which is the input data frame with the specified columns converted to dates according to the specified 'date_format'.
}
\usage{
change_fields_arr_to_dates(my_df, field_names_arr, date_format)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{field_names_arr}{
%%     ~~Describe \code{field_names_arr} here~~
}
  \item{date_format}{
%%     ~~Describe \code{date_format} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns the 'result_df', which is the input data frame with the specified columns converted to dates according to the specified 'date_format'.}
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
