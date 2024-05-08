\name{change_to_dates}
\alias{change_to_dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"

The change_to_dates function is defined to convert a specific column ('field_name') in the input data frame ('my_df') to POSIXct date format using the specified 'date_format'.

Inside the function, it uses the mutate function from the dplyr package to modify 'my_df'. The \{\{field_name\}\} syntax is used to refer to the column specified by 'field_name'.

It returns the 'result_df', which is the input data frame with the specified column converted to dates according to the specified 'date_format'.
}
\usage{
change_to_dates(my_df, field_name, date_format = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{field_name}{
%%     ~~Describe \code{field_name} here~~
}
  \item{date_format}{
%%     ~~Describe \code{date_format} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns the 'result_df', which is the input data frame with the specified column converted to dates according to the specified 'date_format'.
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
