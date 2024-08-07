\name{clean_weeks}
\alias{clean_weeks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spleet the compliance week column
}
\description{
Split week column ("52: 12/26/2022 - 01/01/2023") into 3 columns with proper classes, week_num (week order number), week_start and week_end.
}
\usage{
clean_weeks(my_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
}
\details{
- The `clean_weeks` function is designed to clean up the "week" column in a compliance dataframe.

- It first separates the "week" column into two separate columns using ":" as the delimiter, resulting in columns "week_num" and "week_rest".

- Then, it further separates the "week_rest" column into "week_start" and "week_end" columns using " - " as the delimiter.

- The function then converts the "week_num" column to integers and the "week_start" and "week_end" columns to Date objects using `as.integer` and `as.Date` functions, respectively.

- Finally, it returns the modified dataframe.}
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
