\name{change_to_dates}
\alias{change_to_dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
%%  ~~who you are~~
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
function (my_df, field_name, date_format = "") 
{
    if (date_format == "") {
        my_tryFormats = c("\%m/\%d/\%Y \%I:\%M\%p", "\%m/\%d/\%Y \%I:\%M \%p", 
            "\%m/\%d/\%Y \%R\%OS", "\%Y-\%m-\%d \%H:\%M:\%OS", "\%Y/\%m/\%d \%H:\%M:\%OS", 
            "\%Y-\%m-\%d \%H:\%M", "\%Y/\%m/\%d \%H:\%M", "\%Y-\%m-\%d", "\%Y/\%m/\%d")
    }
    new_field_name <- str_glue("{field_name}_dttm")
    result_df <- mutate(my_df, `:=`(!!new_field_name, as.POSIXct(!!field_name, 
        tryFormats = my_tryFormats, format = date_format)))
    return(result_df)
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
