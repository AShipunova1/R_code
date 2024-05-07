\name{compliance_cleaning}
\alias{compliance_cleaning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compliance_cleaning(compl_arr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{compl_arr}{
%%     ~~Describe \code{compl_arr} here~~
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
