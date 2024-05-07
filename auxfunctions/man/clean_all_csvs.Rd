\name{clean_all_csvs}
\alias{clean_all_csvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clean_all_csvs(csvs, vessel_id_field_name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs}{
%%     ~~Describe \code{csvs} here~~
}
  \item{vessel_id_field_name}{
%%     ~~Describe \code{vessel_id_field_name} here~~
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
function (csvs, vessel_id_field_name = NA) 
{
    csvs_clean0 <- lapply(csvs, clean_headers)
    csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
    return(csvs_clean1)
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
