\name{join_all_csvs}
\alias{join_all_csvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
%%  Anna Shipunova
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
function (corresp_arr, compl_arr) 
{
    corresp <- corresp_arr
    if (!is.data.frame(corresp_arr)) {
        corresp <- join_same_kind_csvs(corresp_arr)
    }
    compl <- compl_arr
    if (!is.data.frame(compl_arr)) {
        compl <- join_same_kind_csvs(compl_arr)
    }
    result_df <- compl \%>\% full_join(corresp, by = c("vesselofficialnumber"), 
        multiple = "all")
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
