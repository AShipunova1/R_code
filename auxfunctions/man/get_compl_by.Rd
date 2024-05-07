\name{get_compl_by}
\alias{get_compl_by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_compl_by(my_df, group_by_for_compl = vars(-c("vessel_official_number", "compliant_", "overridden_")), names_from_list = c("vessel_official_number"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{group_by_for_compl}{
%%     ~~Describe \code{group_by_for_compl} here~~
}
  \item{names_from_list}{
%%     ~~Describe \code{names_from_list} here~~
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
function (my_df, group_by_for_compl = vars(-c("vessel_official_number", 
    "compliant_", "overridden_")), names_from_list = c("vessel_official_number")) 
{
    browser()
    my_df \%>\% dplyr::group_by_at(group_by_for_compl) \%>\% unique() \%>\% 
        tidyr::pivot_wider(names_from = all_of(names_from_list), 
            values_from = c("compliant_", "overridden_"), values_fn = ~paste0(unique(sort(.x)), 
                collapse = "_")) \%>\% dplyr::ungroup() \%>\% return()
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
