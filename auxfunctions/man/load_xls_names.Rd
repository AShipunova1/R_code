\name{load_xls_names}
\alias{load_xls_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load_xls_names(my_paths, xls_names_list, sheet_n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_paths}{
%%     ~~Describe \code{my_paths} here~~
}
  \item{xls_names_list}{
%%     ~~Describe \code{xls_names_list} here~~
}
  \item{sheet_n}{
%%     ~~Describe \code{sheet_n} here~~
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
function (my_paths, xls_names_list, sheet_n = 1) 
{
    my_inputs <- my_paths$inputs
    myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, 
        x))
    contents <- map_df(myfiles, ~my_read_xlsx(.x, sheet_n, ))
    return(contents)
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
