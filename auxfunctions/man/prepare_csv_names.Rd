\name{prepare_csv_names}
\alias{prepare_csv_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prepare_csv_names(filenames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames}{
%%     ~~Describe \code{filenames} here~~
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
function (filenames) 
{
    add_path_corresp <- "from_Fhier/Correspondence"
    add_path_compl <- "from_Fhier/FHIER Compliance"
    my_list <- sapply(filenames, function(x) {
        dplyr::case_when(startsWith(my_headers_case_function(x), 
            "correspond") ~ file.path(add_path_corresp, x), startsWith(my_headers_case_function(x), 
            "fhier_compliance") ~ file.path(add_path_compl, x), 
            .default = file.path(add_path_compl, x))
    })
    return(paste(my_list) \%>\% as.list())
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
