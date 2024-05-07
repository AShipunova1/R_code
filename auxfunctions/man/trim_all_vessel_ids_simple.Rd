\name{trim_all_vessel_ids_simple}
\alias{trim_all_vessel_ids_simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
trim_all_vessel_ids_simple(csvs_clean_ws, col_name_to_trim = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs_clean_ws}{
%%     ~~Describe \code{csvs_clean_ws} here~~
}
  \item{col_name_to_trim}{
%%     ~~Describe \code{col_name_to_trim} here~~
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
function (csvs_clean_ws, col_name_to_trim = NA) 
{
    csvs_clean <- lapply(csvs_clean_ws, function(x) {
        if (is.na(col_name_to_trim)) {
            col_name_to_trim <- grep("vessel.*official.*number", 
                tolower(names(x)), value = TRUE)
        }
        col_name_to_trim_s <- rlang::sym(col_name_to_trim)
        x \%>\% dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) \%>\% 
            return()
    })
    return(csvs_clean)
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
