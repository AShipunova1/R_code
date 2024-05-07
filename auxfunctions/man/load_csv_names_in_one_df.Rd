\name{load_csv_names_in_one_df}
\alias{load_csv_names_in_one_df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load_csv_names_in_one_df(path_to_files, csv_names_list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_to_files}{
%%     ~~Describe \code{path_to_files} here~~
}
  \item{csv_names_list}{
%%     ~~Describe \code{csv_names_list} here~~
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
function (path_to_files, csv_names_list) 
{
    myfiles <- csv_names_list
    if (length(path_to_files) > 0) {
        myfiles <- lapply(csv_names_list, function(x) file.path(path_to_files, 
            x))
    }
    csv_content <- purrr::map_df(myfiles, function(file_name) {
        readr::read_csv(file_name, col_types = cols(.default = "c"), 
            trim_ws = TRUE, na = c("", "NA", "NaN"), name_repair = "universal")
    })
    return(csv_content)
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
