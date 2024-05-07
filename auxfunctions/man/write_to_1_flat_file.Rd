\name{write_to_1_flat_file}
\alias{write_to_1_flat_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write_to_1_flat_file(flat_file_name, file_name_to_write)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flat_file_name}{
%%     ~~Describe \code{flat_file_name} here~~
}
  \item{file_name_to_write}{
%%     ~~Describe \code{file_name_to_write} here~~
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
function (flat_file_name, file_name_to_write) 
{
    sink(flat_file_name, append = TRUE)
    current_file_text <- readr::read_lines(file_name_to_write)
    cat("\n\n#### Current file:", basename(file_name_to_write), 
        "----\n\n")
    cat(current_file_text, sep = "\n")
    sink()
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
