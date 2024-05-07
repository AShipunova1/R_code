\name{my_tee}
\alias{my_tee}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my_tee(my_text, my_title = NA, stat_log_file_path = NA, date_range = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_text}{
%%     ~~Describe \code{my_text} here~~
}
  \item{my_title}{
%%     ~~Describe \code{my_title} here~~
}
  \item{stat_log_file_path}{
%%     ~~Describe \code{stat_log_file_path} here~~
}
  \item{date_range}{
%%     ~~Describe \code{date_range} here~~
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
function (my_text, my_title = NA, stat_log_file_path = NA, date_range = NA) 
{
    the_end = "---"
    if (is.na(date_range)) 
        date_range = 2022
    title_message_print(my_title)
    cat(c(my_text, the_end), sep = "\n")
    if (is.na(stat_log_file_path)) {
        stat_log_file_path <- file.path(Path, Outputs, str_glue("{my_title}_{date_range}_run_{today()}.log"))
    }
    cat(c(my_title, my_text, the_end), file = stat_log_file_path, 
        sep = "\n", append = TRUE)
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
