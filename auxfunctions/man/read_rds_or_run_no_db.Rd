\name{read_rds_or_run_no_db}
\alias{read_rds_or_run_no_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read_rds_or_run_no_db(my_file_path, my_data_list_of_dfs, my_function)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_file_path}{
%%     ~~Describe \code{my_file_path} here~~
}
  \item{my_data_list_of_dfs}{
%%     ~~Describe \code{my_data_list_of_dfs} here~~
}
  \item{my_function}{
%%     ~~Describe \code{my_function} here~~
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
function (my_file_path, my_data_list_of_dfs, my_function) 
{
    if (file.exists(my_file_path)) {
        my_df <- readr::read_rds(my_file_path)
    }
    else {
        tic("run the function")
        my_df <- my_function(my_data_list_of_dfs[[1]], my_data_list_of_dfs[[2]])
        toc()
        readr::write_rds(my_df, my_file_path)
    }
    return(my_df)
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
