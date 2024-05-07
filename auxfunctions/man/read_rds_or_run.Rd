\name{read_rds_or_run}
\alias{read_rds_or_run}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read_rds_or_run(my_file_path, my_data = as.data.frame(""), my_function, force_from_db = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_file_path}{
%%     ~~Describe \code{my_file_path} here~~
}
  \item{my_data}{
%%     ~~Describe \code{my_data} here~~
}
  \item{my_function}{
%%     ~~Describe \code{my_function} here~~
}
  \item{force_from_db}{
%%     ~~Describe \code{force_from_db} here~~
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
function (my_file_path, my_data = as.data.frame(""), my_function, 
    force_from_db = NULL) 
{
    if (file.exists(my_file_path)) {
        modif_time <- file.info(my_file_path)$mtime
    }
    if (file.exists(my_file_path) & is.null(force_from_db)) {
        function_message_print("File already exists, reading.")
        my_result <- readr::read_rds(my_file_path)
    }
    else {
        function_message_print(c("File", my_file_path, "doesn't exists, pulling data from database.", 
            "Must be on VPN."))
        msg_text <- paste(today(), "run for", basename(my_file_path))
        tictoc::tic(msg_text)
        my_result <- my_function(my_data)
        tictoc::toc()
        function_message_print(c("Saving new data into a file here: ", 
            my_file_path))
        try(readr::write_rds(my_result, my_file_path))
        modif_time <- date()
    }
    my_file_name <- basename(my_file_path)
    function_message_print(str_glue("File: {my_file_name} modified {modif_time}"))
    return(my_result)
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
