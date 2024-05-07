\name{set_work_dir_local}
\alias{set_work_dir_local}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set_work_dir_local()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    setwd("~/")
    base_dir <- getwd()
    main_r_dir <- "R_files_local"
    in_dir <- "my_inputs"
    full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
    out_dir <- "my_outputs"
    full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)
    git_r_dir <- "R_code_github"
    full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)
    setwd(file.path(base_dir, main_r_dir))
    my_paths <- list(inputs = full_path_to_in_dir, outputs = full_path_to_out_dir, 
        git_r = full_path_to_r_git_dir)
    return(my_paths)
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
