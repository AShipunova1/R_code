\name{create_dir_if_not}
\alias{create_dir_if_not}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a directory if it doesn't exist
}
\description{
The `create_dir_if_not` function ensures the existence of a directory by creating it if it does not already exist.

This function provides a convenient way to ensure that a directory exists before performing operations such as writing files or storing data.
}
\usage{
create_dir_if_not(curr_dir_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{curr_dir_name}{
%%     ~~Describe \code{curr_dir_name} here~~
}
}
\details{
1. **Function Definition:**
   - `create_dir_if_not <- function(curr_dir_name) { ... }`: Defines a function named `create_dir_if_not` that takes a single argument, `curr_dir_name`, representing the name of the directory to be created.

2. **Checking Directory Existence:**
   - `if (!dir.exists(curr_dir_name)) { ... }`: Checks if the directory specified by `curr_dir_name` does not exist.
   - `dir.exists(curr_dir_name)`: Checks whether the directory specified by `curr_dir_name` exists. The `!` negates the result, so the code block inside the `if` statement executes only if the directory does not exist.

3. **Creating Directory:**
   - `dir.create(curr_dir_name)`: If the directory does not exist, this line creates the directory specified by `curr_dir_name` using the `dir.create` function.

4. **Return Statement:**
   - The function does not have a specific return value. It either creates the directory if it does not exist or takes no action if the directory already exists.
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
function (curr_dir_name) 
{
    if (!dir.exists(curr_dir_name)) {
        dir.create(curr_dir_name)
    }
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
