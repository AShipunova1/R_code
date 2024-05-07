\name{get_username}
\alias{get_username}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Username
}
\description{
This function provides a simple way to obtain the username of the user executing the R code, which can be useful for personalization, authentication, or logging purposes.}
\usage{
get_username()
}
%- maybe also 'usage' for other objects documented here.
\details{
The `get_username` function is designed to retrieve the username of the current user.

1. **Function Definition:**
   - `get_username <- function() { ... }`: Defines a function named `get_username` with no arguments.

2. **Using System Information:**
   - `Sys.info()`: Calls the `Sys.info` function, which provides various system information about the current R session.
   - `Sys.info()["user"]`: Retrieves the username of the current user from the system information.

3. **Converting to Character:**
   - `as.character()`: Converts the username to a character vector. This step ensures consistency in the data type of the returned value.

4. **Return Result:**
   - The function returns the username of the current user as a character string.
}
\value{
%%  ~Describe the value returned
The username of the current user as a character string.
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
function () 
{
    return(as.character(Sys.info()["user"]))
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
