\name{title_message_print}
\alias{title_message_print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a Colored Message in Console
}
\description{
This function is designed to print a title message in blue color to the console. It enhances the visual presentation of the message for better readability.
}
\usage{
title_message_print(title_msg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title_msg}{
%%     ~~Describe \code{title_msg} here~~
}
}
\details{
1. **Function Definition:**
   - `title_message_print <- function (title_msg)`: Defines a function named `title_message_print` that takes one argument, `title_msg`, which represents the title message to be printed.

2. **Printing Title Message:**
   - `crayon::blue(title_msg)`: Formats the `title_msg` with blue color using the `crayon` package. This function enhances the visual appearance of the printed message.
   - `cat(..., sep = "\\n")`: Prints the formatted `title_msg` to the console. The `sep` argument specifies the separator between multiple messages, here set to a newline character (`"\\n"`).
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
function (title_msg)
{
    cat(crayon::blue(title_msg), sep = "\\n")
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
