\name{function_message_print}
\alias{function_message_print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a Function Message
}
\description{
Pretty message print in the console.

Provides a way to print messages with enhanced visual formatting, making them stand out for better readability or emphasis.
}
\usage{
function_message_print(text_msg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text_msg}{
%%     ~~Describe \code{text_msg} here~~
}
}
\details{
The `function_message_print` function is a utility function for printing messages with a cyan background color.

1. **Function Definition:**
   - `function_message_print <-function (text_msg) { ... }`: Defines a function named `function_message_print` that takes one argument, `text_msg`, representing the message to be printed.

2. **Printing Message with Cyan Background:**
   - `crayon::bgCyan$bold(text_msg)`: Formats the `text_msg` with a cyan background color and bold font using the `crayon` package. This creates a visually distinct message when printed.
   - `cat(..., sep = "\\n")`: Prints the formatted message to the console. `cat` is a function used for printing, and `sep = "\\n"` specifies that each message should be printed on a new line.
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
function (text_msg) 
{
    cat(crayon::bgCyan$bold(text_msg), sep = "\\n")
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
