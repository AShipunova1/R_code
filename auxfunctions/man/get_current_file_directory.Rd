\name{get_current_file_directory}
\alias{get_current_file_directory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the current file directory
}
\description{
Overall, this function provides a convenient way to programmatically obtain the directory path of the file being edited in RStudio.
}
\usage{
get_current_file_directory()
}
%- maybe also 'usage' for other objects documented here.
\details{
The `get_current_file_directory` function is designed to retrieve the directory of the currently opened file in RStudio.

1. **Function Definition:**
   - `get_current_file_directory <- function() { ... }`: Defines a function named `get_current_file_directory` with no arguments.

2. **Using RStudio API:**
   - `rstudioapi::getSourceEditorContext()`: Utilizes the `getSourceEditorContext` function from the RStudio API to access information about the current source editor context, including the path of the currently open file.

3. **Extracting Directory Name:**
   - `dirname()`: Takes the path obtained from `getSourceEditorContext()` and extracts the directory portion, removing the filename. This function returns the directory path of the currently open file.

4. **Return Result:**
   - The function returns the directory path where the currently open file is located.
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
Doesn't work everywhere, use 
this.path::this.dir()
instead.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
