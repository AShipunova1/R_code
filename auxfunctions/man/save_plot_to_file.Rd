\name{save_plot_to_file}
\alias{save_plot_to_file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Save a plot to a file
}
\description{
This function allows you to save a plot object to a specified file location with custom dimensions.
}
\usage{
save_plot_to_file(file_full_name, plot_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file_full_name}{
%%     ~~Describe \code{file_full_name} here~~
}
  \item{plot_name}{
%%     ~~Describe \code{plot_name} here~~
}
}
\details{
1. **Function Definition:**
   - `save_plot_to_file <- function(file_full_name, plot_name)`: Defines a function named `save_plot_to_file` that takes two arguments: `file_full_name`, which represents the full path and filename where the plot will be saved, and `plot_name`, which is the plot object to be saved.

2. **Saving the Plot:**
   - `ggplot2::ggsave(...)`: Uses the `ggsave` function from the ggplot2 package to save the plot to a file.
   - `file_full_name`: Specifies the full path and filename where the plot will be saved.
   - `plot_name`: Specifies the plot object to be saved.
   - `width = 30, height = 20, units = "cm"`: Specifies the dimensions of the saved plot. The width is set to 30 centimeters, the height to 20 centimeters, and the units are specified as centimeters.
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
