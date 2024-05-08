\name{legend_for_grid_arrange}
\alias{legend_for_grid_arrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the legend only
}
\description{
This function is useful when you have multiple plots arranged in a grid and need to separately manipulate or display the legend. It allows for more flexible customization and arrangement of plot components.
}
\usage{
legend_for_grid_arrange(legend_plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{legend_plot}{
The plot containing the legend}
}
\details{
The `legend_for_grid_arrange` function is responsible for extracting a legend plot from a grid arrangement of plots.

1. **Function Definition:**
   - `legend_for_grid_arrange <- function(legend_plot) { ... }`: Defines a function named `legend_for_grid_arrange` with one argument, `legend_plot`, which represents the plot containing the legend.

2. **Extracting the Legend:**
   - `my_legend <- cowplot::get_legend(legend_plot)`: Uses the `get_legend` function from the `cowplot` package to extract the legend from the provided `legend_plot`. This function retrieves the legend associated with a plot.

3. **Return Result:**
   - The function returns the extracted legend plot stored in `my_legend`.
}
\value{
%%  ~Describe the value returned
The extracted legend plot stored in `my_legend`.
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
