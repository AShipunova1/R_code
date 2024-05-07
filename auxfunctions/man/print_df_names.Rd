\name{print_df_names}
\alias{print_df_names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print DF names to console
}
\description{
This function prints the comma separated column names of a dataframe, with an option to limit the number of names displayed. It's useful for quickly inspecting the structure of a dataframe, especially when dealing with datasets with a large number of columns.
}
\usage{
print_df_names(my_df, names_num = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{names_num}{
%%     ~~Describe \code{names_num} here~~
}
}
\details{
1. **Function Definition:**
   - `print_df_names <- function (my_df, names_num = 100) { ... }`: Defines a function named `print_df_names` with two arguments: `my_df` (the dataframe whose column names will be printed) and `names_num` (the maximum number of names to display, defaulted to 100).

2. **Extracting Column Names:**
   - `names(my_df)`: Retrieves the column names of the input dataframe `my_df`.

3. **Selecting Subset of Names:**
   - `%>% head(names_num)`: Uses the pipe operator (`%>%`) to pass the column names to the `head` function, which selects the first `names_num` names. This is useful when the dataframe has a large number of columns, and we want to limit the display to a manageable number.

4. **Formatting as a String:**
   - `%>% paste0(collapse = ", ")`: Concatenates the selected column names into a single string, separated by commas. This creates a more readable output.

5. **Returning the Result:**
   - `%>% return()`: Returns the concatenated string of column names as the output of the function.
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
function (my_df, names_num = 100) 
{
    names(my_df) \%>\% head(names_num) \%>\% paste0(collapse = ", ") \%>\% 
        return()
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
