\name{clean_names_and_addresses}
\alias{clean_names_and_addresses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cleans the strings in character columns
}
\description{
- This function `clean_names_and_addresses` cleans the strings in character columns of a dataframe.
- It uses `mutate` with `across` to apply multiple string cleaning operations to all character columns.
- The `str_squish` function is used to remove leading, trailing, and extra internal whitespace from each string.
- `replace_na` function replaces missing values with empty strings.
- The `str_replace_all` function is used to perform multiple replacements using regular expressions to clean up commas, semicolons, and unwanted substrings like "UN".
- Finally, it returns the cleaned dataframe.}
\usage{
clean_names_and_addresses(my_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the cleaned dataframe.}
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
function (my_df) 
{
    my_df_cleaned <- mutate(my_df, across(where(is.character), 
        ~str_squish(.x)), across(where(is.character), ~replace_na(.x, 
        "")), across(where(is.character), ~str_replace_all(.x, 
        ", ;", ";")), across(where(is.character), ~str_replace_all(.x, 
        "\\s+[,;]", ",")), across(where(is.character), ~str_replace_all(.x, 
        ";,+", ";")), across(where(is.character), ~str_replace_all(.x, 
        ";;+", ";")), across(where(is.character), ~str_replace_all(.x, 
        ",,+", ",")), across(where(is.character), ~str_replace_all(.x, 
        "[,;] *\\bUN\\b *", "")), across(where(is.character), 
        ~str_replace_all(.x, "\\bUN\\b", "")), across(where(is.character), 
        ~str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")), across(where(is.character), 
        ~str_replace_all(.x, "^[,;] ", "")), across(where(is.character), 
        ~str_replace_all(.x, "^[,;]$", "")), across(where(is.character), 
        ~str_replace_all(.x, "[,;]$", "")), across(where(is.character), 
        ~str_squish(.x)))
    return(my_df_cleaned)
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
