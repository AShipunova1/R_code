\name{clean_names_and_addresses}
\alias{clean_names_and_addresses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
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
