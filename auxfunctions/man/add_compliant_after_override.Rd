\name{add_compliant_after_override}
\alias{add_compliant_after_override}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a new column 'compliant_after_override' based on compliance and override status
}
\description{
Explanations:
1. Create a new variable 'res' to store the result.
2. Use 'rowwise' to perform operations row by row.
3. Use 'mutate' to create a new column 'compliant_after_override' based on conditions specified in 'case_when'.
   - If 'is_comp' is 0 and 'overridden' is 0, set 'compliant_after_override' to "no".
   - If 'is_comp' is 1 or 'overridden' is 1, set 'compliant_after_override' to "yes".
   - If 'is_comp' is NA, set 'compliant_after_override' to NA.
   - For all other cases, set 'compliant_after_override' to the string representation of 'is_comp'.
4. Use 'ungroup' to remove grouping from the data frame.
}
\usage{
add_compliant_after_override(my_compl_df, 
   overridden_col_name = "overridden", 
   compliance_col_name = "is_comp")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_compl_df}{
%%     ~~Describe \code{my_compl_df} here~~
}
  \item{overridden_col_name}{
%%     ~~Describe \code{overridden_col_name} here~~
}
  \item{compliance_col_name}{
%%     ~~Describe \code{compliance_col_name} here~~
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
Anna Shipunova
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
