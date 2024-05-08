\name{separate_permits_into_3_groups}
\alias{separate_permits_into_3_groups}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Separate Permits into 3 Groups
}
\description{
This function separates permit groups into three categories ("sa_only", "gom_only", "dual") based on specified patterns found in the permit group field.
}
\usage{
separate_permits_into_3_groups(my_df, permit_group_field_name = "permitgroup")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{permit_group_field_name}{
%%     ~~Describe \code{permit_group_field_name} here~~
}
}
\details{
1. **Function Definition:**
   - `separate_permits_into_3_groups <- function (my_df, permit_group_field_name = "permitgroup")`: Defines a function named `separate_permits_into_3_groups` that takes two arguments: `my_df`, which is the input data frame containing permit information, and `permit_group_field_name`, which is the name of the field containing permit group information. By default, it assumes the field name is "permitgroup".

2. **Mutating Permit Groups:**
   - `my_df %>% mutate(...)`: Uses the pipe operator to mutate the input data frame.
   - `permit_sa_gom = dplyr::case_when(...)`: Creates a new column named "permit_sa_gom" based on conditions defined in the `case_when` function.
   - `!grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only"`: Checks if the permit group does not contain certain patterns (e.g., "RCG", "HRCG", "CHG", "HCHG"). If true, assigns the value "sa_only" to the new column.
   - `!grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only"`: Checks if the permit group does not contain certain patterns (e.g., "CDW", "CHS", "SC"). If true, assigns the value "gom_only" to the new column.
   - `.default = "dual"`: If none of the above conditions are met, assigns the value "dual" to the new column.

3. **Return Statement:**
   - `%>% return()`: Uses the pipe operator to return the mutated data frame.
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
