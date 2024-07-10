\name{get_compl_by}
\alias{get_compl_by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Compliance by
}
\description{
This function is designed to organize compliance data by grouping it based on specified variables and spreading unique values into separate columns, providing a more structured and readable format for analysis.
}
\usage{
get_compl_by(my_df,
  group_by_for_compl = vars(-c("vessel_official_number",
   "compliant_",
   "overridden_")),
  names_from_list = c("vessel_official_number"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
The data frame containing compliance data.
}
  \item{group_by_for_compl}{
A list of column names to group the data by for compliance.
}
  \item{names_from_list}{
List of variable names for which unique values will be spread into columns.
}
}
\details{
The `get_compl_by` function aims to organize compliance data by grouping and pivoting it into a wider format.

1. **Function Definition:**
   - `get_compl_by <- function(my_df, group_by_for_compl = vars(-c("vessel_official_number", "compliant_", "overridden_")), names_from_list = c("vessel_official_number")) \{ .. \}`: Defines a function named `get_compl_by` that takes three arguments: `my_df` (the data frame containing compliance data), `group_by_for_compl` (variables to group the data by for compliance), and `names_from_list` (list of variable names for which unique values will be spread into columns).

2. **Grouping Data:**
   - `my_df %>% dplyr::group_by_at(group_by_for_compl)`: Groups the data frame `my_df` by the variables specified in `group_by_for_compl` using `group_by_at` from `dplyr`.

3. **Removing Duplicates:**
   - `unique()`: Removes duplicate rows from the grouped data frame, ensuring each combination of grouped variables appears only once.

4. **Pivoting Data:**
   - `tidyr::pivot_wider(names_from = all_of(names_from_list), values_from = c("compliant_", "overridden_"), values_fn = ~paste0(unique(sort(.x)), collapse = "_"))`: Transforms the data from long to wide format using `pivot_wider` from `tidyr`. It spreads unique values of variables specified in `names_from_list` into separate columns, with values from columns "compliant_" and "overridden_" concatenated into a single value.

5. **Ungrouping Data:**
   - `dplyr::ungroup()`: Ungroups the data frame, removing the grouping previously applied.

6. **Return Result:**
   - `return()`: Returns the processed data frame containing compliance information.
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
