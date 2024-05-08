\name{add_cnt_in_gr}
\alias{add_cnt_in_gr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add counts in a group
}
\description{
Explanations:
The function 'add_cnt_in_gr' performs the following operations:
1. Groups the data frame by the specified columns using `group_by_at(group_by_col)`. `group_by_col` is defined above.
2. Adds a new column named 'cnt_col_name' representing the count of distinct vessel official numbers in each group using `mutate(\{cnt_col_name\} := n_distinct(vessel_official_number))`.
The syntax `\{cnt_col_name\} :=` is used to create a new column dynamically with the name provided in the `cnt_col_name` argument.
3. Removes the grouping to return the data to its original structure with `ungroup()`.
4. Returns the modified data frame.
}
\usage{
add_cnt_in_gr(my_df, group_by_col, cnt_col_name = "total_vsl_m_by_year_perm")
}
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
a dataframe
}
  \item{group_by_col}{
%%     ~~Describe \code{group_by_col} here~~
a column name to group by
}
  \item{cnt_col_name}{
%%     ~~Describe \code{cnt_col_name} here~~
a new column name
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
