\name{compl__back_to_longer_format}
\alias{compl__back_to_longer_format}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Return compliance dataframe back to longer format (auxiliary)
}
\description{
Explanations:
The function 'compl__back_to_longer_format' performs the following operations:
1. Turns the data frame back to a longer format with vessel IDs in one column.
2. Specifies the columns to pivot. All columns except those specified in 'cols_names' are treated as vessel IDs.
3. Sets the values to the column 'is_compl_or_both'.
4. Sets the names to the column 'vessel_official_number'.
5. Returns the modified data frame.
}
\usage{
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
  \item{cols_names}{
%%     ~~Describe \code{cols_names} here~~
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
Usage example:
cols_names are all names except vessel_official_numbers

cols_names <-
  c("year",
    "permit_sa_gom_dual",
    "total_vsl_y_by_year_perm",
    "year_permit_sa_gom_dual"
    )

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both <-
  compl__back_to_longer_format(
    compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both,
    cols_names
  )
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
