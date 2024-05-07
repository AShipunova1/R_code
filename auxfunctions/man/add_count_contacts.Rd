\name{add_count_contacts}
\alias{add_count_contacts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_count_contacts(all_data_df_clean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all_data_df_clean}{
%%     ~~Describe \code{all_data_df_clean} here~~
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
%%  ~~who you are~~
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
function (all_data_df_clean) 
{
    contactdate_field_name <- find_col_name(all_data_df_clean, 
        "contact", "date")[1]
    vessel_id_field_name <- find_col_name(all_data_df_clean, 
        "vessel", "number")[1]
    result_df <- all_data_df_clean \%>\% dplyr::mutate(was_contacted = dplyr::if_else(is.na(contactdate_field_name), 
        "no", "yes")) \%>\% dplyr::add_count(!!dplyr::sym(vessel_id_field_name), 
        was_contacted, name = "contact_freq")
    return(result_df)
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
