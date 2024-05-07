\name{corresp_cleaning}
\alias{corresp_cleaning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corresp_cleaning(csvs_clean1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs_clean1}{
%%     ~~Describe \code{csvs_clean1} here~~
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
function (csvs_clean1) 
{
    corresp_arr <- csvs_clean1[[1]]
    corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
    createdon_field_name <- find_col_name(corresp_arr, "created", 
        "on")[1]
    contactdate_field_name <- find_col_name(corresp_arr, "contact", 
        "date")[1]
    corresp_arr_contact_cnts <- change_to_dates(corresp_arr_contact_cnts, 
        createdon_field_name)
    corresp_arr_contact_cnts <- change_to_dates(corresp_arr_contact_cnts, 
        contactdate_field_name)
    return(corresp_arr_contact_cnts)
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
