\name{get_compl_and_corresp_data}
\alias{get_compl_and_corresp_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_compl_and_corresp_data(my_paths, filenames = csv_names_list_22_23, vessel_id_field_name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_paths}{
%%     ~~Describe \code{my_paths} here~~
}
  \item{filenames}{
%%     ~~Describe \code{filenames} here~~
}
  \item{vessel_id_field_name}{
%%     ~~Describe \code{vessel_id_field_name} here~~
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

csv_names_list_22_23 = c("Correspondence.csv",
                         "FHIER_Compliance_22.csv",
                         "FHIER_Compliance_23.csv")

## The function is currently defined as
function (my_paths, filenames = csv_names_list_22_23, vessel_id_field_name = NA) 
{
    csv_names_list <- prepare_csv_names(filenames)
    csv_contents <- load_csv_names(my_paths, csv_names_list)
    csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name)
    corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)
    compl_arr <- csvs_clean1[2:length(csvs_clean1)]
    compl_clean <- compliance_cleaning(compl_arr)
    return(list(compl_clean, corresp_arr_contact_cnts_clean))
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