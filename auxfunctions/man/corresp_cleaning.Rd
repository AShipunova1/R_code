\name{corresp_cleaning}
\alias{corresp_cleaning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clean correspondence data downloaded from FHIER
}
\description{
This function prepares the correspondence data by adding contact frequency information and converting relevant columns to date format.
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
The `corresp_cleaning` function processes a list of cleaned CSV files related to correspondence data.

1. **Function Definition:**
   - `corresp_cleaning <- function(csvs_clean1) \{ .. \}`: Defines a function named `corresp_cleaning` that takes a single argument, `csvs_clean1`, which is assumed to be a list containing cleaned CSV files.

2. **Extracting Data:**
   - `corresp_arr <- csvs_clean1[[1]]`: Extracts the first element from the `csvs_clean1` list and assigns it to the `corresp_arr` variable.

3. **Adding Contact Counts:**
   - `corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)`: Calls the `add_count_contacts` function to add a new column (`contact_freq`) to the correspondence data frame, indicating the frequency of contacts associated with each vessel.

4. **Finding Column Names:**
   - `createdon_field_name <- find_col_name(corresp_arr, "created", "on")[1]`: Searches for the column name related to the creation date ("created on") within the correspondence data frame (`corresp_arr`) using the `find_col_name` function. It retrieves the first match and assigns it to `createdon_field_name`.
   - `contactdate_field_name <- find_col_name(corresp_arr, "contact", "date")[1]`: Searches for the column name related to the contact date ("contact date") within the correspondence data frame (`corresp_arr`). It retrieves the first match and assigns it to `contactdate_field_name`.

5. **Converting Columns to Dates:**
   - `corresp_arr_contact_cnts <- change_to_dates(corresp_arr_contact_cnts, createdon_field_name)`: Calls the `change_to_dates` function to convert the column identified by `createdon_field_name` to date format in the `corresp_arr_contact_cnts` data frame.
   - `corresp_arr_contact_cnts <- change_to_dates(corresp_arr_contact_cnts, contactdate_field_name)`: Calls the `change_to_dates` function again to convert the column identified by `contactdate_field_name` to date format in the `corresp_arr_contact_cnts` data frame.

6. **Return Statement:**
   - `return(corresp_arr_contact_cnts)`: Returns the processed correspondence data frame (`corresp_arr_contact_cnts`) as the output of the function.
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
This is one of the steps in auxfunctions::get_compl_and_corresp_data()
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
