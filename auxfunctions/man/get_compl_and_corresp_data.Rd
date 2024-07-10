\name{get_compl_and_corresp_data}
\alias{get_compl_and_corresp_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get compliance and correspondence data
}
\description{
This function facilitates the extraction, cleaning, and organization of compliance and correspondence data from files, downloaded from FHIER, providing a convenient way to preprocess the data for further analysis or visualization.
}
\usage{
get_compl_and_corresp_data(my_paths, 
  filenames = csv_names_list_22_23,
  vessel_id_field_name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_paths}{
Paths to the files.
}
  \item{filenames}{
A list of filenames.
}
  \item{vessel_id_field_name}{
A column name for vessel id, like "vessel_official_number"
}
}
\details{
The `get_compl_and_corresp_data` function is responsible for processing and cleaning compliance and correspondence data from CSV files.

It uses other functions from this package.

1. **Function Definition:**
   - `get_compl_and_corresp_data <- function(my_paths, filenames = csv_names_list_22_23, vessel_id_field_name = NA) \{ .. \}`: Defines a function named `get_compl_and_corresp_data` that takes three arguments: `my_paths` (paths to the CSV files), `filenames` (list of CSV filenames), and `vessel_id_field_name` (name of the vessel ID field).

2. **Prepare CSV Names:**
   - `csv_names_list <- prepare_csv_names(filenames)`: Prepares a list of CSV filenames based on the input list `filenames` using the `prepare_csv_names` function.

3. **Load CSV Contents:**
   - `csv_contents <- load_csv_names(my_paths, csv_names_list)`: Loads the contents of CSV files specified by `my_paths` and `csv_names_list` using the `load_csv_names` function.

4. **Clean CSVs:**
   - `csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name)`: Cleans all CSV files in `csv_contents` using the `clean_all_csvs` function, with an optional argument `vessel_id_field_name` to specify the vessel ID field.

5. **Clean Correspondence Data:**
   - `corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)`: Cleans the correspondence data in `csvs_clean1` using the `corresp_cleaning` function.

6. **Extract Compliance Data:**
   - `compl_arr <- csvs_clean1[2:length(csvs_clean1)]`: Extracts compliance data from `csvs_clean1` by excluding the first element (assumed to be correspondence data).

7. **Clean Compliance Data:**
   - `compl_clean <- compliance_cleaning(compl_arr)`: Cleans the compliance data in `compl_arr` using the `compliance_cleaning` function.

8. **Return Result:**
   - `return(list(compl_clean, corresp_arr_contact_cnts_clean))`: Returns a list containing the cleaned compliance data (`compl_clean`) and cleaned correspondence data (`corresp_arr_contact_cnts_clean`).
}
\value{
\item{compl_clean }{Cleaned compliance data}
\item{corresp_arr_contact_cnts_clean }{Cleaned correspondence data}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Anna Shipunova
}
\note{
Example: 
csv_names_list_22_23 = c("Correspondence.csv",
                         "FHIER_Compliance_22.csv",
                         "FHIER_Compliance_23.csv")

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% \code{\link[auxfunctions]{prepare_csv_names}}
\code{\link{prepare_csv_names}},

\code{\link{load_csv_names}},

\code{\link{clean_all_csvs}},

\code{\link{corresp_cleaning}},

\code{\link{compliance_cleaning}}
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
