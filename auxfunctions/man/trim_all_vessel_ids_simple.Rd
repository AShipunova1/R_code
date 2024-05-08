\name{trim_all_vessel_ids_simple}
\alias{trim_all_vessel_ids_simple}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trim All Vessel IDs
}

\description{
This auxiliary function processes vessel ID columns in each data frame by trimming leading and trailing whitespace, making the data more consistent and easier to work with. If the column name is not provided, it attempts to detect it automatically.
}
\usage{
trim_all_vessel_ids_simple(csvs_clean_ws, col_name_to_trim = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs_clean_ws}{
%%     ~~Describe \code{csvs_clean_ws} here~~
}
  \item{col_name_to_trim}{
%%     ~~Describe \code{col_name_to_trim} here~~
}
}
\details{
1. **Function Definition:**
   - `trim_all_vessel_ids_simple <- function (csvs_clean_ws, col_name_to_trim = NA)`: Defines a function named `trim_all_vessel_ids_simple` that takes two arguments: `csvs_clean_ws`, a list of data frames to process, and `col_name_to_trim`, the name of the column containing vessel IDs to be trimmed. This argument is set to `NA` by default.

2. **Column Name Detection:**
   - `if (is.na(col_name_to_trim)) \{ ... \}`: Checks if the `col_name_to_trim` argument is not provided. If it's not provided, the function attempts to detect the column name containing vessel IDs.
   - `col_name_to_trim <- grep("vessel.*official.*number", tolower(names(x)), value = TRUE)`: Uses `grep` to search for column names containing variations of "vessel official number" (case-insensitive) within the column names of the first data frame (`x`) in the list. This is assigned to `col_name_to_trim`.

3. **Column Trimming:**
   - `col_name_to_trim_s <- rlang::sym(col_name_to_trim)`: Converts `col_name_to_trim` to a symbol using `rlang::sym`. Symbols are used to refer to columns in a non-standard evaluation context.
   - `x %>% dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s))`: Applies the `trimws` function to the column specified by `col_name_to_trim_s` within each data frame (`x`) in the list. This removes leading and trailing whitespace from the vessel IDs.

4. **Returning Processed Data Frames:**
   - `return(csvs_clean)`: Returns the list of data frames with trimmed vessel IDs.
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
Usually used from auxfunctions::clean_all_csvs()
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
