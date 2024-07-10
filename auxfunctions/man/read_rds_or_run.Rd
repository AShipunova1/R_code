\name{read_rds_or_run}
\alias{read_rds_or_run}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read an .rds file or run an SQL query
}
\description{
This function, `read_rds_or_run`, is designed to read data from an RDS file if it exists or run a specified function to obtain the data from the Oracle database and save it as an RDS file if the file does not exist or if the `force_from_db` parameter is set.
}
\usage{
read_rds_or_run(my_file_path, 
  my_data = as.data.frame(""), 
  my_function, 
  force_from_db = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{my_file_path}{The path to the RDS file to be read or saved.}

\item{my_data}{The data to be used with the function. Default is an empty data frame.}

\item{my_function}{The function to be run to obtain the data if necessary.}

\item{force_from_db}{A flag that, when set, will force the function to run the specified function instead of reading from the file, even if the file exists.}
}
\details{
1. **Check File Existence**: The function first checks if the file specified by `my_file_path` exists and, if so, retrieves its modification time.

2. **Read or Run**: Depending on the existence of the file and the `force_from_db` flag:
    - **File Exists and `force_from_db` is not set**: If the file exists and `force_from_db` is not set, the function reads the data from the RDS file using `readr::read_rds(my_file_path)` and assigns it to `my_result`.
    - **File Does Not Exist or `force_from_db` is set**: If the file does not exist or `force_from_db` is set, the function follows these steps:
        - Prints a message indicating the file doesn't exist and data will be pulled from the database.
        - Times the function execution using `tictoc::tic()` and starts with a message indicating the date and purpose of the run.
        - Runs the specified function (`my_function`) on the provided `my_data` to generate the result (`my_result`), e.g., downloading data from the Oracle database.
        - Stops timing the function execution using `tictoc::toc()`.
        - Saves the result as an RDS file to the specified `my_file_path` for future use using `readr::write_rds(my_result, my_file_path)`. A `try` block is used to handle potential errors in writing the file.
        - Prints a message indicating that the new data is being saved into a file.

3. **Print File Information**: After obtaining the data, the function prints the file name and modification time to provide information on when the data was last downloaded or modified.

4. **Return**: The function returns the generated or read data (`my_result`).

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
