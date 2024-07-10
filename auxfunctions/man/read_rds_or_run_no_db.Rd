\name{read_rds_or_run_no_db}
\alias{read_rds_or_run_no_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read an .rds file if it exists, or run a function
}
\description{
Overall, this function checks if a binary file exists at a specified path. If it does, it reads the file. If not, it executes a specified function using provided data frames, saves the result as a binary file, and returns the result.
}
\usage{
read_rds_or_run_no_db(my_file_path, my_data_list_of_dfs, my_function)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_file_path}{The file path to read from or write to.}
  \item{my_data_list_of_dfs}{A list containing the data frames needed for the function.}
  \item{my_function}{The function to be executed if the file doesn't exist.}
}
\details{
1. **Function Definition:**
   - `read_rds_or_run_no_db <- function(my_file_path, my_data_list_of_dfs, my_function) \{ ... \}`: Defines a function named `read_rds_or_run_no_db` with three arguments:
     - `my_file_path`: The file path to read from or write to.
     - `my_data_list_of_dfs`: A list containing the data frames needed for the function.
     - `my_function`: The function to be executed if the file doesn't exist.

2. **File Existence Check:**
   - `if (file.exists(my_file_path)) \{ ... \} else \{ ... \}`: Checks if the file specified by `my_file_path` exists.

3. **Read or Execute Function:**
   - If the file exists:
     - `my_df <- readr::read_rds(my_file_path)`: Reads the binary file specified by `my_file_path` using the `read_rds` function from the `readr` package and assigns it to `my_df`.
   - If the file doesn't exist:
     - `my_df <- my_function(my_data_list_of_dfs[[1]], my_data_list_of_dfs[[2]])`: Executes the function `my_function` using the first two data frames from `my_data_list_of_dfs` as arguments and assigns the result to `my_df`.

4. **Timing Execution:**
   - `tic("run the function")`: Starts timing the execution of the function.
   - `toc()`: Stops timing the execution of the function and prints the elapsed time.

5. **Write Binary File:**
   - `readr::write_rds(my_df, my_file_path)`: Writes the resulting data frame `my_df` as a binary file to the specified `my_file_path`.

6. **Return Result:**
   - `return(my_df)`: Returns the resulting data frame `my_df`.

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
