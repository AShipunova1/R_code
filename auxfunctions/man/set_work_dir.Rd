\name{set_work_dir}
\alias{set_work_dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set Anna's work directories
}
\description{
This function sets the working directory to the user's home directory, defines directory paths for inputs, outputs, and GitHub R code, changes the working directory to the main R directory, and returns these paths as a list.

It assumes a specific directory structure, where data are separated from the code, so the latter can be stored on GitHub.

}
\usage{
set_work_dir()
}
%- maybe also 'usage' for other objects documented here.
\details{
1. **Function Definition:**

   - `set_work_dir <- function ()`: Defines a function named `set_work_dir` that takes no arguments.

2. **Setting Working Directory:**

   - `setwd("~/")`: Sets the working directory to the user's home directory.

   - `base_dir <- getwd()`: Retrieves the current working directory and assigns it to the variable `base_dir`.

3. **Defining Directory Paths:**

   - `main_r_dir <- "R_files_local"`: Specifies the directory name for local R files.

   - `in_dir <- "my_inputs"`: Specifies the directory name for input files.

   - `full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)`: Combines the base directory, main R directory, and input directory to create the full path to the input directory.

   - `out_dir <- "my_outputs"`: Specifies the directory name for output files.

   - `full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)`: Combines the base directory, main R directory, and output directory to create the full path to the output directory.

   - `git_r_dir <- "R_code_github"`: Specifies the directory name for the GitHub repository containing R code.

   - `full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)`: Combines the base directory and GitHub R directory to create the full path to the GitHub R directory.

4. **Creating Paths List:**

   - `my_paths <- list(...)`: Creates a list named `my_paths` containing the paths to the input directory, output directory, and GitHub R directory.

5. **Returning Paths List:**

   - `return(my_paths)`: Returns the `my_paths` list containing the paths to the input directory, output directory, and GitHub R directory.

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
