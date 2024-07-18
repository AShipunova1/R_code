\name{prepare_csv_full_path}
\alias{prepare_csv_full_path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare Complaince and Correspondence CSVs Full Paths
}
\description{
This function constructs full file paths for a list of filenames by combining an input directory, a subdirectory, and the filenames. If `input_dir_part` is not provided, it uses a default directory set by `auxfunctions::set_work_dir()`. The resulting full paths are returned as a list.
}
\usage{
prepare_csv_full_path(filenames_list, add_path, [input_dir_part])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filenames_list}{
For example:
all_csv_names_list[2:3]
}
  \item{add_path}{
add_path represents the subdirectory within the input directory
  
For example:
  add_path <- "from_Fhier/Correspondence"
or
  add_path <- "from_Fhier/FHIER Compliance"

}
  \item{input_dir_part}{
A full path to the directory above the `add_path` (optional)
}

}
\details{
`prepare_csv_full_path <- function(filenames_list, add_path, input_dir_part = NA) { ... }` defines a function `prepare_csv_full_path` that takes a list of filenames (`filenames_list`), a subdirectory path (`add_path`), and an optional input directory part (`input_dir_part`).

\itemize{\item  `if (is.na(input_dir_part)) { ... }` checks if `input_dir_part` is `NA`:

  \itemize{\item `is.na(input_dir_part)` checks if `input_dir_part` is `NA`.

  \item If true, it executes the code block inside the `if` statement:

    \itemize{\item `my_paths <- auxfunctions::set_work_dir()` sets the working directory using the `set_work_dir` function from the `auxfunctions` package and assigns the result to `my_paths`.

    \item `input_dir_part <- my_paths$inputs` assigns the `inputs` element of `my_paths` to `input_dir_part`.}
    }}

\itemize{
\item `my_list <- sapply(filenames_list, function(x) { ... })` uses `sapply` to apply a function to each element in `filenames_list`:

  \itemize{\item `sapply(filenames_list, function(x) { file.path(input_dir_part, add_path, x) })` constructs full file paths by combining `input_dir_part`, `add_path`, and each filename `x` using `file.path`.}

\item  `return(paste(my_list) |> as.list())` converts the resulting list of paths to a character vector and then to a list, and returns it:

    \itemize{\item `paste(my_list)` converts the elements of `my_list` to a single character string.

  \item `|> as.list()` converts the character string to a list using the pipe operator (`|>`).
  }
}
}
\value{
A list of full paths.
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
\examples{
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
