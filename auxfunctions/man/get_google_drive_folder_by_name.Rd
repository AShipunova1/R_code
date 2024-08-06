\name{get_google_drive_folder_by_name}
\alias{get_google_drive_folder_by_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a folder in Google Drive with a name matching the provided pattern.
}
\description{
Returns the information about the found Google Drive folder.
}
\usage{
get_google_drive_folder_by_name(google_drive_project_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{google_drive_project_name}{
For example, "Egregious violators"
}
}
\details{
Explanations:

- `googledrive::drive_find(pattern = google_drive_project_name, type = "folder", n_max = 1)` uses the `googledrive` package to find a folder in Google Drive that matches the given name pattern:

  - `pattern = google_drive_project_name` specifies the name pattern to search for.

  - `type = "folder"` limits the search to folders only.

  - `n_max = 1` restricts the search to return only one result.

- The result of `drive_find` is stored in the variable `google_drive_project_path_info`.

- `return(google_drive_project_path_info)` returns the information about the found folder.

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
