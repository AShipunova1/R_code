\name{get_my_used_function_helps}
\alias{get_my_used_function_helps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to get an "auxfunctions" function help as a text
}
\description{
This function retrieves and processes the help text for a given function from the "auxfunctions" package, extracts the description and details sections, and formats them as commented text.
}
\usage{
get_my_used_function_helps(function_name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{function_name}{As a string (in quotes)}
}
\details{
- `get_my_used_function_helps <- function(function_name) { ... }` defines a function `get_my_used_function_helps` that takes a function name as input.

- `# browser()` is a commented-out line that can be uncommented for debugging purposes using the `browser()` function.

- `used_tags <- c("description", "details")` defines the tags to look for in the help file.

- `help_text <- help(function_name, "auxfunctions") |> utils:::.getHelpFile()` retrieves the help file for the given function from the "auxfunctions" package and processes it using `utils:::.getHelpFile()`.

- `used_tags_help_list <- map(used_tags, \(one_tag) { ... }) |> setNames(used_tags)` processes the help file to extract sections based on the defined tags:

  - `map(used_tags, \(one_tag) { ... })` applies a function to each tag in `used_tags`.

  - `help_text |> purrr::keep(~ attr(.x, "Rd_tag") == paste0("\\", one_tag))` keeps elements in the help file that match the current tag.

  - `purrr::map(as.character)` converts the matched elements to character vectors.

  - `purrr::flatten_chr()` flattens the list of character vectors into a single character vector.

  - `paste0(collapse = "")` concatenates the elements into a single string.

  - `setNames(used_tags)` assigns names to the list elements based on `used_tags`.

- `used_tags_help <- paste(used_tags_help_list[[1]], "\n", used_tags_help_list[[2]])` combines the extracted sections into a single string with a newline separator.

- `used_tags_help_commented <- used_tags_help |> str_replace_all("\n", "\n# ")` adds comment symbols to each line of the help text:

  - `str_replace_all("\n", "\n# ")` replaces all newline characters with newline characters followed by a comment symbol.

- `return(used_tags_help_commented)` returns the commented help text.


}
\value{
A string
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
