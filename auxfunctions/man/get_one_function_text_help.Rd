\name{get_one_function_text_help}
\alias{get_one_function_text_help}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get a function definition and help documentation as a plain text
}
\description{
This function retrieves and processes the text and help documentation for a given function, replaces function names with their definitions, splits the text into multiple lines, converts it to R Markdown format, and returns the formatted text.

Used to create qmd files.
}
\usage{
get_one_function_text_help(function_name,
           my_split_newline_char = "@@@")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{function_name}{ A function name as a string, in quotes.
  }
  \item{my_split_newline_char}{Optional. Default is "@@@"}
}
\details{

- `get_one_function_text_help <- function(fun_name, my_split_newline_char = "@@@") { ... }` defines a function `get_one_function_text_help` that takes a function name and an optional newline character split parameter as input.

- `my_used_function_names <- c(fun_name)` creates a vector `my_used_function_names` containing the function name.

- `my_used_function_texts <- auxfunctions::get_my_used_function_texts(my_used_function_names)` retrieves the text for all the functions used.

- `my_used_function_helps <- auxfunctions::get_my_used_function_helps(my_used_function_names)` retrieves the help documentation for all the functions used.

- `idx <- length(my_used_function_names)` gets the length of the `my_used_function_names` vector.

- `my_fun_name_to_search <- str_glue("(", my_split_newline_char, ".+{my_used_function_names[[idx]]})")` creates a pattern to search for the function name in the text:

  - `str_glue("(", my_split_newline_char, ".+{my_used_function_names[[idx]]})")` constructs a string with the function name and the split newline character.

- `my_fun_name_w_txt <- purrr::reduce(seq_len(length(my_used_function_names)), \(acc, nxt) auxfunctions::replace_function_with_def(acc, nxt), .init = my_fun_name_to_search)` replaces function names with their definitions in the text:

  - `purrr::reduce(seq_len(length(my_used_function_names)), \(acc, nxt) auxfunctions::replace_function_with_def(acc, nxt), .init = my_fun_name_to_search)` iteratively applies the `replace_function_with_def` function to the text, starting with `my_fun_name_to_search`.

- `my_fun_name_w_txt_split <- auxfunctions::split_one_line_text_back(my_fun_name_w_txt)` splits the single line text back into multiple lines.

- `my_fun_name_w_txt_split_rmd_text <- knitr::spin(text = my_fun_name_w_txt_split, knit = FALSE, format = 'qmd')` converts the split text into R Markdown format using `knitr::spin`:

  - `knitr::spin(text = my_fun_name_w_txt_split, knit = FALSE, format = 'qmd')` processes the text to generate R Markdown format without knitting it.

- `my_fun_name_w_txt_split_rmd_text <- my_fun_name_w_txt_split_rmd_text |> str_replace_all(my_slash_replacement, "\\\\")` changes back to `\s` and `\b` in functions:

  - `str_replace_all(my_slash_replacement, "\\\\")` replaces all occurrences of `my_slash_replacement` with backslashes.

- `cat(my_fun_name_w_txt_split_rmd_text, sep = "\n")` prints the final R Markdown formatted text.

- `return(my_fun_name_w_txt_split_rmd_text)` returns the final R Markdown formatted text.

}
\value{
  the R Markdown formatted text
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
