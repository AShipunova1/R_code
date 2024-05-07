\name{pretty_print}
\alias{pretty_print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pretty Print with a Title
}
\description{
This function is designed to print text with a specified title and an optional ending marker. It first prints the title, then the main text, and finally the ending marker.
}
\usage{
pretty_print(my_text, my_title, the_end = "---")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_text}{
%%     ~~Describe \code{my_text} here~~
}
  \item{my_title}{
%%     ~~Describe \code{my_title} here~~
}
  \item{the_end}{
%%     ~~Describe \code{the_end} here~~
}
}
\details{
1. **Function Definition:**
   - `pretty_print <- function(my_text, my_title, the_end = "---") { ... }`: Defines a function named `pretty_print` with three arguments: `my_text`, `my_title`, and `the_end`.

2. **Printing the Title:**
   - `title_message_print(my_title)`: Calls a function `title_message_print` to print the title `my_title`. This function formats and prints the title message.
   
3. **Printing the Text:**
   - `cat(c(my_text, the_end), sep = "\n")`: Concatenates `my_text` and `the_end` into a character vector and prints them using `cat`. Each element is separated by a newline (`\n`). This line prints the main text followed by the ending marker.

4. **Default Argument:**
   - `the_end = "---"`: Defines a default value for the argument `the_end`. If the argument is not provided when calling the function, it defaults to `"---"`. This is used as a marker to denote the end of the printed content.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{title_message_print}}
}
\examples{
df |> 
  count(wt = n) |>
  unlist() |>
  pretty_print("Total cnt in groups")
      
## The function is currently defined as
function (my_text, my_title, the_end = "---") 
{
    title_message_print(my_title)
    cat(c(my_text, the_end), sep = "\n")
  }
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
