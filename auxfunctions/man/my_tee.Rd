\name{my_tee}
\alias{my_tee}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
My implementation of a Tee function
}
\description{
- This function, `my_tee`, is designed to print messages to both the console and a log file.
- The function includes three main operations:
  - Print the message to the console with a title.
  - Create a log file path and write the message to the log file.
}
\usage{
my_tee(my_text, my_title = NA, stat_log_file_path = NA, date_range = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_text}{The text message to print and log.}
  
  \item{my_title}{An optional title for the message. If not provided,  the function will default to `NA`.}
  
  \item{stat_log_file_path}{The file path where the message should be logged. If not provided, a default path is created.}
  
  \item{date_range}{
An optional date range used to customize the log file's name. The default value is `2022`.}
}
\details{
1. The function `my_tee` is defined with four parameters: `my_text`, `my_title`, `stat_log_file_path`, and `date_range`.
#
2. The function initializes a constant `the_end` with the value `"---"`, which is used to mark the end of each message.
#
3. If the `date_range` parameter is not provided, it defaults to `2022`.
#
4. The function prints the title and message to the console. It uses the function `title_message_print` (not defined in the provided code) to print the title. Then, it prints the text of the message followed by the end mark (`the_end`).
#
5. If the `stat_log_file_path` parameter is not provided, the function constructs a default file path using the `Path` and `Outputs` variables (not defined in the provided code) and the current date (`today()`). The file path is constructed with the title, date range, and date as part of the file name.
#
6. Finally, the function writes the title, message, and end mark to the log file at the specified path. The function appends the new message to the file if it already exists.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (my_text, my_title = NA, stat_log_file_path = NA, date_range = NA) 
{
    the_end = "---"
    if (is.na(date_range)) 
        date_range = 2022
    title_message_print(my_title)
    cat(c(my_text, the_end), sep = "\\n")
    if (is.na(stat_log_file_path)) {
        stat_log_file_path <- file.path(Path, Outputs, str_glue("{my_title}_{date_range}_run_{today()}.log"))
    }
    cat(c(my_title, my_text, the_end), file = stat_log_file_path, 
        sep = "\\n", append = TRUE)
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
