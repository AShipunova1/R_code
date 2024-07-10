\name{current_project_paths}
\alias{current_project_paths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a List of Paths to the Current Project Code, Input and Output directories
}
\description{
Creates a list of paths to the current project code, input, and output directories. By default uses Anna's computer setup. It also creates input and output directories if they do not exist.
}
\usage{
current_project_paths()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of strings for paths.
For example:

   list( 
   
   project_name = current_project_name,
   
   code = current_project_dir_name,
   
   input = curr_proj_input_path,
         
   output = curr_proj_output_path)
 
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
\code{\link{set_work_dir}},
\code{\link{create_dir_if_not}}

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
