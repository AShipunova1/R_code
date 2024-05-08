\name{clean_all_csvs}
\alias{clean_all_csvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cleans sn uploaded csv
}
\description{
Cleaning, regularly done for csvs downloaded from FHIER,
usually from inside other functions.

The clean_all_csvs function is defined to clean a list of CSVs (csvs) and has an optional parameter vessel_id_field_name, which specifies the column to trim.

It returns the list of cleaned CSVs, where each CSV has had its headers unified and the vessel ID column (if specified) trimmed for consistency.

}
\usage{
clean_all_csvs(csvs, vessel_id_field_name = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{csvs}{
%%     ~~Describe \code{csvs} here~~
}
  \item{vessel_id_field_name}{
%%     ~~Describe \code{vessel_id_field_name} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
