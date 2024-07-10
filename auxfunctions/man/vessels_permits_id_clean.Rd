\name{vessels_permits_id_clean}
\alias{vessels_permits_id_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vessel Permit IDs Clean
}
\description{
This function is responsible for renaming specific columns in the input dataframe `my_df` to more descriptive names ("PERMIT_VESSEL_ID" and "VESSEL_VESSEL_ID").
}
\usage{
vessels_permits_id_clean(my_df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
%%     ~~Describe \code{my_df} here~~
}
}
\details{
Here's the breakdown of the `vessels_permits_id_clean` function:

1. **Function Definition:**
   - `vessels_permits_id_clean <- function (my_df)`: Defines a function named `vessels_permits_id_clean` that takes one argument, `my_df`, which is the input dataframe containing vessel permits data as it comes from the Oracle database.

2. **Column Renaming:**
   - `vessels_permits <- rename(rename(my_df, PERMIT_VESSEL_ID = "QCSJ_C000000000300000"), VESSEL_VESSEL_ID = "QCSJ_C000000000300001")`: This line renames two columns in the input dataframe `my_df`.
     - The first `rename` function renames the column named "QCSJ_C000000000300000" to "PERMIT_VESSEL_ID".
     - The second `rename` function (nested within the first) renames the column named "QCSJ_C000000000300001" to "VESSEL_VESSEL_ID".
     - The resulting dataframe with renamed columns is assigned to `vessels_permits`.

3. **Returning Processed Dataframe:**
   - `return(vessels_permits)`: Returns the dataframe `vessels_permits` with the renamed columns.
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
my_df usually comes from join of Oracle database tables with vessels and permits
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
