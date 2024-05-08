\name{my_to_sf}
\alias{my_to_sf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dataframe to SF object
}
\description{
This function converts a data frame with longitude and latitude columns into a spatial object (such as a Simple Features object) using the specified CRS. The resulting spatial object retains the original data frame's structure and includes the spatial attributes specified by the longitude and latitude columns.
}
\usage{
my_to_sf(my_df, my_crs = sf::st_crs(sa_shp))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_df}{
The data frame to convert to a spatial object.
}
  \item{my_crs}{
the coordinate reference system to use, with a default value of `sf::st_crs(sa_shp)`
}
}
\details{
1. **Function Definition:**
   - `my_to_sf <- function(my_df, my_crs = sf::st_crs(sa_shp)) \{ ... \}`: Defines a function named `my_to_sf` with two arguments: `my_df` (the data frame to convert to a spatial object) and `my_crs` (the coordinate reference system to use, with a default value of `sf::st_crs(sa_shp)`).

2. **Converting to Spatial Object:**
   - `%>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = my_crs, remove = FALSE)`: Pipes the input data frame `my_df` into the `st_as_sf` function from the `sf` package, converting it to a spatial object.
     - `coords`: Specifies which columns in the data frame represent the longitude and latitude coordinates. Here, it indicates that the columns named "longitude" and "latitude" contain the spatial coordinates.
     - `crs`: Specifies the coordinate reference system (CRS) for the spatial object. This argument defaults to `my_crs`, which is the CRS defined in the function arguments.
     - `remove`: A logical value indicating whether to remove the columns used as coordinates from the resulting spatial object. Here, it's set to `FALSE` to retain these columns in the output.

3. **Return Result:**
   - `%>% return()`: The `%>%` pipe forwards the resulting spatial object to the `return()` function, which returns the spatial object as the output of the `my_to_sf` function.
}
\value{
A spatial object.
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
