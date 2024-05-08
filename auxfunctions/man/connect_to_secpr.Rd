\name{connect_to_secpr}
\alias{connect_to_secpr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connect to "SECPR" Oracle database from R
}
\description{
This function encapsulates the process of connecting to the "SECPR" Oracle database securely by retrieving credentials from the keyring and establishing a connection using the ROracle package. It ensures that sensitive information such as passwords is not exposed in the code, enhancing security.
}
\usage{
con <- connect_to_secpr()
}
%- maybe also 'usage' for other objects documented here.
\details{
This `connect_to_secpr` function establishes a connection to an Oracle database named "SECPR" using credentials stored securely.

1. **Function Definition:**
   - `connect_to_secpr <- function() { ... }`: Defines a function named `connect_to_secpr` with no input parameters.

2. **Retrieve Username:**
   - `my_username <- keyring::key_list("SECPR")[1, 2]`: Retrieves the username associated with the Oracle database "SECPR" from the keyring package. It accesses the first entry (row) and the second column, assuming the username is stored in the second column.

3. **Establish Database Connection:**
   - `con <- dbConnect(...)`:
     - `dbDriver("Oracle")`: Creates a database driver object specifically for Oracle databases.
     - `username = my_username`: Specifies the retrieved username for the database connection.
     - `password = keyring::key_get("SECPR", my_username)`: Retrieves the corresponding password for the username from the keyring package, ensuring secure access to the credentials.
     - `dbname = "SECPR"`: Specifies the name of the Oracle database to connect to, which is "SECPR" in this case.
     - `dbConnect(...)`: Establishes a connection to the Oracle database using the provided driver, username, password, and database name.

4. **Return Statement:**
   - `return(con)`: Returns the established database connection (`con`) as the output of the function.
}
\value{
It returns the established database connection (con), which can be used to interact with the "SECPR" Oracle database.
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
