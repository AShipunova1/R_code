\name{add_count_contacts}
\alias{add_count_contacts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count contacts
}
\description{
# The add_count_contacts function is defined to add two new columns ('was_contacted' and 'contact_freq') to the input data frame ('all_data_df_clean') based on the presence of contact dates.

# It returns the 'result_df', which is the input data frame with the added columns indicating whether a vessel was contacted ('was_contacted') and the frequency of contacts ('contact_freq').

# Use for contacts in the setup function before combining with compliant dataframes
}
\usage{
add_count_contacts(all_data_df_clean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{all_data_df_clean}{
%%     ~~Describe \code{all_data_df_clean} here~~
}
}
\details{
Explanations:
- The `add_count_contacts` function takes a dataframe `all_data_df_clean` as input.
- It first finds the column names for "contact date" and "vessel number" using the `find_col_name` function.
- Then, it adds a new column named "was_contacted", which indicates whether a contact was made with each vessel based on the presence of a contact date. If the contact date is missing (`NA`), it assigns "no"; otherwise, it assigns "yes".
- The `add_count` function is then used to count the number of contacts per vessel, distinguishing between vessels that were contacted and those that were not. The result is stored in a new column named "contact_freq".
- Finally, the function returns the dataframe with the added columns.
}
\value{
A dataframe with the added columns.
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
% \examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
