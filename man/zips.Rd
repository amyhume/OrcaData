\name{zips}
\alias{zips}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ZIP CODE AND LOCATION FUNCTION
}
\description{
This function pulls public US zipcode data and cleans it
}
\usage{
zips()
}
%- maybe also 'usage' for other objects documented here.
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
%%  ~~who you are~~
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
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    zipcodeR::download_zip_data(force = T)
    zip_info <- select(zipcodeR::zip_code_db, zipcode, major_city, 
        state, timezone)
    zip_info$zipcode <- as.integer(zip_info$zipcode)
    zip_info <- mutate(zip_info, final_city = ifelse(zipcode >= 
        10001 & zipcode <= 10282, "New York", ifelse(zipcode >= 
        10301 & zipcode <= 10314, "New York", ifelse(zipcode >= 
        10451 & zipcode <= 10475, "New York", ifelse(zipcode >= 
        11004 & zipcode <= 11109, "New York", ifelse(zipcode >= 
        11351 & zipcode <= 11697, "New York", ifelse(zipcode >= 
        11201 & zipcode <= 11256, "New York", major_city)))))))
    zip_info$location <- str_c(zip_info$final_city, " ", zip_info$state)
    zip_info <- select(zip_info, zipcode, location, timezone)
    zip_info <- data.frame(zip_info)
    return(zip_info)
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
