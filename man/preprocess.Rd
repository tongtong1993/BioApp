\name{preprocess}
\alias{preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This is the first function of BioApp which preprocess the input dataset.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
preprocess(CPM_type, raw_data, class_index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CPM_type}{options for classification or cox regression. Input either "cox" or "classification".
%%     ~~Describe \code{x} here~~
}
  \item{raw_data}{input dataset include label and features}
  \item{class_index}{The column index of the label}
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
## Set output Rdata file.
output_path = "./output.rda"
## The function will relocate the label to the first column.
preprocess("classification", iris, c(5))
[1] "1 :  Species"
[1] "2 :  Sepal.Length"
[1] "3 :  Sepal.Width"
[1] "4 :  Petal.Length"
[1] "5 :  Petal.Width"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
