\name{compareODM}
\alias{compareODM}
\title{ compareODM }
\description{
compareODM provides an automated comparison of medical forms in ODM format with UMLS-based semantic annotation.\cr
It is intended to support design of medical forms. 
Input: two files in CDISC ODM format (Version 1.3; see examples)\cr
Output: A summary of identical, matching, similar and differing data items.\cr
Items are compared using UMLS annotations.\cr
Data from identical and matching items can be aggregated and analyzed jointly.\cr
Data from similar items need transformation for joint analysis.\cr
}
\usage{ compareODM( ODMfile1="", ODMfile2="" ) }
\arguments{
  \item{ODMfile1}{
   Name of first ODM file for comparison.\cr
   File format: ODM version 1.3, see examples. If omitted, file.choose()-dialog is started
  }
  \item{ODMfile2}{
   Name of second ODM file for comparison.\cr
   File format: ODM version 1.3, see examples. If omitted, file.choose()-dialog is started
  }
}
\references{
   An UMLS-based method to enable automated comparison of medical forms (manuscript submitted)
}
\author{ Martin Dugas (dugas@uni-muenster.de) }
\examples{
   \dontrun{
compareODM(ODMfile1=file.path(path.package("compareODM"),"extdata","compare1.xml"), 
   ODMfile2=file.path(path.package("compareODM"),"extdata","compare2.xml"))
   }
}
\keyword{ ODM }
\keyword{ UMLS }
