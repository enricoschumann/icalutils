\name{ical.utils}
\alias{ical.utils-package}
\alias{ical.utils}
\alias{read_icalendar}
\alias{read_vevent}
\alias{read_vtimezone}
\title{
  Read and Create iCalendar Data
}
\description{
  Read and create iCalendar data
}
\usage{
read_icalendar(file, ...)
read_vevent(file, ...,
            strict.eol = TRUE,
            adjust.allday = TRUE,
            return.class = "list",
            timestamps.POSIXct = TRUE,
            timestamps.Date = FALSE,
            timestamps.tz = "")
read_vtimezone(file, ..., strict.eol = TRUE)
}
\arguments{
  \item{file}{
    character
  }
  \item{strict.eol}{
    logical. RFC 5545 says that the end of line should be signalled by CRLF
  }
  \item{adjust.allday}{

    logical. If an all-day event ends on a given day,
    its end is stored as the next day in the icalendar
    format.
    If \code{TRUE}, the end day is moved one day back.

  }
  \item{return.class}{
-    character: currently list and data.frame are supported
  }
  \item{timestamps.POSIXct}{
    logical. Applies only if return.class is data.frame
  }
  \item{timestamps.Date}{
    logical. Applies only if return.class is data.frame
  }
  \item{timestamps.tz}{
    character. Applies only if return.class is data.frame.
    Default is \code{""}, which means to use the current timezone.
  }
  \item{\dots}{
    additional arguments
}
}
\details{

  \code{read_icalendar} reads a file in ical format. It returns a list,
  in which each entry contains one calendar component.

  Alternatively,  \code{return.class} may be set to \code{data.frame}

  - written in R

  - icalendar supports timestamps with and without
    times, and these are mapped to \R's
    \code{\link{Date}} and \code{\link{POSIXct}},
    respectively

    - handle (expand) recurring events

}
\value{

  a \code{\link{list}}, or a \code{\link{data.frame}} (if
  \code{return.class} is specified)

}
\references{
  RFC 5545
}
\author{
  Enrico Schumann
}
\seealso{
  package \pkg{ical}
}
\examples{
\donttest{
    read_vevent("calendar.ics")
}
}
