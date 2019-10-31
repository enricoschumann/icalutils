\name{icalutils}
\alias{icalutils}
\alias{icalutils-package}
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
            recur.expand = FALSE,
            recur.until = NULL,
            recur.count = NULL,
            use.OlsonNames = TRUE,
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
    logical. RFC 5545 says that the end of line should be signalled by CRLF.
  }
  \item{adjust.allday}{

    logical.  If \code{TRUE}, the end day is moved one day back.
    If an all-day event ends on a given day,
    its end is stored as the next day in the icalendar
    format.


  }
  \item{return.class}{
    character: currently list and data.frame are supported
  }
  \item{recur.expand}{
    logical
  }
  \item{recur.until}{
    a date or datetime
  }
  \item{recur.count}{
    an integer
  }
  \item{use.OlsonNames}{

    logical. Map timezone names in file to names returned
    by \code{\link{OlsonNames}}?

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
  \code{read_vevent} and \code{read_vtimezone} work the same, but only
  return events and timezones.

  For \code{read_vevent}, the argument \code{return.class} may be set to
  \code{data.frame}, which will result in a selection of properties of
  the events; see Value section below.

}
\value{

  a \code{\link{list}} of lists: one list for every (top-level)
  component of the iCalendar object (i.e. VEVENTs, VTIMEZONEs,
  VJOURNALs, etc.).

  \code{return.class} may be set to \code{\link{data.frame}}, which will
  contain for all VEVENTs

    \item{\code{uid}}{field value}
    \item{\code{summary}}{field value}
    \item{\code{description}}{field value, or \code{\link{NA}}}
    \item{\code{location}}{field value, or \code{\link{NA}}}
    \item{\code{start}}{field value}
    \item{\code{end}}{field value}
    \item{\code{all.day}}{logical}
    \item{\code{recurring}}{logical}


}
\references{
  RFC 5545 \url{https://tools.ietf.org/html/rfc5545}
}
\author{
  Enrico Schumann
}
\seealso{
  packages \pkg{ical} and \pkg{calendar}
}
\examples{
\donttest{
    read_vevent("calendar.ics")
}
}
