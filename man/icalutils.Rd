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

    character. Applies only if return.class is
    data.frame. This is used for local time: timestamps
    that have no timezone information attached.
    Default is \code{""}, which means to use the
    current timezone.

  }
  \item{\dots}{
    additional arguments
}
}
\details{
  
  An icalendar file consist of one or more
  calendars, each of which has one or more so-called
  components such as events or journal entries. The
  components in turn are composed of properties, such
  as \sQuote{summary}. A property may parameters: a
  \sQuote{summary}, for instance, may a parameter that
  tells its \sQuote{language}.

  \code{read_icalendar} reads a file in icalendar
  format. It returns a list, in which each entry
  contains one calendar component.  \code{read_vevent}
  and \code{read_vtimezone} work the same, but only
  return events and timezones.

  For \code{read_vevent}, the argument
  \code{return.class} may be set to \code{data.frame},
  which will result in a selection of properties of the
  events; see Value section below.

  \subsection{Timezones}{

    Timezone names, which include specific names when
    daylight-saving time is in effect, are not portable
    and may vary between systems. Thus, the icalender
    specification requires that timezone information
    (i.e. a mapping from a local time to an GMT offset)
    must be included in the file.

    In the current implementation, this information is
    not used. Instead, \pkg{icalutils} uses \R{}'s
    (very good) timezone support (see
    \code{\link{timezones}}) and maps datetimes with a
    timezone attribute to the timezone with the same
    name in the Olsen database. This clean and
    transparent approach will not work on Windows (of
    course not). Thus, Windows timezone names (such as
    the infamous \emph{Romance time} for Paris) are
    mapped to Olsen names via data provided by the
    Unicode consortium.    
    
  }
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
