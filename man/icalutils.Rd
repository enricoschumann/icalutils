\name{icalutils}
\alias{icalutils}
\alias{icalutils-package}
\alias{read_icalendar}
\alias{read_vevent}
\alias{read_vtimezone}
\alias{as.data.frame.icalutils_vevent}
\alias{rrule}

\title{
  Read and Create iCalendar Data
}
\description{
  Read and create iCalendar data
}
\usage{
read_vevent(file, strict.eol = TRUE,
            use.OlsonNames = TRUE,
            uid.names =  FALSE, ...)
read_vtimezone(file, strict.eol = TRUE, ...)

\method{as.data.frame}{icalutils_vevent}(x, row.names = NULL, optional = FALSE,
              adjust.allday = TRUE,
              recur.expand = FALSE,
              recur.until = NULL,
              recur.count = NULL,
              use.OlsonNames = TRUE,
              all.timestamps.POSIXct = TRUE,
              all.timestamps.Date = FALSE,
              timestamps.tz = "",
              \dots)

rrule(dtstart, dtend, freq,
      until = NULL, count = NULL,
      interval = 1,
      bysecond = NULL, byminute = NULL, byhour = NULL,
      byday = NULL, bymonthday = NULL, byyearday = NULL,
      byweekno = NULL, bymonth = NULL, bysetpos = NULL,
      wkst = NULL, rdate = NULL, exdate = NULL, text = NULL)
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
  \item{uid.names}{
    logical: use \code{UID} values as names for the resulting list?
    Careful on Windows: \code{UID}s are often insanely long.

  }
  \item{all.timestamps.POSIXct}{
    logical. To recover the original \code{Date},
    use \code{as.Date(..., tz = "<tz>")}.
  }
  \item{all.timestamps.Date}{
    logical.
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
  \item{x}{
    a list with class attribute \code{icalutils_vevent}
  }
  \item{row.names}{
    see \code{\link{as.data.frame}}
  }
  \item{optional}{
    see \code{\link{as.data.frame}}
  }

  \item{dtstart}{
    \code{\link{Date}} or \code{\link{POSIXct}}
  }
  \item{dtend}{
    \code{\link{Date}} or \code{\link{POSIXct}}
}
  \item{freq}{

    a character string: \code{SECONDLY}, \code{MINUTELY}, \code{HOURLY},
    \code{DAILY}, \code{WEEKLY}, \code{MONTHLY}, or \code{YEARLY}

  }
  \item{until}{
    \code{\link{Date}} or \code{\link{POSIXct}}
  }
  \item{count}{
    integer
  }
  \item{interval}{
    integer. Default is 1.
  }
  \item{bysecond}{

    a vector of integers between 0 and 60 (though 60 will be changed to
    59, to comply with the POSIX standard)

  }
  \item{byminute}{
    a vector of integers
  }
  \item{byhour}{
    a vector of integers
  }
  \item{byday}{

    The weekdays (Monday, Tuesday, ...) on which an event
    takes place. Takes either a vector of two-letter weekday
    abbreviations (\code{mo}, \code{tu}, ...), or a list of
    two vector.

  }
  \item{bymonthday}{
    a vector of integers
  }
  \item{byyearday}{
    a vector of integers
  }
  \item{byweekno}{
    a vector of integers
  }
  \item{bymonth}{
    a vector of integers
  }
  \item{bysetpos}{
    a vector of integers
  }
  \item{wkst}{
    character
  }
  \item{rdate}{
    \code{\link{Date}} or \code{\link{POSIXct}}
  }
  \item{exdate}{
    \code{\link{Date}} or \code{\link{POSIXct}}
  }
  \item{text}{
    the text of the RRULE as used in an icalendar file
  }
}
\details{

  An icalendar file consist of one or more calendars, each
  of which has one or more so-called components such as
  events or journal entries. The components in turn are
  composed of properties, such as \sQuote{summary}. A
  property may have parameters: a \sQuote{summary}, for
  instance, may have a parameter that tells its
  \sQuote{language}.

  \code{read_vevent} reads a file in icalendar format. It
  returns a list, in which each entry contains one
  event. \code{read_vtimezone} works the same, but returns
  only timezone components.

  For \code{read_vevent}, an \link{as.data.frame} method is
  available, which returns a \code{\link{data.frame}} of a
  selection of properties of the events; see Value section
  below.

  \subsection{Dates and times}{

    icalendar files have three ways to describe timestamps:
    dates, datetimes with timezone information (which
    includes UTC times) and datetimes without timezone
    information.

  }

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

  For \code{read_vevent}, a \code{\link{list}} of lists: one
  list for every VEVENT component of the iCalendar
  object. If coerced to a data.frame, the following columns
  are present. Additional columns may be added later; so
  columns should always be addressed by name, not by position.

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

  For the icalendar standard see \acronym{RFC} 5545
  \url{https://tools.ietf.org/html/rfc5545}

  The mappings from Windows timezone names to Olson
  names are taken from the Unicode Common Locale Data
  Repository (\url{http://cldr.unicode.org/}).

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
rrule(dtstart = as.Date("2019-01-01"),
      dtend =   as.Date("2019-01-05"),
      freq = "yearly", count = 5)
## $text
## [1] "FREQ=yearly;COUNT=5;INTERVAL=1"
##
## $recurrence_set
##      DTSTART      DTEND
## 1 2019-01-01 2019-01-05
## 2 2020-01-01 2020-01-05
## 3 2021-01-01 2021-01-05
## 4 2022-01-01 2022-01-05
## 5 2023-01-01 2023-01-05

}
