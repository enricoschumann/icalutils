\name{icalutils}
\alias{as.data.frame.icalendar}
\alias{icalutils-package}
\alias{icalutils}
\alias{read_icalendar}
\alias{rrule}
\alias{save_attachments}
\alias{to_vevent}
\alias{vevent}
\concept{icalendar}
\title{
  Read and Create iCalendar Data
}
\description{
  Read and create iCalendar data
}
\usage{
read_icalendar(file, strict.eol = TRUE, use.OlsonNames = TRUE,
               uid.names = FALSE, keep.source = TRUE,
               components = c("VEVENT", "VTODO", "VJOURNAL",
                              "VFREEBUSY", "VTIMEZONE"),
               ...)

\method{as.data.frame}{icalendar}(x, row.names = NULL, optional = FALSE,
              adjust.allday = TRUE,
              recur.expand = FALSE,
              recur.until = NULL,
              recur.count = NULL,
              use.OlsonNames = TRUE,
              all.timestamps.POSIXct = TRUE,
              all.timestamps.Date = FALSE,
              timestamps.tz = "",
              components = c("VEVENT", "VTODO"),
              \dots)

rrule(dtstart, dtend, freq,
      until = NULL, count = NULL,
      interval = 1,
      bysecond = NULL, byminute = NULL, byhour = NULL,
      byday = NULL, bymonthday = NULL, byyearday = NULL,
      byweekno = NULL, bymonth = NULL, bysetpos = NULL,
      wkst = NULL, rdate = NULL, exdate = NULL, text = NULL)

vevent(dtstart, summary, dtend = NULL,
       all.day = inherits(dtstart, "Date"),
       description = NULL, uid = NULL, categories = NULL,
       ...,
       vcalendar = TRUE, file, fold = TRUE)

to_vevent(x, \dots)

save_attachments(file, out.dir, strict.eol = TRUE)
}
\arguments{

  \item{file}{character. The file to be read
    (\code{read_icalendar}, \code{save_attachments}) or
    written (\code{vevent}).}

  \item{out.dir}{character: the directory where to store
    attachments}

  \item{strict.eol}{logical. \acronym{RFC} 5545 says that
    the end of a line should be signalled by CRLF, and when
    \code{strict.eol} is \code{TRUE}, such line ends are
    required.  (This also means that when long lines are
    unfolded, the function looks for CRLF followed by a
    space character.)  iCalendar files that use a simple LF
    may be read with \code{strict.eol} set to \code{FALSE}.}

  \item{adjust.allday}{logical.  If \code{TRUE}, the end day
    is moved one day back.  If an all-day event ends on a
    given day, its end is stored as the next day in the
    icalendar format.}

  \item{recur.expand}{logical}

  \item{recur.until}{a date or datetime}

  \item{recur.count}{an integer}

  \item{use.OlsonNames}{logical. Map timezone names in file
    to names returned by \code{\link{OlsonNames}}?}

  \item{uid.names}{logical: use \code{UID} values as names
    for the resulting list?  Careful with Outlook:
    \code{UID}s are often insanely long.}

  \item{all.timestamps.POSIXct}{logical. To recover the
    original \code{Date}, use \code{as.Date(..., tz =
    "<tz>")}.}

  \item{all.timestamps.Date}{logical}

  \item{timestamps.tz}{character. Applies only if
    return.class is data.frame. This is used for local time:
    timestamps that have no timezone information attached.
    Default is \code{""}, which means to use the current
    timezone.}

  \item{\dots}{additional arguments}

  \item{x}{a list with class attribute \code{vevent} or, for
    \code{to_event} an object for which a coercion method
    exists}

  \item{row.names}{see \code{\link{as.data.frame}}}

  \item{optional}{see \code{\link{as.data.frame}}}

  \item{dtstart}{\code{\link{Date}} or \code{\link{POSIXct}}}

  \item{dtend}{\code{\link{Date}} or \code{\link{POSIXct}}}

  \item{freq}{a character string: \code{SECONDLY},
    \code{MINUTELY}, \code{HOURLY}, \code{DAILY},
    \code{WEEKLY}, \code{MONTHLY}, or \code{YEARLY} }

  \item{until}{\code{\link{Date}} or \code{\link{POSIXct}} }

  \item{count}{integer }

  \item{interval}{integer. Default is 1.}

  \item{bysecond}{a vector of integers between 0 and 60
    (though 60 will be changed to 59, to comply with the
    POSIX standard) }

  \item{byminute}{a vector of integers }

  \item{byhour}{a vector of integers }

  \item{byday}{The weekdays (Monday, Tuesday, ...) on which
    an event takes place. Takes either a vector of
    two-letter weekday abbreviations (\code{mo}, \code{tu},
    ...), or a list of two vectors.}

  \item{bymonthday}{a vector of integers }

  \item{byyearday}{a vector of integers }

  \item{byweekno}{a vector of integers }

  \item{bymonth}{a vector of integers }

  \item{bysetpos}{a vector of integers }

  \item{wkst}{ character }

  \item{rdate}{ \code{\link{Date}} or \code{\link{POSIXct}} }

  \item{exdate}{ \code{\link{Date}} or \code{\link{POSIXct}} }

  \item{text}{ the text of the RRULE as used in an iCalendar file }

  \item{keep.source}{ logical }

  \item{components}{ a character vector }

  \item{summary}{character}

  \item{description}{character}

  \item{uid}{character. If \code{NULL}, a UID is genereated
    as suggest at \url{https://www.jwz.org/doc/mid.html} .}

  \item{all.day}{logical}

  \item{fold}{logical. If \code{TRUE} (the default), long lines are
    wrapped as described in \acronym{rfc 5545}, section 3.1.}

  \item{vcalendar}{logical. If \code{TRUE}, the VEVENT component is
    wrapped into a VCALENDAR, i.e. is made into an iCalendar object}

  \item{categories}{a character vector of keywords}

}
\details{

  \code{read_icalendar} reads a file and returns an object of class
  \code{icalendar}, which is a list of lists: each of these lists
  comprises one component (e.g. an event) of the icalendar.

  The \link{as.data.frame} method returns a \code{\link{data.frame}} of
  a selection of properties of the events; see Value section below.


  \subsection{Terminology}{

    An iCalendar stream (typically, a file) consist of one or more
    iCalendar objects. Each iCalendar object has some properties (such
    as the version) and one or more components, such as events, TODOs or
    journal entries. (This implies that there cannot be empty iCalendar
    objects.) These components are again composed of properties, such as
    \sQuote{summary}. A property may have parameters: a
    \sQuote{summary}, for instance, may have a parameter that tells its
    \sQuote{language}.

  }

  \subsection{Dates and times}{

    iCalendar objects have three ways to describe timestamps: dates,
    datetimes with timezone information (which includes UTC times) and
    datetimes without timezone information.  The functions in package
    \pkg{icalutils} map dates to \code{\link{Date}} and datetimes to
    \code{\link{POSIXct}}.  For times without timezones,
    \code{\link{POSIXct}} with timezone \code{UTC} is used, and an
    attribute \code{"localtime"} with value \code{TRUE} is attached.

  }

  \subsection{All-day events}{

    The iCalendar specification does not detail how to treat all-day
    events.  Two implementations seem widespread: i) events on a single
    day define only \code{DTSTART}, but no end; ii) DTEND is set to the
    date after the final day of the event. For instance, an event that
    lasts from August, 1 to August, 3 has DTSTART of August, 1, and
    DTEND at August, 4 (as in August, 4, 00:00:00). If
    \code{adjust.allday} is \code{TRUE} (the default), the end will be
    set to August, 3 (i.e. DTEND becomes becomes inclusive).

  }

  \subsection{Timezones}{

    Timezone names, which include specific names when daylight-saving
    time is in effect, are not portable and may vary between
    systems. Thus, the iCalender specification requires that timezone
    information (i.e. a mapping from a local time to a GMT offset) must
    be included in the file, as components of type \code{VTIMEZONE}.

    In the current version of \pkg{icalutils}, information in such
    \code{VTIMEZONE} components is not used. Instead, \pkg{icalutils}
    uses \R{}'s (very good) timezone support (see
    \code{\link{timezones}}) and maps datetimes with a timezone
    parameter to the timezone with the same name in the Olson
    database. This clean and transparent approach will not work on
    Windows (of course not). Thus, Windows timezone names (such as the
    infamous \emph{Romance time} for Paris) are mapped to Olson names
    via data provided by the Unicode consortium. See References.

  }
}
\value{

  For \code{read_icalendar}, a \code{\link{list}} of lists: one
  list for every component of the iCalendar
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

  For the iCalendar standard see \acronym{RFC} 5545
  \url{https://tools.ietf.org/html/rfc5545}.
  Additional \acronym{RFC}s, such as 7986, may become relevant in
  future versions.

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
