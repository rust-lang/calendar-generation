use chrono::NaiveDate;
use serde::Deserialize;
use std::{
    fmt,
    path::{Path, PathBuf},
};

// Current name of toml-to-ical.
const NAME: &str = env!("CARGO_PKG_NAME");
// Current version of toml-to-ical.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// RFC 5545 §3.1 specifies that folding must occur at 75 octets.
const FOLD_THRESHOLD_OCTETS: usize = 75;

// Delete alongwith `floor_char_boundary` when appropriate.
fn is_utf8_char_boundary(c: &u8) -> bool {
    // This is bit magic equivalent to: b < 128 || b >= 192
    (*c as i8) >= -0x40
}

/// Use `str::floor_char_boundary` when `round_char_boundary` feature is stable.
fn floor_char_boundary(s: &str, index: usize) -> usize {
    if index >= s.len() {
        s.len()
    } else {
        let lower_bound = index.saturating_sub(3);
        let new_index =
            s.as_bytes()[lower_bound..=index].iter().rposition(|b| is_utf8_char_boundary(b));

        // SAFETY: we know that the character boundary will be within four bytes
        unsafe { lower_bound + new_index.unwrap_unchecked() }
    }
}

/// Fold content lines according to RFC 5545 §3.1.
fn fold(s: String) -> String {
    let s = s.replace('\n', "\\n");

    if s.as_bytes().len() < FOLD_THRESHOLD_OCTETS {
        return s;
    }

    let orig = s.as_bytes();
    let mut ret: Vec<u8> = Vec::new();

    let mut old_idx = 0;
    let mut idx = FOLD_THRESHOLD_OCTETS;
    loop {
        idx = floor_char_boundary(&s, idx);
        ret.extend_from_slice(orig.get(old_idx..idx).expect("idx greater than len"));

        if idx == s.len() {
            break;
        }

        ret.extend("\n\t".as_bytes());

        old_idx = idx;
        idx += FOLD_THRESHOLD_OCTETS;
        idx -= "\n\t".len();
    }

    String::from_utf8(ret).expect("invalid folding")
}

macro_rules! folded_writeln {
    ($dst:expr $(,)?) => {
        std::write!($dst, "\n")
    };
    ($dst:expr, $($arg:tt)*) => {
        std::write!($dst, "{}\n", &$crate::fold(std::format!($($arg)*)))
    };
}

trait DateTimeExt {
    /// Generates an iCalendar date-time string format with the prefix symbols.
    /// e.g. `:19970714T173000Z` or `;TZID=America/New_York:19970714T133000`
    ///
    /// ref: <https://tools.ietf.org/html/rfc5545#section-3.3.5>
    fn to_ical_format(&self) -> String;
}

impl DateTimeExt for UtcDateTime {
    fn to_ical_format(&self) -> String {
        self.format("%Y%m%dT%H%M%SZ").to_string()
    }
}

impl DateTimeExt for NaiveDate {
    fn to_ical_format(&self) -> String {
        self.format("%Y%m%d").to_string()
    }
}

/// `chrono::DateTime` fixed to UTC.
type UtcDateTime = chrono::DateTime<chrono::Utc>;

pub trait External {
    type Error;

    fn from_path(path: &Path) -> Result<Calendar, Self::Error>;
}

/// A calendar.
#[derive(Deserialize)]
pub struct Calendar {
    /// Name of the calendar.
    name: String,
    /// Description of the calendar.
    description: String,

    /// Meta configuration (e.g. include other calendar descriptions).
    meta: Option<Meta>,
    /// List of events.
    #[serde(default)]
    events: Vec<Event>,
}

impl Calendar {
    pub fn load<E: External>(path: &Path) -> Result<Self, <E as External>::Error> {
        let mut root = E::from_path(path)?;
        if let Some(meta) = &root.meta {
            for include in &meta.includes {
                let mut child = Calendar::load::<E>(include)?;
                root.events.extend(child.events.drain(..));
            }
        }
        Ok(root)
    }
}

impl fmt::Display for Calendar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "BEGIN:VCALENDAR")?;
        folded_writeln!(f, "VERSION:2.0")?;
        folded_writeln!(f, "PRODID:-//{NAME}//{VERSION}//EN")?;
        folded_writeln!(f, "CALSCALE:GREGORIAN")?;
        folded_writeln!(f, "X-WR-CALNAME:{}", self.name)?;
        folded_writeln!(f, "X-WR-CALDESC:{}", self.description)?;
        for event in &self.events {
            event.fmt(f)?;
        }
        folded_writeln!(f, "END:VCALENDAR")
    }
}

/// Meta configuration.
/// e.g. include other calendar descriptions.
#[derive(Deserialize)]
struct Meta {
    /// Other calendar TOML files that events should be included from.
    #[serde(default)]
    includes: Vec<PathBuf>,
}

/// Datetime that an event was created.
#[derive(Deserialize)]
#[serde(transparent)]
struct Uid(String);

impl fmt::Display for Uid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "UID:{}", self.0)
    }
}

/// Datetime that an event was created.
#[derive(Deserialize)]
#[serde(transparent)]
struct CreatedOn(UtcDateTime);

impl fmt::Display for CreatedOn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "CREATED:{}", self.0.to_ical_format())
    }
}

/// Datetime that an event was last modified.
#[derive(Deserialize)]
#[serde(transparent)]
struct LastModified(UtcDateTime);

impl fmt::Display for LastModified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "DTSTAMP:{}", self.0.to_ical_format())?;
        folded_writeln!(f, "LAST-MODIFIED:{}", self.0.to_ical_format())
    }
}

/// Title or short description of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Title(String);

impl fmt::Display for Title {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "SUMMARY:{}", self.0)
    }
}

/// Long description of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Description(String);

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "DESCRIPTION:{}", self.0)
    }
}

/// Datetime that an event will start.
#[derive(Deserialize)]
#[serde(untagged)]
enum Start {
    DateTime(UtcDateTime),
    Date(NaiveDate),
}

impl fmt::Display for Start {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Start::DateTime(t) => folded_writeln!(f, "DTSTART:{}", t.to_ical_format()),
            Start::Date(t) => folded_writeln!(f, "DTSTART;VALUE=DATE:{}", t.to_ical_format()),
        }
    }
}

/// Datetime that an event will end.
#[derive(Deserialize)]
#[serde(untagged)]
enum End {
    DateTime(UtcDateTime),
    Date(NaiveDate),
}

impl fmt::Display for End {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            End::DateTime(t) => folded_writeln!(f, "DTEND:{}", t.to_ical_format()),
            End::Date(t) => folded_writeln!(f, "DTEND;VALUE=DATE:{}", t.to_ical_format()),
        }
    }
}

/// Location of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Location(String);

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "LOCATION:{}", self.0)
    }
}

/// Status of the event.
#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum Status {
    /// Event is not confirmed to be happening yet.
    Tentative,
    /// Event is confirmed to be happening.
    Confirmed,
    /// Event has been cancelled.
    Cancelled,
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(
            f,
            "STATUS:{}",
            match self {
                Status::Tentative => "TENTATIVE",
                Status::Confirmed => "CONFIRMED",
                Status::Cancelled => "CANCELLED",
            }
        )
    }
}

/// Is a subscriber as free or busy during the event?
#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum Transparency {
    // Subscriber is considered busy during the event.
    Opaque,
    // Subscriber is not considered busy during the event.
    Transparent,
}

impl fmt::Display for Transparency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(
            f,
            "TRANSP:{}",
            match self {
                Transparency::Opaque => "OPAQUE",
                Transparency::Transparent => "TRANSPARENT",
            }
        )
    }
}

/// Organizer of an event.
#[derive(Deserialize)]
struct Organizer {
    /// Name of event organiser.
    name: String,
    /// Email address of event organiser.
    email: String,
}

impl fmt::Display for Organizer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "ORGANIZER:CN={};mailto:{}", self.name, self.email)
    }
}

/// Unit of time that recurrence happens on.
#[derive(Clone, Copy, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Frequency {
    /// Every year.
    Yearly,
    /// Every month.
    Monthly,
    /// Every week.
    Weekly,
    /// Every daily.
    Daily,
    /// Every hour.
    Hourly,
    /// Every minute.
    Minutely,
    /// Every second.
    Secondly,
}

impl fmt::Display for Frequency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Frequency::Yearly => write!(f, "YEARLY"),
            Frequency::Monthly => write!(f, "MONTHLY"),
            Frequency::Weekly => write!(f, "WEEKLY"),
            Frequency::Daily => write!(f, "DAILY"),
            Frequency::Hourly => write!(f, "HOURLY"),
            Frequency::Minutely => write!(f, "MINUTELY"),
            Frequency::Secondly => write!(f, "SECONDLY"),
        }
    }
}

#[derive(Clone, Copy, Deserialize)]
struct RecurrenceRule {
    /// Unit of time that recurrence happens on.
    /// e.g. daily, weekly, monthly.
    frequency: Frequency,
    /// How many `frequency` between each event?
    /// e.g. an weekly recurrence with interval of two is every other week.
    interval: Option<u16>,
    /// Number of recurrences.
    count: Option<u32>,
    /// Date after which there will be no more recurrences.
    until: Option<UtcDateTime>,
}

impl fmt::Display for RecurrenceRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut rule = "RRULE:".to_string();
        rule.push_str(&format!("FREQ={};", self.frequency));
        if let Some(interval) = self.interval {
            rule.push_str(&format!("INTERVAL={interval};"));
        }
        if let Some(count) = self.count {
            rule.push_str(&format!("COUNT={count};"));
        }
        if let Some(until) = self.until {
            rule.push_str(&format!("UNTIL={};", until.to_ical_format()));
        }
        write!(f, "{rule}")
    }
}

#[derive(Default, Deserialize)]
#[serde(transparent)]
struct RecurrenceRules(Vec<RecurrenceRule>);

impl fmt::Display for RecurrenceRules {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rule in &self.0 {
            folded_writeln!(f, "{rule}")?;
        }
        Ok(())
    }
}

#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Recurrences(Vec<UtcDateTime>);

impl fmt::Display for Recurrences {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            let recurrence_strs: Vec<_> = self.0.iter().map(|d| d.to_ical_format()).collect();
            folded_writeln!(f, "RDATE:{}", recurrence_strs.join(","))
        } else {
            Ok(())
        }
    }
}

#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Exceptions(Vec<UtcDateTime>);

impl fmt::Display for Exceptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            let exception_strs: Vec<_> = self.0.iter().map(|d| d.to_ical_format()).collect();
            folded_writeln!(f, "EXDATE:{}", exception_strs.join(","))
        } else {
            Ok(())
        }
    }
}

#[derive(Deserialize)]
struct Event {
    /// Globally unique identifier for this event.
    uid: Uid,
    /// Datetime that this event was created.
    created_on: Option<CreatedOn>,
    /// Datetime this event was last modified.
    last_modified_on: LastModified,
    /// Title or short description of this event.
    title: Title,
    /// Long description of this event.
    description: Option<Description>,
    /// Datetime that this event will start. Always in UTC.
    start: Start,
    /// Datetime that this event will end. Always in UTC.
    end: Option<End>,
    /// Location of this event.
    location: Option<Location>,
    /// Status of this event.
    status: Option<Status>,
    /// Is a subscriber is considered busy during the event?
    transparency: Option<Transparency>,
    /// Who is responsible for this event?
    organizer: Option<Organizer>,
    /// List of datetimes that this event should repeat on.
    #[serde(default)]
    recurrences: Recurrences,
    /// Rules for how this event should repeat.
    #[serde(default)]
    recurrence_rules: RecurrenceRules,
    /// List of dates which are exceptions to the recurrence rules.
    #[serde(default)]
    exceptions: Exceptions,
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "BEGIN:VEVENT")?;
        self.uid.fmt(f)?;
        if let Some(created_on) = &self.created_on {
            created_on.fmt(f)?;
        }
        self.last_modified_on.fmt(f)?;
        self.title.fmt(f)?;
        if let Some(description) = &self.description {
            description.fmt(f)?;
        }
        self.start.fmt(f)?;
        if let Some(end) = &self.end {
            end.fmt(f)?;
        }
        if let Some(location) = &self.location {
            location.fmt(f)?;
        }
        if let Some(status) = &self.status {
            status.fmt(f)?;
        }
        if let Some(transparency) = &self.transparency {
            transparency.fmt(f)?;
        }
        if let Some(organizer) = &self.organizer {
            organizer.fmt(f)?;
        }
        self.recurrence_rules.fmt(f)?;
        self.recurrences.fmt(f)?;
        self.exceptions.fmt(f)?;
        folded_writeln!(f, "END:VEVENT")
    }
}

#[cfg(test)]
mod tests {
    use super::fold;

    /// Simple test of folding, check that line break and white space are inserted at 75 octets.
    #[test]
    fn folding_simple() {
        assert_eq!(
            fold("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbb".to_string()),
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\tbbbbbbbbbbbbbbbb"
        );
    }

    /// Advanced test of folding, check that line break and white space are inserted at 75 octets in
    /// the presence of multi-byte characters (ASCII characters 128-255).
    #[test]
    fn folding_multibyte_ascii_char() {
        assert_eq!(
            fold("ããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããããêêêêêêêêêêêêêêê".to_string()),
            "ããããããããããããããããããããããããããããããããããããã\n\tãããããããããããããããããããããããããããããããããããã\n\tãããêêêêêêêêêêêêêêê"
        );
    }

    /// Advanced test of folding, check that line break and white space are inserted at 75 octets in
    /// the presence of multi-byte characters (2-byte characters from UTF-8).
    #[test]
    fn folding_multibyte_utf8_char() {
        assert_eq!(
            fold("ĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳŧŧŧŧŧŧŧŧŧŧŧŧŧŧŧŧ".to_string()),
            "ĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳ\n\tĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳĳ\n\tĳĳĳŧŧŧŧŧŧŧŧŧŧŧŧŧŧŧŧ"
        );
    }

    /// Advanced test of folding, where length of string is an exact multiple of the 75 octet
    /// threshold, so one fold would appear to be enough if the consider additional length added by
    /// the newline + white space isn't considered.
    #[test]
    fn folding_multiple() {
        assert_eq!(
            fold("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyzz".to_string()),
            "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n\tyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy\n\tzz"
        )
    }
}
