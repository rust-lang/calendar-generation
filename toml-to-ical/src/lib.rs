use chrono::TimeZone;
use rrule::{RRule, RRuleError, Unvalidated};
use serde::Deserialize;
use std::{
    fmt,
    ops::Deref,
    path::{Path, PathBuf},
};

// Current name of toml-to-ical.
const NAME: &str = env!("CARGO_PKG_NAME");
// Current version of toml-to-ical.
const VERSION: &str = env!("CARGO_PKG_VERSION");

trait DateTimeExt {
    /// Convert from a `UtcDateTime` to a `TzDateTime`.
    fn to_tzdatetime(&self) -> TzDateTime;

    /// Generates an iCalendar date-time string format with the prefix symbols.
    /// e.g. `:19970714T173000Z` or `;TZID=America/New_York:19970714T133000`
    ///
    /// ref: <https://tools.ietf.org/html/rfc5545#section-3.3.5>
    fn to_ical_format(&self) -> String;
}

impl DateTimeExt for UtcDateTime {
    fn to_tzdatetime(&self) -> TzDateTime {
        rrule::Tz::UTC.from_utc_datetime(&self.naive_utc())
    }

    fn to_ical_format(&self) -> String {
        self.format("%Y%m%dT%H%M%SZ").to_string()
    }
}

/// `chrono::DateTime` fixed to UTC.
type UtcDateTime = chrono::DateTime<chrono::Utc>;
/// `chrono::DateTime` using `rrule::Tz`.
type TzDateTime = chrono::DateTime<rrule::Tz>;

pub trait External {
    type Error: From<RRuleError>;

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
    events: Vec<Event>,
}

impl Calendar {
    pub fn load<E: External>(path: &Path) -> Result<Self, <E as External>::Error> {
        let mut root = E::from_path(path)?;
        if let Some(meta) = &root.meta {
            for include in &meta.includes {
                let mut child = E::from_path(include)?;
                root.events.extend(child.events.drain(..));
            }
        }
        root.validate::<E>()?;
        Ok(root)
    }

    fn validate<E: External>(&self) -> Result<(), <E as External>::Error> {
        for event in &self.events {
            event.recurrence.validate(&event.start)?;
        }
        Ok(())
    }
}

impl fmt::Display for Calendar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "BEGIN:VCALENDAR")?;
        writeln!(f, "VERSION:2.0")?;
        writeln!(f, "PRODID:-//{NAME}//toml-to-ical {VERSION}//EN")?;
        writeln!(f, "CALSCALE:GREGORIAN")?;
        writeln!(f, "X-WR-CALNAME:{}", self.name)?;
        writeln!(f, "X-WR-CALDESC:{}", self.description)?;
        for event in &self.events {
            event.fmt(f)?;
        }
        writeln!(f, "END:VCALENDAR")
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
        writeln!(f, "UID:{}", self.0)
    }
}

/// Datetime that an event was created.
#[derive(Deserialize)]
#[serde(transparent)]
struct CreatedOn(UtcDateTime);

impl fmt::Display for CreatedOn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "CREATED:{}", self.0.to_ical_format())
    }
}

/// Datetime that an event was last modified.
#[derive(Deserialize)]
#[serde(transparent)]
struct LastModified(UtcDateTime);

impl fmt::Display for LastModified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DTSTAMP:{}", self.0.to_ical_format())?;
        writeln!(f, "LAST-MODIFIED:{}", self.0.to_ical_format())
    }
}

/// Title or short description of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Title(String);

impl fmt::Display for Title {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "SUMMARY:{}", self.0)
    }
}

/// Long description of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Description(String);

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DESCRIPTION:{}", self.0)
    }
}

/// Datetime that an event will start.
#[derive(Deserialize)]
#[serde(transparent)]
struct Start(UtcDateTime);

impl fmt::Display for Start {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DTSTART:{}", self.0.to_ical_format())
    }
}

impl Deref for Start {
    type Target = UtcDateTime;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Datetime that an event will end.
#[derive(Deserialize)]
#[serde(transparent)]
struct End(UtcDateTime);

impl fmt::Display for End {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "DTEND:{}", self.0.to_ical_format())
    }
}

/// Location of an event.
#[derive(Deserialize)]
#[serde(transparent)]
struct Location(String);

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "LOCATION:{}", self.0)
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
        writeln!(
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
        writeln!(
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
        writeln!(f, "ORGANIZER:CN={};mailto:{}", self.name, self.email)
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

impl Into<rrule::Frequency> for Frequency {
    fn into(self) -> rrule::Frequency {
        match self {
            Frequency::Yearly => rrule::Frequency::Yearly,
            Frequency::Monthly => rrule::Frequency::Monthly,
            Frequency::Weekly => rrule::Frequency::Weekly,
            Frequency::Daily => rrule::Frequency::Daily,
            Frequency::Hourly => rrule::Frequency::Hourly,
            Frequency::Minutely => rrule::Frequency::Minutely,
            Frequency::Secondly => rrule::Frequency::Secondly,
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

impl RecurrenceRule {
    fn validate(&self, start: &UtcDateTime) -> Result<(), RRuleError> {
        Into::<RRule<Unvalidated>>::into(*self).validate(start.to_tzdatetime()).map(|_| ())
    }
}

impl Into<RRule<Unvalidated>> for RecurrenceRule {
    fn into(self) -> RRule<Unvalidated> {
        let mut rule = RRule::new(self.frequency.into());
        if let Some(interval) = self.interval {
            rule = rule.interval(interval);
        }
        if let Some(count) = self.count {
            rule = rule.count(count);
        }
        if let Some(until) = self.until {
            rule = rule.until(until.to_tzdatetime());
        }
        rule
    }
}

#[derive(Default, Deserialize)]
#[serde(transparent)]
struct RecurrenceRules(Vec<RecurrenceRule>);

impl RecurrenceRules {
    fn validate(&self, start: &UtcDateTime) -> Result<(), RRuleError> {
        for rule in &self.0 {
            rule.validate(start)?;
        }

        Ok(())
    }
}

impl fmt::Display for RecurrenceRules {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rule in &self.0 {
            Into::<RRule<Unvalidated>>::into(*rule).fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Exceptions(Vec<UtcDateTime>);

impl fmt::Display for Exceptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.0.is_empty() {
            let exception_strs: Vec<_> = self.0.iter().map(|d| d.to_ical_format()).collect();
            writeln!(f, "EXDATE:{}", exception_strs.join(","))
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
    end: End,
    /// Location of this event.
    location: Option<Location>,
    /// Status of this event.
    status: Option<Status>,
    /// Is a subscriber is considered busy during the event?
    transparency: Option<Transparency>,
    /// Who is responsible for this event?
    organizer: Option<Organizer>,
    /// Should this event repeat, and if so, how?
    #[serde(default)]
    recurrence: RecurrenceRules,
    /// List of dates which are exceptions to the recurrence rules.
    #[serde(default)]
    exceptions: Exceptions,
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "BEGIN:VEVENT")?;
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
        self.end.fmt(f)?;
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
        self.recurrence.fmt(f)?;
        self.exceptions.fmt(f)?;
        writeln!(f, "END:VEVENT")
    }
}
