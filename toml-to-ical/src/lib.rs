use chrono::{NaiveDate, NaiveDateTime};
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer,
};
use std::{
    collections::HashSet,
    fmt,
    path::{Path, PathBuf},
};
use thiserror::Error;

#[cfg(test)]
mod tests;

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

/// `chrono::DateTime` fixed to UTC.
type UtcDateTime = chrono::DateTime<chrono::Utc>;

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

impl DateTimeExt for NaiveDateTime {
    fn to_ical_format(&self) -> String {
        self.format("%Y%m%dT%H%M%S").to_string()
    }
}

impl DateTimeExt for NaiveDate {
    fn to_ical_format(&self) -> String {
        self.format("%Y%m%d").to_string()
    }
}

pub trait External {
    type Error;

    fn load_without_includes(&self, path: &Path) -> Result<Calendar, Self::Error>;
}

#[derive(Debug, Error, Eq, PartialEq)]
pub enum ValidationError {
    #[error("Event {0} has duplicate `uid`")]
    DuplicateUid(String),
    #[error("Event {0} has no duration (`start` is a datetime and `end` is absent)")]
    ZeroDurationEvent(String),
    #[error("Event {0} has different types for `start` and `end`")]
    MismatchedDateTypes(String),
    #[error("Event {0} has recurrence rule which sets mutually exclusive `until` and `count`")]
    CountUntilMutuallyExclusive(String),
    #[error("Event {0} does not exist but a specific recurrence is requested")]
    RecurrenceDoesNotExist(String),
    #[error("Recurrence of event {0} has recurrences or recurrence rules")]
    RecurrenceCannotHaveRecurrenceRules(String),
    #[error("`recurrence_id` of event {0} has different date type than `start` of original event")]
    MismatchedRecurrenceDateTypes(String),
    #[error("Event {0} has recurrence rule with `by_second` out of range [0, 60]")]
    BySecondOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_minute` out of range [0, 59]")]
    ByMinuteOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_hour` out of range [0, 23]")]
    ByHourOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_day` out of range [0, 52]")]
    ByDayOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_month_day` out of range [0, 31]")]
    ByMonthDayOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_year_day` out of range [0, 365]")]
    ByYearDayOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_week_no` out of range [0, 52]")]
    ByWeekNoOutOfRange(String),
    #[error("Event {0} has recurrence rule with `by_month_no` out of range [0, 12]")]
    ByMonthOutOfRange(String),
}

/// A calendar.
#[derive(Default, Deserialize)]
pub struct Calendar {
    /// Name of the calendar.
    name: String,
    /// Description of the calendar.
    description: String,

    /// Timezone definitions.
    #[serde(default)]
    timezones: Vec<Timezone>,
    /// Meta configuration (e.g. include other calendar descriptions).
    meta: Option<Meta>,
    /// List of events.
    #[serde(default)]
    events: Vec<Event>,
}

impl Calendar {
    pub fn load<E: External>(ctx: &E, path: &Path) -> Result<Self, <E as External>::Error> {
        let mut root = ctx.load_without_includes(path)?;
        if let Some(meta) = &root.meta {
            for include in &meta.includes {
                let mut child = Calendar::load(ctx, include)?;
                root.events.extend(child.events.drain(..));
                root.timezones.extend(child.timezones.drain(..));
                root.timezones.sort_by(|t1, t2| t1.name.cmp(&t2.name));
                root.timezones.dedup_by(|t1, t2| t1.name == t2.name);
            }
        }
        Ok(root)
    }

    pub fn validate(&self) -> Result<(), ValidationError> {
        let mut seen_uids = HashSet::new();
        for event in &self.events {
            let uid = event.uid.0.clone();
            if event.recurrence_id.is_none() && !seen_uids.insert(&event.uid) {
                return Err(ValidationError::DuplicateUid(uid));
            }

            if matches!(event.start, Start::DateTime(_)) && event.end.is_none() {
                return Err(ValidationError::ZeroDurationEvent(uid));
            }

            if let Some(end) = &event.end {
                if event.start.is_date() != end.is_date() || event.start.has_tz() != end.has_tz() {
                    return Err(ValidationError::MismatchedDateTypes(uid));
                }
            }

            if let Some(recurrence_id) = &event.recurrence_id {
                if !seen_uids.contains(&event.uid) {
                    return Err(ValidationError::RecurrenceDoesNotExist(uid));
                }

                if !event.recurrence_rules.0.is_empty() || !event.recurrences.0.is_empty() {
                    return Err(ValidationError::RecurrenceCannotHaveRecurrenceRules(uid));
                }

                if event.start.is_date() != recurrence_id.is_date()
                    || event.start.tz() != recurrence_id.tz()
                {
                    return Err(ValidationError::MismatchedRecurrenceDateTypes(uid));
                }
            }

            for rule in &event.recurrence_rules.0 {
                if rule.until.is_some() && rule.count.is_some() {
                    return Err(ValidationError::CountUntilMutuallyExclusive(uid));
                }

                if let Some(by_second) = &rule.by_second {
                    if by_second.0.iter().any(|s| *s > 60) {
                        return Err(ValidationError::BySecondOutOfRange(uid));
                    }
                }

                if let Some(by_minute) = &rule.by_minute {
                    if by_minute.0.iter().any(|s| *s > 59) {
                        return Err(ValidationError::ByMinuteOutOfRange(uid));
                    }
                }

                if let Some(by_hour) = &rule.by_hour {
                    if by_hour.0.iter().any(|s| *s > 23) {
                        return Err(ValidationError::ByHourOutOfRange(uid));
                    }
                }

                if let Some(by_day) = &rule.by_day {
                    if by_day.0.iter().any(|s| s.num < -53 || s.num > 53 || s.num == 0) {
                        return Err(ValidationError::ByDayOutOfRange(uid));
                    }
                }

                if let Some(by_month_day) = &rule.by_month_day {
                    if by_month_day.0.iter().any(|s| *s < -31 || *s > 31 || *s == 0) {
                        return Err(ValidationError::ByMonthDayOutOfRange(uid));
                    }
                }

                if let Some(by_year_day) = &rule.by_year_day {
                    if by_year_day.0.iter().any(|s| *s < -366 || *s > 366 || *s == 0) {
                        return Err(ValidationError::ByYearDayOutOfRange(uid));
                    }
                }

                if let Some(by_week_no) = &rule.by_week_no {
                    if by_week_no.0.iter().any(|s| *s < -53 || *s > 53 || *s == 0) {
                        return Err(ValidationError::ByWeekNoOutOfRange(uid));
                    }
                }

                if let Some(by_month) = &rule.by_month {
                    if by_month.0.iter().any(|s| *s > 12 || *s == 0) {
                        return Err(ValidationError::ByMonthOutOfRange(uid));
                    }
                }
            }
        }

        Ok(())
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
        for timezone in &self.timezones {
            timezone.fmt(f)?;
        }
        for event in &self.events {
            event.fmt(f)?;
        }
        folded_writeln!(f, "END:VCALENDAR")
    }
}

/// Meta configuration.
/// e.g. include other calendar descriptions.
#[derive(Default, Deserialize)]
struct Meta {
    /// Other calendar TOML files that events should be included from.
    #[serde(default)]
    includes: Vec<PathBuf>,
}

/// Complete timezone definition.
#[derive(Default, Deserialize)]
struct Timezone {
    /// Name of the timezone.
    name: String,
    /// Offset without DST.
    standard: TimezoneOffsets,
    /// Offset with DST.
    daylight: TimezoneOffsets,
}

impl fmt::Display for Timezone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "BEGIN:VTIMEZONE")?;
        folded_writeln!(f, "TZID:{}", self.name)?;
        folded_writeln!(f, "X-LIC-LOCATION:{}", self.name)?;
        folded_writeln!(f, "BEGIN:DAYLIGHT")?;
        self.daylight.fmt(f)?;
        folded_writeln!(f, "END:DAYLIGHT")?;
        folded_writeln!(f, "BEGIN:STANDARD")?;
        self.standard.fmt(f)?;
        folded_writeln!(f, "END:STANDARD")?;
        folded_writeln!(f, "END:VTIMEZONE")
    }
}

/// Specific timezone definition.
#[derive(Default, Deserialize)]
struct TimezoneOffsets {
    // Name of the timezone.
    name: String,
    /// When this timezone takes effect.
    start: TimezoneStart,
    /// When this timezone repeats.
    rule: RecurrenceRule,
    /// UTC offset in use when this timezone begins.
    from: TimezoneOffsetFrom,
    /// UTC offset when this timezone is in use.
    to: TimezoneOffsetTo,
}

impl fmt::Display for TimezoneOffsets {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "TZNAME:{}", self.name)?;
        self.from.fmt(f)?;
        self.to.fmt(f)?;
        self.start.fmt(f)?;
        folded_writeln!(f, "{}", self.rule)
    }
}

/// Datetime that timezone starts.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct TimezoneStart(NaiveDateTime);

impl fmt::Display for TimezoneStart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "DTSTART:{}", self.0.to_ical_format())
    }
}

/// UTC offset in use when this timezone begins.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct TimezoneOffsetFrom(TimezoneOffset);

impl fmt::Display for TimezoneOffsetFrom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "TZOFFSETFROM:{}", self.0)
    }
}

/// UTC offset when this timezone is in use.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct TimezoneOffsetTo(TimezoneOffset);

impl fmt::Display for TimezoneOffsetTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "TZOFFSETTO:{}", self.0)
    }
}

/// UTC offset for a timezone.
#[derive(Debug, Default, PartialEq, Eq)]
struct TimezoneOffset {
    /// Adding or subtracting time from UTC.
    neg: bool,
    /// Number of hours difference.
    hour: u8,
    /// Number of minutes difference.
    minute: u8,
}

impl<'de> Deserialize<'de> for TimezoneOffset {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct OffsetVisitor;

        impl<'de> Visitor<'de> for OffsetVisitor {
            type Value = TimezoneOffset;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("must be of format `{-/+}HHMM`")
            }

            fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let err = de::Error::custom("must be of format `{-/+}HHMM`");

                let (neg, remaining) = if value.starts_with('-') {
                    (true, &value[1..])
                } else if value.starts_with('+') {
                    (false, &value[1..])
                } else {
                    return Err(err);
                };

                let (hour, remaining) = if remaining.len() > 2 {
                    let hour = match u8::from_str_radix(&remaining[..2], 10) {
                        Ok(hour) if hour > 23 => return Err(err),
                        Ok(hour) => hour,
                        Err(_) => return Err(err),
                    };
                    (hour, &remaining[2..])
                } else {
                    return Err(err);
                };

                let minute = if remaining.len() == 2 {
                    match u8::from_str_radix(&remaining, 10) {
                        Ok(min) if min > 59 => return Err(err),
                        Ok(min) => min,
                        Err(_) => return Err(err),
                    }
                } else {
                    return Err(err);
                };

                if hour == 0 && minute == 0 {
                    return Err(err);
                }

                Ok(TimezoneOffset { neg, hour, minute })
            }
        }

        deserializer.deserialize_string(OffsetVisitor)
    }
}

impl fmt::Display for TimezoneOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.neg {
            write!(f, "-")?;
        } else {
            write!(f, "+")?;
        }
        write!(f, "{:02}{:02}", self.hour, self.minute)
    }
}

/// Datetime that an event was created.
#[derive(Default, Deserialize, Eq, Hash, PartialEq)]
#[serde(transparent)]
struct Uid(String);

impl fmt::Display for Uid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "UID:{}", self.0)
    }
}

/// Datetime that an event was created.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct CreatedOn(UtcDateTime);

impl fmt::Display for CreatedOn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "CREATED:{}", self.0.to_ical_format())
    }
}

/// Datetime that an event was last modified.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct LastModified(UtcDateTime);

impl fmt::Display for LastModified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "DTSTAMP:{}", self.0.to_ical_format())?;
        folded_writeln!(f, "LAST-MODIFIED:{}", self.0.to_ical_format())
    }
}

/// Title or short description of an event.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Title(String);

impl fmt::Display for Title {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "SUMMARY:{}", self.0)
    }
}

/// Long description of an event.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Description(String);

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "DESCRIPTION:{}", self.0)
    }
}

trait DateProperty {
    fn is_date(&self) -> bool;

    fn is_datetime(&self) -> bool {
        !self.is_date()
    }

    fn tz(&self) -> Option<&str>;

    fn has_tz(&self) -> bool {
        self.tz().is_some()
    }
}

/// Datetime that an event will start.
#[derive(Deserialize)]
#[serde(untagged)]
enum Start {
    DateTimeWithTz { date: NaiveDateTime, timezone: String },
    DateWithTz { date: NaiveDate, timezone: String },
    DateTime(UtcDateTime),
    Date(NaiveDate),
}

impl DateProperty for Start {
    fn is_date(&self) -> bool {
        match *self {
            Start::DateTimeWithTz { .. } | Start::DateTime(_) => false,
            Start::DateWithTz { .. } | Start::Date(_) => true,
        }
    }

    fn tz(&self) -> Option<&str> {
        match self {
            Start::DateTimeWithTz { timezone, .. } | Start::DateWithTz { timezone, .. } => {
                Some(timezone.as_str())
            }
            Start::DateTime(_) | Start::Date(_) => None,
        }
    }
}

impl Default for Start {
    fn default() -> Self {
        Start::DateTime(Default::default())
    }
}

impl fmt::Display for Start {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Start::DateTimeWithTz { date, timezone } => {
                folded_writeln!(f, "DTSTART;TZID={timezone}:{}", date.to_ical_format())
            }
            Start::DateWithTz { date, timezone } => {
                folded_writeln!(f, "DTSTART;TZID={timezone};VALUE=DATE:{}", date.to_ical_format())
            }
            Start::DateTime(date) => folded_writeln!(f, "DTSTART:{}", date.to_ical_format()),
            Start::Date(date) => folded_writeln!(f, "DTSTART;VALUE=DATE:{}", date.to_ical_format()),
        }
    }
}

/// Datetime that an event will end.
#[derive(Deserialize)]
#[serde(untagged)]
enum End {
    DateTimeWithTz { date: NaiveDateTime, timezone: String },
    DateWithTz { date: NaiveDate, timezone: String },
    DateTime(UtcDateTime),
    Date(NaiveDate),
}

impl DateProperty for End {
    fn is_date(&self) -> bool {
        match *self {
            End::DateTimeWithTz { .. } | End::DateTime(_) => false,
            End::DateWithTz { .. } | End::Date(_) => true,
        }
    }

    fn tz(&self) -> Option<&str> {
        match self {
            End::DateTimeWithTz { timezone, .. } | End::DateWithTz { timezone, .. } => {
                Some(timezone.as_str())
            }
            End::DateTime(_) | End::Date(_) => None,
        }
    }
}

impl Default for End {
    fn default() -> Self {
        End::DateTime(Default::default())
    }
}

impl fmt::Display for End {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            End::DateTimeWithTz { date, timezone } => {
                folded_writeln!(f, "DTEND;TZID={timezone}:{}", date.to_ical_format())
            }
            End::DateWithTz { date, timezone } => {
                folded_writeln!(f, "DTEND;TZID={timezone};VALUE=DATE:{}", date.to_ical_format())
            }
            End::DateTime(t) => folded_writeln!(f, "DTEND:{}", t.to_ical_format()),
            End::Date(t) => folded_writeln!(f, "DTEND;VALUE=DATE:{}", t.to_ical_format()),
        }
    }
}

/// Location of an event.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Location(String);

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "LOCATION:{}", self.0)
    }
}

/// Status of the event.
#[derive(Default, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Status {
    /// Event is not confirmed to be happening yet.
    Tentative,
    /// Event is confirmed to be happening.
    #[default]
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
#[derive(Default, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Transparency {
    // Subscriber is considered busy during the event.
    #[default]
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
#[derive(Default, Deserialize)]
struct Organizer {
    /// Name of event organiser.
    name: String,
    /// Email address of event organiser.
    email: String,
}

impl fmt::Display for Organizer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        folded_writeln!(f, "ORGANIZER;CN={}:mailto:{}", self.name, self.email)
    }
}

/// Unit of time that recurrence happens on.
#[derive(Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Frequency {
    /// Every year.
    Yearly,
    /// Every month.
    Monthly,
    /// Every week.
    #[default]
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

/// How many `frequency` between each event?
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Interval(u16);

impl fmt::Display for Interval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "INTERVAL={};", self.0)
    }
}

/// Number of recurrences.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Count(u32);

impl fmt::Display for Count {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "COUNT={};", self.0)
    }
}

/// Date after which there will be no more recurrences.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct Until(UtcDateTime);

impl fmt::Display for Until {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNTIL={};", self.0.to_ical_format())
    }
}

/// Seconds on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct BySecond(Vec<u16>);

impl fmt::Display for BySecond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYSEC={};", as_strs.join(","))
    }
}

/// Minutes on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByMinute(Vec<u16>);

impl fmt::Display for ByMinute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYMINUTE={};", as_strs.join(","))
    }
}

/// Hours on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByHour(Vec<u16>);

impl fmt::Display for ByHour {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYHOUR={};", as_strs.join(","))
    }
}

/// Days of the week.
#[derive(Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Weekday {
    // Monday
    #[default]
    Monday,
    // Tuesday
    Tuesday,
    // Wednesday
    Wednesday,
    // Thursday
    Thursday,
    // Friday
    Friday,
    // Saturday
    Saturday,
    // Sunday
    Sunday,
}

impl fmt::Display for Weekday {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Weekday::Monday => write!(f, "MO"),
            Weekday::Tuesday => write!(f, "TU"),
            Weekday::Wednesday => write!(f, "WE"),
            Weekday::Thursday => write!(f, "TH"),
            Weekday::Friday => write!(f, "FR"),
            Weekday::Saturday => write!(f, "SA"),
            Weekday::Sunday => write!(f, "SU"),
        }
    }
}

/// Day of the week and ordinal number indicating which of those weeksdays.
#[derive(Default, Deserialize)]
struct WeekdayNum {
    day: Weekday,
    num: i16,
}

impl fmt::Display for WeekdayNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.num, self.day)
    }
}

/// Days of the week on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByDay(Vec<WeekdayNum>);

impl fmt::Display for ByDay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYDAY={};", as_strs.join(","))
    }
}

/// Days of the month on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByMonthDay(Vec<i16>);

impl fmt::Display for ByMonthDay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYMONTHDAY={};", as_strs.join(","))
    }
}

/// Days of the year on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByYearDay(Vec<i16>);

impl fmt::Display for ByYearDay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYYEARDAY={};", as_strs.join(","))
    }
}

/// Weeks of the year on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByWeekNo(Vec<i16>);

impl fmt::Display for ByWeekNo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYWEEKNO={};", as_strs.join(","))
    }
}

/// Months on which the event will recur.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct ByMonth(Vec<u8>);

impl fmt::Display for ByMonth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let as_strs: Vec<_> = self.0.iter().map(ToString::to_string).collect();
        write!(f, "BYMONTH={};", as_strs.join(","))
    }
}

/// First day of the week.
#[derive(Default, Deserialize)]
#[serde(transparent)]
struct WeekStart(Weekday);

impl fmt::Display for WeekStart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WKST={};", self.0)
    }
}

#[derive(Default, Deserialize)]
struct RecurrenceRule {
    /// Unit of time that recurrence happens on.
    /// e.g. daily, weekly, monthly.
    frequency: Frequency,
    /// How many `frequency` between each event?
    /// e.g. an weekly recurrence with interval of two is every other week.
    interval: Option<Interval>,
    /// Number of recurrences.
    count: Option<Count>,
    /// Date after which there will be no more recurrences.
    until: Option<UtcDateTime>,
    /// Seconds on which the event will recur.
    by_second: Option<BySecond>,
    /// Minutes on which the event will recur.
    by_minute: Option<ByMinute>,
    /// Hours on which the event will recur.
    by_hour: Option<ByHour>,
    /// Days of the week on which the event will recur.
    by_day: Option<ByDay>,
    /// Days of the month on which the event will recur.
    by_month_day: Option<ByMonthDay>,
    /// Days of the year on which the event will recur.
    by_year_day: Option<ByYearDay>,
    /// Weeks of the year on which the event will recur.
    by_week_no: Option<ByWeekNo>,
    /// Months of the year on which the event will recur.
    by_month: Option<ByMonth>,
    /// First day of the week.
    week_start: Option<WeekStart>,
}

impl fmt::Display for RecurrenceRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RRULE:FREQ={};", self.frequency)?;
        if let Some(interval) = &self.interval {
            write!(f, "{interval}")?;
        }
        if let Some(count) = &self.count {
            write!(f, "{count}")?;
        }
        if let Some(until) = &self.until {
            write!(f, "{until}")?;
        }
        if let Some(by_second) = &self.by_second {
            write!(f, "{by_second}")?;
        }
        if let Some(by_minute) = &self.by_minute {
            write!(f, "{by_minute}")?;
        }
        if let Some(by_hour) = &self.by_hour {
            write!(f, "{by_hour}")?;
        }
        if let Some(by_day) = &self.by_day {
            write!(f, "{by_day}")?;
        }
        if let Some(by_month_day) = &self.by_month_day {
            write!(f, "{by_month_day}")?;
        }
        if let Some(by_year_day) = &self.by_year_day {
            write!(f, "{by_year_day}")?;
        }
        if let Some(by_week_no) = &self.by_week_no {
            write!(f, "{by_week_no}")?;
        }
        if let Some(by_month) = &self.by_month {
            write!(f, "{by_month}")?;
        }
        if let Some(week_start) = &self.week_start {
            write!(f, "{week_start}")?;
        }
        Ok(())
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

/// Identify a specific recurrence by specifying the datetime of that recurrence.
#[derive(Deserialize)]
#[serde(untagged)]
enum RecurrenceId {
    DateTimeWithTz { date: NaiveDateTime, timezone: String },
    DateWithTz { date: NaiveDate, timezone: String },
    DateTime(UtcDateTime),
    Date(NaiveDate),
}

impl DateProperty for RecurrenceId {
    fn is_date(&self) -> bool {
        match *self {
            RecurrenceId::DateTimeWithTz { .. } | RecurrenceId::DateTime(_) => false,
            RecurrenceId::DateWithTz { .. } | RecurrenceId::Date(_) => true,
        }
    }

    fn tz(&self) -> Option<&str> {
        match self {
            RecurrenceId::DateTimeWithTz { timezone, .. }
            | RecurrenceId::DateWithTz { timezone, .. } => Some(timezone.as_str()),
            RecurrenceId::DateTime(_) | RecurrenceId::Date(_) => None,
        }
    }
}

impl Default for RecurrenceId {
    fn default() -> Self {
        RecurrenceId::DateTime(Default::default())
    }
}

impl fmt::Display for RecurrenceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecurrenceId::DateTimeWithTz { date, timezone } => {
                folded_writeln!(f, "RECURRENCE-ID;TZID={timezone}:{}", date.to_ical_format())
            }
            RecurrenceId::DateWithTz { date, timezone } => {
                folded_writeln!(
                    f,
                    "RECURRENCE-ID;TZID={timezone};VALUE=DATE:{}",
                    date.to_ical_format()
                )
            }
            RecurrenceId::DateTime(date) => {
                folded_writeln!(f, "RECURRENCE-ID:{}", date.to_ical_format())
            }
            RecurrenceId::Date(date) => {
                folded_writeln!(f, "RECURRENCE-ID;VALUE=DATE:{}", date.to_ical_format())
            }
        }
    }
}

#[derive(Default, Deserialize)]
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
    /// Identify a specific recurrence by specifying the datetime of that recurrence.
    recurrence_id: Option<RecurrenceId>,
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
        if let Some(recurrence_id) = &self.recurrence_id {
            recurrence_id.fmt(f)?;
        }
        folded_writeln!(f, "END:VEVENT")
    }
}
