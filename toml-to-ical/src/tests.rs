use serde::Deserialize;

use crate::{
    fold, ByDay, ByHour, ByMinute, ByMonth, ByMonthDay, BySecond, ByWeekNo, ByYearDay, Calendar,
    Count, End, Event, RecurrenceId, RecurrenceRule, RecurrenceRules, Start, TimezoneOffset, Uid,
    ValidationError, Weekday, WeekdayNum,
};

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

// Check that duplicate UIDs are rejected.
#[test]
fn validation_duplicate_uid() {
    let cal = Calendar {
        events: vec![
            Event {
                uid: Uid("1".to_string()),
                end: Some(Default::default()),
                ..Default::default()
            },
            Event { uid: Uid("1".to_string()), ..Default::default() },
        ],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::DuplicateUid("1".to_string()));
}

// Check that a zero duration event is rejected.
#[test]
fn validation_zero_duration_event() {
    // Only need to use `Default::default` here because that will produce a default date for
    // `start` and a `None` for `end`.
    let cal = Calendar { events: vec![Event { ..Default::default() }], ..Default::default() };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ZeroDurationEvent("".to_string()));
}

// Check that mismatched start/end types are rejected.
#[test]
fn validation_mismatched_start_end_types() {
    let cal = Calendar {
        events: vec![Event {
            start: Start::DateTime(Default::default()),
            end: Some(End::Date(Default::default())),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::MismatchedDateTypes("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            start: Start::Date(Default::default()),
            end: Some(End::DateTime(Default::default())),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::MismatchedDateTypes("".to_string()));
}

// Check that mutually exclusive fields in recurrence rules are rejected.
#[test]
fn validation_mutually_exclusive_count_until() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                count: Some(Count(2)),
                until: Some(Default::default()),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::CountUntilMutuallyExclusive("".to_string())
    );
}

// Check that invalid `by_second` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_second() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_second: Some(BySecond(vec![61])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::BySecondOutOfRange("".to_string()));
}

// Check that invalid `by_minute` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_minute() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_minute: Some(ByMinute(vec![60])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByMinuteOutOfRange("".to_string()));
}

// Check that invalid `by_hour` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_hour() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_hour: Some(ByHour(vec![24])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByHourOutOfRange("".to_string()));
}

// Check that invalid `by_day` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_day() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_day: Some(ByDay(vec![WeekdayNum { day: Weekday::Monday, num: Some(0) }])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByDayOutOfRange("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_day: Some(ByDay(vec![WeekdayNum { day: Weekday::Monday, num: Some(54) }])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByDayOutOfRange("".to_string()));
}

// Check that invalid `by_month_day` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_month_day() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_month_day: Some(ByMonthDay(vec![0])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByMonthDayOutOfRange("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_month_day: Some(ByMonthDay(vec![32])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByMonthDayOutOfRange("".to_string()));
}

// Check that invalid `by_year_day` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_year_day() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_year_day: Some(ByYearDay(vec![0])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByYearDayOutOfRange("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_year_day: Some(ByYearDay(vec![367])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByYearDayOutOfRange("".to_string()));
}

// Check that invalid `by_week_no` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_week_no() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_week_no: Some(ByWeekNo(vec![0])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByWeekNoOutOfRange("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_week_no: Some(ByWeekNo(vec![54])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByWeekNoOutOfRange("".to_string()));
}

// Check that invalid `by_week_no` in recurrence rules are rejected.
#[test]
fn validation_invalid_by_month() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_month: Some(ByMonth(vec![0])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByMonthOutOfRange("".to_string()));

    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_rules: RecurrenceRules(vec![RecurrenceRule {
                by_month: Some(ByMonth(vec![13])),
                ..Default::default()
            }]),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(cal.validate().unwrap_err(), ValidationError::ByMonthOutOfRange("".to_string()));
}

#[test]
fn parse_tz_offset() {
    #[derive(Deserialize)]
    struct Harness {
        offset: TimezoneOffset,
    }

    // Valid, negative
    assert_eq!(
        toml::from_str::<Harness>("offset = \"-1000\"").unwrap().offset,
        TimezoneOffset { neg: true, hour: 10, minute: 00 }
    );
    // Valid, positive
    assert_eq!(
        toml::from_str::<Harness>("offset = \"+1323\"").unwrap().offset,
        TimezoneOffset { neg: false, hour: 13, minute: 23 }
    );
    assert_eq!(
        toml::from_str::<Harness>("offset = \"+0000\"").unwrap().offset,
        TimezoneOffset { neg: false, hour: 00, minute: 00 }
    );
    // Invalid, no sign
    assert!(toml::from_str::<Harness>("offset = \"1323\"").is_err());
    // Invalid, zeroes can't be negative
    assert!(toml::from_str::<Harness>("offset = \"-0000\"").is_err());
    // Invalid, out of range
    assert!(toml::from_str::<Harness>("offset = \"+4500\"").is_err());
    assert!(toml::from_str::<Harness>("offset = \"-1078\"").is_err());
    // Invalid, wrong length
    assert!(toml::from_str::<Harness>("offset = \"+450000\"").is_err());
    assert!(toml::from_str::<Harness>("offset = \"-10\"").is_err());
}

// Check that recurrences must refer to events that exist.
#[test]
fn validation_recurrence_does_not_exist() {
    let cal = Calendar {
        events: vec![Event {
            end: Some(Default::default()),
            recurrence_id: Some(Default::default()),
            ..Default::default()
        }],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::RecurrenceDoesNotExist("".to_string())
    );
}

// Check that specific recurrences do not themselves have recurrence rules.
#[test]
fn validation_recurrence_cannot_have_rules() {
    let cal = Calendar {
        events: vec![
            Event {
                end: Some(Default::default()),
                recurrence_rules: RecurrenceRules(vec![Default::default()]),
                ..Default::default()
            },
            Event {
                end: Some(Default::default()),
                recurrence_id: Some(Default::default()),
                recurrence_rules: RecurrenceRules(vec![Default::default()]),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::RecurrenceCannotHaveRecurrenceRules("".to_string())
    );
}

// Check that mismatched start/recurrence_id types are rejected.
#[test]
fn validation_mismatched_start_recurrence_id_types() {
    let cal = Calendar {
        events: vec![
            Event {
                end: Some(Default::default()),
                recurrence_rules: RecurrenceRules(vec![Default::default()]),
                ..Default::default()
            },
            Event {
                start: Start::DateTime(Default::default()),
                end: Some(End::DateTime(Default::default())),
                recurrence_id: Some(RecurrenceId::Date(Default::default())),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::MismatchedRecurrenceDateTypes("".to_string())
    );

    let cal = Calendar {
        events: vec![
            Event {
                end: Some(Default::default()),
                recurrence_rules: RecurrenceRules(vec![Default::default()]),
                ..Default::default()
            },
            Event {
                start: Start::Date(Default::default()),
                end: Some(End::Date(Default::default())),
                recurrence_id: Some(RecurrenceId::DateTime(Default::default())),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::MismatchedRecurrenceDateTypes("".to_string())
    );

    let cal = Calendar {
        events: vec![
            Event {
                end: Some(Default::default()),
                recurrence_rules: RecurrenceRules(vec![Default::default()]),
                ..Default::default()
            },
            Event {
                start: Start::DateTimeWithTz {
                    date: Default::default(),
                    timezone: "Foo".to_string(),
                },
                end: Some(End::DateTimeWithTz {
                    date: Default::default(),
                    timezone: "Foo".to_string(),
                }),
                recurrence_id: Some(RecurrenceId::DateTimeWithTz {
                    date: Default::default(),
                    timezone: "Bar".to_string(),
                }),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    assert_eq!(
        cal.validate().unwrap_err(),
        ValidationError::MismatchedRecurrenceDateTypes("".to_string())
    );
}
