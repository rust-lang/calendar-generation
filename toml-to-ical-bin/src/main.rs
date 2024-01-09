use clap::Parser;
use std::{
    ffi::OsStr,
    fs::{read_to_string, File, OpenOptions},
    io::{self, Error as IoError, Write},
    path::{Path, PathBuf},
};
use thiserror::Error;
use toml::{de::Error as TomlError, from_str};
use toml_to_ical::{Calendar, External, ValidationError};

#[derive(Debug, Error)]
enum Error {
    #[error("Failed to open input calendar description: {0}")]
    OpenInput(#[from] IoError),
    #[error("Failed to deserialize input calendar description: {0}")]
    ParseInput(#[from] TomlError),
    #[error("Failed to validate calendar description: {0}")]
    Validation(#[from] ValidationError),
    #[error("Failed to open output: {0}")]
    OpenOutput(IoError),
    #[error("Failed to write to output: {0}")]
    WriteOutput(IoError),
}

#[derive(Parser)]
#[command(author, version, about)]
struct Opt {
    // Path to the input calendar description.
    input: PathBuf,
    /// Specify path to write the iCalendar to [default: -]
    #[arg(short, long)]
    output: Option<PathBuf>,
}

struct Session;

impl External for Session {
    type Error = Error;

    fn from_path(path: &Path) -> Result<Calendar, Error> {
        let as_str = read_to_string(path)?;
        from_str(&as_str).map_err(Into::into)
    }
}

/// Returns `true` if the file type is a fifo.
#[cfg(not(target_family = "unix"))]
fn is_fifo(_: std::fs::FileType) -> bool {
    false
}

/// Returns `true` if the file type is a fifo.
#[cfg(target_family = "unix")]
fn is_fifo(file_type: std::fs::FileType) -> bool {
    use std::os::unix::fs::FileTypeExt;
    file_type.is_fifo()
}

/// Wrapper around output writer which handles differences between stdout, file and pipe outputs.
pub(crate) enum Output {
    Stdout(io::Stdout),
    File(File),
    Pipe(File),
}

impl Output {
    /// Create a `Output` from the input path (or "-" for stdout).
    pub(crate) fn new(path: &OsStr) -> io::Result<Self> {
        if path == "-" {
            return Ok(Output::Stdout(io::stdout()));
        }

        let file =
            OpenOptions::new().read(true).write(true).create(true).truncate(true).open(path)?;
        if is_fifo(file.metadata()?.file_type()) {
            Ok(Output::File(file))
        } else {
            Ok(Output::Pipe(file))
        }
    }
}

impl io::Write for Output {
    fn flush(&mut self) -> io::Result<()> {
        match self {
            Output::Stdout(stdout) => stdout.flush(),
            Output::Pipe(pipe) => pipe.flush(),
            Output::File(file) => file.flush(),
        }
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Output::Stdout(stdout) => stdout.write(buf),
            Output::Pipe(pipe) => pipe.write(buf),
            Output::File(file) => file.write(buf),
        }
    }
}

fn generate() -> Result<(), Error> {
    let opts = Opt::parse();

    let calendar = Calendar::load::<Session>(&opts.input)?;
    calendar.validate()?;

    let mut output = if let Some(output) = opts.output {
        Output::new(output.as_os_str())
    } else {
        Output::new(OsStr::new("-"))
    }
    .map_err(Error::OpenOutput)?;
    write!(output, "{calendar}").map_err(Error::WriteOutput)?;

    Ok(())
}

fn main() {
    if let Err(e) = generate() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}
