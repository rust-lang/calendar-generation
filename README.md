# toml-to-ical
`toml-to-ical` is a simple command-line utility for generating [iCalendar][ical] documents from
a list of events in TOML format.

It is intended for use in open source projects which have a public calendar with meetings. Instead
of using Google Calendar or some other calendar service, just generate the calendar from a list of
events in TOML format and generate and host the calendar using something like GitHub Actions/Pages
in CI.

## Generate calendars with GitHub Actions
If you have a repository containing TOML documents defining calendars, then you can create a
GitHub Actions workflow with the following content to generate calendars available via GitHub Pages:

```yaml
name: generate calendars

on:
  push:
    branches: ["master", "main"]

permissions:
  contents: read
  pages: write
  id-token: write

concurrency:
  group: "pages"
  cancel-in-progress: false

jobs:
  generate:
    name: generate
    runs-on: ubuntu-20.04
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4.1.1
      - name: Generate calendars
        uses: rust-lang/calendar-generation@main
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3.0.0
        with:
          path: 'result'
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4.0.2
```

You can configure the calendar generation action with the following inputs:

- `input_directory`
  - Directory containing TOML calendar definitions
  - **default:** `.`
- `output_directory`
  - Directory to output iCalendar documents
  - **default:** `result`

#### Stability
<sup>
<code>toml-to-ical</code> has no stability guarantees.
</sup>

<br>

#### Author and acknowledgements
<sup>
<code>toml-to-ical</code> is authored by <a href="https://davidtw.co">David Wood</a> of <i>Huawei
Technologies Research & Development (UK) Ltd</i>.
</sup>

<br>

#### License
<sup>
Licensed under either of <a href="https://www.apache.org/licenses/LICENSE-2.0">Apache License,
Version 2.0</a> or <a href="https://opensource.org/licenses/MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in
this crate by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without
any additional terms or conditions.
</sub>

<br>

#### Code of conduct
<sup>
When contributing or interacting with this project, we ask abide the
<a href="https://www.rust-lang.org/en-US/conduct.html">Rust Code of Conduct</a> and ask that you do
too.
</sup>


[ical]: https://en.wikipedia.org/wiki/ICalendar
