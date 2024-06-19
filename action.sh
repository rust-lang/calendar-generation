#! /usr/bin/env bash

set -euxo pipefail

mkdir -p "$2"

shopt -s globstar
for path in "$1"/**/*.toml; do
  filepath=${path##$1/}
  mkdir -p $2/$(dirname "$filepath")
  toml-to-ical -i $path -o $2/${filepath%.*}.ics
done
