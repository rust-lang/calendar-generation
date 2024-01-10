#! /usr/bin/env bash

set -euxo pipefail

mkdir -p $2
for path in $1/*.toml; do
  filename=$(basename $path)
  toml-to-ical $path -o $2/${filename%.*}.ics
done
