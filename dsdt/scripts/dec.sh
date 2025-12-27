#!/usr/bin/env bash

set -x
source "$(realpath "$0" | sed 's|/[^/]*$||g')/init.sh"

if ! [ -d "$DAT_DIR" ]; then
  echo "$DAT_DIR is absent"
  exit 1
fi

mkdir -p "$DSL_DIR"
rm -rf "$DSL_DIR"/*

cd "$DAT_DIR" 

set +e
mv *dsl "$DSL_DIR"
set -e

for i in *.dat; do
  echo ">>>>>>>>>>> dec $i"
  iasl -d "$i"
done

mv *dsl "$DSL_DIR"
