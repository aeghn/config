#!/usr/bin/env bash
set -eu

export RDIR="$(dirname "$(realpath "$0")")/.."
export DAT_DIR="$RDIR/dats"
export DSL_DIR="$RDIR/dsls"
export OUT_DIR="$RDIR/out"

git_bak() {
  local msg="$1"
  shift

  set +e
  git add "$@"
  git commit -m "BACKUP: $msg"

  set -e
}
