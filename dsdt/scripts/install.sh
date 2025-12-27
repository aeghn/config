#!/usr/bin/env bash

source "$(realpath "$0" | sed 's|/[^/]*$||g')/init.sh"

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
cd "$OUT_DIR"

mkdir -p kernel/firmware/acpi/

cd "$DSL_DIR"
git_bak "DSL" "$DSL_DIR"
for i in "$@"; do
iasl -tc "$i"
mv "${i%.dsl}.aml" $OUT_DIR/kernel/firmware/acpi/
done

cd "$OUT_DIR"
find kernel | cpio -H newc --create > acpi-patch.img
sudo cp acpi-patch.img /boot
git add "$OUT_DIR"
echo -e "install patch\n\n$(find -type f)" > git-commit.log
cat git-commit.log
git commit -F git-commit.log
rm -f git-commit.log
