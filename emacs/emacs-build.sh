#!/usr/bin/env bash

SRC="/e/repos/emacs"

set -e

cd "$SRC"

git clean -xfd

./autogen.sh
./configure --without-pop --with-sound=no --with-modules \
    --without-dbus \
    --without-compress-install

    # --with-native-compilation \

make NATIVE_FULL_AOT=1 -j$(nproc)
make install
