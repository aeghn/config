#!/usr/bin/env bash

SRC="/e/repos/emacs"

set -e

cd "$SRC"

./autogen.sh
./configure --without-pop --with-native-compilation --enable-link-time-optimization --with-dbus --with-sound=no 


make NATIVE_FULL_AOT=1 -j$(nproc)
make install
