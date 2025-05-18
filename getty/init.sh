#!/usr/bin/env bash

mkdir -p /etc/systemd/system/getty@tty1.service.d/
cat <<EOF > /etc/systemd/system/getty@tty1.service.d/autologin.conf
[Service]
ExecStart=
ExecStart=-/sbin/agetty -o '-p -f -- \\u' --noclear --autologin chin %I \$TERM
EOF

cat <<EOF > $HOME/.
if [ -z "\$DISPLAY" ] && [ "\$XDG_VTNR" = 1 ]; then
  
fi
EOF
