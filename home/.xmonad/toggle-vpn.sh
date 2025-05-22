#!/bin/bash

/usr/bin/osd-vpn-connect --status > /dev/null

if [ 4 -eq $? ]; then
  /usr/bin/osd-vpn-connect --lite
else
  /usr/bin/osd-vpn-disconnect
  echo "disconnected"
fi
