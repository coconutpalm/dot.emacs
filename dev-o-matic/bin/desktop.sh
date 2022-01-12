#!/usr/bin/env -S bash

# Open a dev desktop using `vncviewer`, which must be on the path
#
# Port forwarding:
#  5900 - Spice
#  5901 - VNC
#  4713 - Pulseaudio
# x5672 - RabbitMQ
#
ssh -f -p 2222 \
  -L  5900:localhost:5900 \
  -L  5901:localhost:5901 \
  -L  4713:localhost:4713 \
  -R  5672:localhost:5672 \
  -R 15672:localhost:15672 \
  localhost sleep 10

vncviewer localhost:1 &
