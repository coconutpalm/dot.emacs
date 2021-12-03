#!/usr/bin/env -S bash

# Open a dev desktop using `vncviewer`, which must be on the path
ssh -f -p 2222 -L 5901:localhost:5901 localhost sleep 10
vncviewer localhost:1 &

