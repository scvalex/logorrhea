#!/bin/bash

control_c() { echo -en "\n*** Killing all background tasks ***\n"; jobs -p | xargs kill; exit $?; }
wait_all() { trap control_c SIGINT; for job in `jobs -p`; do wait $job; done }


~/bin/stylus --watch &
python -m SimpleHTTPServer &
# node js/websocket-echo.js

wait_all
