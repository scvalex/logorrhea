#!/bin/bash

control_c() { echo -en "\n*** Killing all background tasks ***\n"; jobs -p | xargs kill; exit $?; }
wait_all() { trap control_c SIGINT; for job in `jobs -p`; do wait $job; done }


~/node_modules/stylus/bin/stylus --watch &
python -m SimpleHTTPServer &


wait_all
