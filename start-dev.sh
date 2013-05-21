#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -config brain \
    -pa  ebin edit deps/*/ebin \
    -boot start_sasl \
    -sname brain_dev \
    -s brain \
    -s reloader
