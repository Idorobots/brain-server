#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname mochihttp_dev \
    -s mochihttp \
    -s reloader
