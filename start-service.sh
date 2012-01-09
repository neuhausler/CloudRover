#!/bin/sh
PATH=/sbin:/usr/sbin:/bin:/usr/bin:/usr/local/bin:/usr/local/sbin
cd `dirname $0`
exec erl -noshell -detached -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s cloudrover
