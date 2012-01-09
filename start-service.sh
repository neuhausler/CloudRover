#!/bin/sh
cd `dirname $0`
exec erl -noshell -detached -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s cloudrover
