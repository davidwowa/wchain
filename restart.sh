#!/bin/bash

echo "stop beams"
killall beam.smp
echo "stop erl childs"
killall erl_child_setup
echo "done"

echo "start server"
#export PATH=$PATH:~/.cache/rebar3/bin
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/go/bin/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/go/bin:/Library/TeX/texbin:/Users/David/.cache/rebar3/bin
echo "clean"
rebar3 clean
echo "compile"
rebar3 compile
echo "run"
rebar3 run