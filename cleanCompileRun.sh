#!/bin/bash
#export PATH=$PATH:~/.cache/rebar3/bin
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/go/bin/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/local/go/bin:/Library/TeX/texbin:/Users/David/.cache/rebar3/bin
rebar3 clean
rebar3 compile
rebar3 run
