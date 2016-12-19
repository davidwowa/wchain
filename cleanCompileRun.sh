#!/bin/bash
# export PATH=$PATH:~/.cache/rebar3/bin
rebar3 clean
rebar3 compile
rebar3 run
