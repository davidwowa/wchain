#!/bin/bash
echo "stop beams"
killall beam.smp
echo "stop erl childs"
killall erl_child_setup
echo "done"
