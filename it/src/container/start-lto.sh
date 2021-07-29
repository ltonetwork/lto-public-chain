#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $LTO_OPTS
java $LTO_OPTS -jar /opt/lto/lto.jar /opt/lto/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
