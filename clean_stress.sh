#! /usr/bin/env bash

rm -rf client*.log
rm server.log

find -maxdepth 1 -type d -name "client*" | xargs rm -rf
