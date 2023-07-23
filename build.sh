#!/bin/bash

if command -v cc > /dev/null 2>&1; then
  CC=cc
elif command -v gcc > /dev/null 2>&1; then
  CC=gcc
elif command -v zig > /dev/null 2>&1; then
  CC='zig cc'
fi

$CC -s -Os -Wall -o minilisp minilisp.c
