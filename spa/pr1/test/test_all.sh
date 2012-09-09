#!/bin/sh

for i in *.in; do echo "testing $i";../Main < $i > /tmp/f; diff /tmp/f ${i%*.in}.sol; done
