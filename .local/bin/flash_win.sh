#!/bin/sh

# Flashes an active window.

transset-df -a -m 0 0.9
sleep .1
transset-df -a -x 1
sleep .1
transset-df -a -m 0 0.9
sleep .1
transset-df -a -x 1
sleep .1

