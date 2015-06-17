#!/bin/sh

set -e

ssh -L4005:127.0.0.1:4005 $1
